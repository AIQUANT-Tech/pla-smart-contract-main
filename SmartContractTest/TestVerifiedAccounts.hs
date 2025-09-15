{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fewer imports" #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Plastik.Test.TestVerifiedAccounts where

import           PlutusTx                    (Data (..), compile)
import qualified PlutusTx
import           PlutusTx.Prelude            hiding (Semigroup(..), unless, (<$>),(<*>))
import           Plutus.V2.Ledger.Api        (BuiltinData, PubKeyHash, Validator,Datum (..), ScriptContext (..),mkValidatorScript, TxOut (..),txOutDatum, OutputDatum (..), txInfoOutputs, unsafeFromBuiltinData, unValidatorScript, PubKeyHash(..), Address(..), Credential(ScriptCredential), ValidatorHash(..))
import           Plutus.V2.Ledger.Contexts   (txSignedBy, scriptContextTxInfo)
import PlutusTx.Prelude (Bool (..), Integer, Maybe (..), traceError, traceIfFalse, (&&), (>), (>=), (<=), (==))
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import qualified PlutusTx.AssocMap           as Map  -- Use PlutusTx AssocMap
import           Prelude                     (Show(..), String, FilePath, IO, putStrLn, print)
import           GHC.Generics                (Generic)
import           PlutusTx.Builtins           (error, emptyByteString)
import           PlutusTx.Builtins.Internal  (BuiltinData, BuiltinString(..))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2, displayError, writeFileTextEnvelope)
import Codec.Serialise (serialise)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import qualified PlutusTx.Builtins as Builtins  -- For creating test PubKeyHash
import qualified Data.ByteString.Char8 as BSC
import qualified Test.Tasty                 as Tasty
import qualified Test.Tasty.HUnit           as HUnit
import qualified Test.Tasty.QuickCheck      as QC
import qualified Plutus.V2.Ledger.Contexts  as V2
import qualified Plutus.V2.Ledger.Tx        as V2
import qualified Plutus.V1.Ledger.Interval  as P.Interval
import qualified Plutus.V1.Ledger.Value     as V1
import qualified Data.ByteString            as BS
import           Data.String                (IsString(..))
import           Test.QuickCheck            (Arbitrary(..), elements, listOf, oneof, suchThat, vectorOf, Property, (===))
import Test.Tasty.QuickCheck
    ( testProperty
    , Arbitrary (..)
    , Positive (..)
    , Property
    , chooseInt
    , elements
    , (===)
    , (==>)
    , Gen
    , (.&&.)
    , property
    , listOf1
    , vectorOf
    , suchThat
    , conjoin
    )

import Control.Applicative (Applicative(..), (<$>), (<*>))
--------------------------------------------------------------------------------
-- On-Chain State (Datum)
--------------------------------------------------------------------------------

-- | The state holds the contract owner, a set of addresses with the validator role (represented as Map keys),
-- and a map of addresses to their verified status.
data VerifiedAccountsState = VerifiedAccountsState
    { owner       :: PubKeyHash
    , validators  :: Map.Map PubKeyHash ()  -- Using Map to represent a set (keys are validators)
    , verifiedMap :: Map.Map PubKeyHash Bool
    }
    deriving (Show, Generic)

PlutusTx.unstableMakeIsData ''VerifiedAccountsState

--------------------------------------------------------------------------------
-- Redeemer Type
--------------------------------------------------------------------------------

data VerifyAction = 
      UpdateVerification PubKeyHash Bool
    | CheckVerification PubKeyHash
    deriving (Show, Generic)

PlutusTx.unstableMakeIsData ''VerifyAction

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

{-# INLINABLE updateVerification #-}
updateVerification :: Map.Map PubKeyHash Bool -> PubKeyHash -> Bool -> Map.Map PubKeyHash Bool
updateVerification m key newVal = Map.insert key newVal m

{-# INLINABLE isVerified #-}
isVerified :: Map.Map PubKeyHash Bool -> PubKeyHash -> Bool
isVerified m key = case Map.lookup key m of
                      Just b  -> b
                      Nothing -> False

{-# INLINABLE getContinuingOutput #-}
getContinuingOutput :: ScriptContext -> TxOut
getContinuingOutput ctx =
    case [ o | o <- txInfoOutputs (scriptContextTxInfo ctx)
             , case txOutDatum o of
                 OutputDatum _ -> True
                 _             -> False
           ] of
      [o] -> o
      _   -> traceError "Expected exactly one continuing output"

--------------------------------------------------------------------------------
-- On-Chain Validator
--------------------------------------------------------------------------------

{-# INLINABLE mkVerifiedValidator #-}
mkVerifiedValidator :: VerifiedAccountsState -> VerifyAction -> ScriptContext -> Bool
mkVerifiedValidator oldState action ctx =
    let info = scriptContextTxInfo ctx
        -- Use BuiltinString directly with overloaded strings
        safeFromJust :: BuiltinString -> Maybe a -> a
        safeFromJust err = maybe (traceError err) id
    in case action of 
        UpdateVerification target newVal ->
            let
                validatorKeys = Map.keys (validators oldState)
                authorized = any (\v -> txSignedBy info v) validatorKeys
                out = getContinuingOutput ctx
                newDatum = case txOutDatum out of
                             OutputDatum (Datum d) -> PlutusTx.fromBuiltinData d
                             _                     -> Nothing
            in
            traceIfFalse "Not authorized" authorized
            && traceIfFalse "Invalid output datum" (isJust newDatum)
            && let newState = safeFromJust "Datum corruption" newDatum in
               traceIfFalse "Owner mismatch" (owner oldState == owner newState)
            && traceIfFalse "Validators mismatch" (validators oldState == validators newState)
            && traceIfFalse "VerifiedMap mismatch" (verifiedMap newState == updateVerification (verifiedMap oldState) target newVal)

        CheckVerification target ->
            let
                out = getContinuingOutput ctx
                newDatum = case txOutDatum out of
                             OutputDatum (Datum d) -> PlutusTx.fromBuiltinData d
                             _                     -> Nothing
            in
            traceIfFalse "Invalid output datum" (isJust newDatum)
            && let newState = safeFromJust "Datum corruption" newDatum in
               traceIfFalse "Owner changed" (owner oldState == owner newState)
            && traceIfFalse "Validators changed" (validators oldState == validators newState)
            && traceIfFalse "VerifiedMap changed" (verifiedMap oldState == verifiedMap newState)
            && traceIfFalse "Not verified" (isVerified (verifiedMap oldState) target)

{-# INLINEABLE wrapValidator #-}
wrapValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapValidator datum redeemer context =
    let state  = unsafeFromBuiltinData datum :: VerifiedAccountsState
        action = unsafeFromBuiltinData redeemer :: VerifyAction
        ctx    = unsafeFromBuiltinData context :: ScriptContext
    in if mkVerifiedValidator state action ctx
          then ()
          else traceError "Verification update failed"

validator :: Validator
validator = mkValidatorScript $$(compile [|| wrapValidator ||])

writePlutusScript :: FilePath -> Validator -> IO ()
writePlutusScript file validator = do
  -- let script = PlutusScriptSerialised . SBS.toShort . LBS.toStrict $ serialise validator

  createDirectoryIfMissing True (takeDirectory file)
  result <-
    writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing $
      PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise $
        Plutus.V2.Ledger.Api.unValidatorScript
          validator
  case result of
    Left err -> print $ displayError err
    Right () -> putStrLn "Successfully wrote Plutus script to file."

writeScript :: IO ()
writeScript = writePlutusScript "Plastik/TestVerifiedAccounts.plutus" validator


------------------------------------------------------------
-- Test Data
------------------------------------------------------------

-- Test Public Key Hashes
alicePKH, bobPKH, charliePKH :: PubKeyHash
alicePKH = PubKeyHash (Builtins.toBuiltin (BSC.pack "Alice"))
bobPKH = PubKeyHash (Builtins.toBuiltin (BSC.pack "Bob"))
charliePKH = PubKeyHash (Builtins.toBuiltin (BSC.pack "Charlie"))
-- validator1PKH = PubKeyHash (Builtins.toBuiltin (BSC.pack "Validator1"))
-- validator2PKH = PubKeyHash (Builtins.toBuiltin (BSC.pack "Validator2"))

initialState :: VerifiedAccountsState
initialState = VerifiedAccountsState
    { owner = alicePKH
    , validators = Map.fromList [(alicePKH, ())]
    , verifiedMap = Map.empty
    }

------------------------------------------------------------
-- Mock Context Creation
------------------------------------------------------------

-- | Creates a mock ScriptContext with specific signatories and outputs
mockContext :: [PubKeyHash] -> [TxOut] -> ScriptContext
mockContext signers outputs =
    let txInfo = V2.TxInfo
            { V2.txInfoInputs = [V2.TxInInfo (V2.TxOutRef "" 0) (V2.TxOut (addressFromValidator validator) mempty (OutputDatum $ Datum $ PlutusTx.toBuiltinData initialState) Nothing)]
            , V2.txInfoOutputs = outputs
            , V2.txInfoFee = mempty
            , V2.txInfoMint = mempty
            , V2.txInfoDCert = []
            , V2.txInfoWdrl = Map.empty
            , V2.txInfoValidRange = P.Interval.always
            , V2.txInfoSignatories = signers
            , V2.txInfoData = Map.empty
            , V2.txInfoId = ""
            }
        purpose = V2.Spending (V2.TxOutRef "" 0)
    in ScriptContext txInfo purpose

-- Simplified address creation
addressFromValidator :: Validator -> Address
addressFromValidator _ = Address (ScriptCredential (ValidatorHash (Builtins.toBuiltin (BSC.pack "mock"))) ) Nothing

------------------------------------------------------------
-- Generators
------------------------------------------------------------

instance Arbitrary VerifiedAccountsState where
    arbitrary = do
        own <- elements [alicePKH, bobPKH, charliePKH]
        validatorsList <- listOf arbitrary `suchThat` (not . null)
        verifiedList <- listOf ((,) <$> arbitrary <*> arbitrary)
        return VerifiedAccountsState
            { owner = own
            , validators = Map.fromList [(v, ()) | v <- validatorsList]
            , verifiedMap = Map.fromList verifiedList
            }


instance Arbitrary VerifyAction where
    arbitrary = oneof
        [ UpdateVerification <$> arbitrary <*> arbitrary
        , CheckVerification <$> arbitrary
        ]

instance Arbitrary PubKeyHash where
    arbitrary = do
        bytes <- vectorOf 28 (QC.choose (0, 255))  -- 28 bytes for Ed25519
        return $ PubKeyHash (Builtins.toBuiltin (BS.pack bytes))

------------------------------------------------------------
-- QuickCheck Properties
------------------------------------------------------------

-- | Authorized validator can update verification status
prop_validUpdate :: Property
prop_validUpdate = QC.forAll (arbitrary `suchThat` (\st -> not (Map.null (validators st)))) $ \st ->
    QC.forAll (QC.elements (Map.keys (validators st))) $ \v ->
        QC.forAll arbitrary $ \target ->
            QC.forAll arbitrary $ \newVal ->
                let action = UpdateVerification target newVal
                    newVerifiedMap = updateVerification (verifiedMap st) target newVal
                    newState = st { verifiedMap = newVerifiedMap }
                    validOutput = [mkOutput newState]
                    ctx = mockContext [v] validOutput
                 in mkVerifiedValidator st action ctx === True

-- | Unauthorized signer cannot update verification
prop_invalidUpdate :: Property
prop_invalidUpdate = QC.forAll arbitrary $ \(st, target, newVal, signer) ->
    not (Map.member signer (validators st)) ==>
        let action = UpdateVerification target newVal
            ctx = mockContext [signer] []
         in mkVerifiedValidator st action ctx === False

-- | Verification check returns correct status
prop_checkVerification :: Property
prop_checkVerification = QC.forAll arbitrary $ \(st, target) ->
    let action = CheckVerification target
        expected = isVerified (verifiedMap st) target
        validOutput = [mkOutput st]  -- State shouldn't change
        ctx = mockContext [] validOutput
     in mkVerifiedValidator st action ctx === expected

-- Helper to create valid output
mkOutput :: VerifiedAccountsState -> TxOut
mkOutput s = TxOut (addressFromValidator validator) mempty 
                (OutputDatum $ Datum $ PlutusTx.toBuiltinData s) Nothing

------------------------------------------------------------
-- Unit Tests
------------------------------------------------------------

unit_ValidUpdate :: HUnit.Assertion
unit_ValidUpdate = do
    let action = UpdateVerification bobPKH True
        newState = initialState { verifiedMap = Map.singleton bobPKH True }
        validOutput = [mkOutput newState]
        ctx = mockContext [alicePKH] validOutput
    HUnit.assertBool "Validator should update status" $ mkVerifiedValidator initialState action ctx


unit_InvalidUpdate :: HUnit.Assertion
unit_InvalidUpdate = do
    let action = UpdateVerification bobPKH True
        newState = initialState { verifiedMap = Map.singleton bobPKH True }
        validOutput = [mkOutput newState]
        ctx = mockContext [bobPKH] validOutput  -- Bob isn't a validator
    HUnit.assertBool "Non-validator shouldn't update" $ not (mkVerifiedValidator initialState action ctx)

unit_CheckVerified :: HUnit.Assertion
unit_CheckVerified = do
    let stateWithVerified = initialState { verifiedMap = Map.singleton bobPKH True }
        action = CheckVerification bobPKH
        ctx = mockContext [] [mkOutput stateWithVerified]
    HUnit.assertBool "Should verify existing" $ mkVerifiedValidator stateWithVerified action ctx

unit_CheckUnverified :: HUnit.Assertion
unit_CheckUnverified = do
    let action = CheckVerification charliePKH
        ctx = mockContext [] [mkOutput initialState]
    HUnit.assertBool "Shouldn't verify unknown" $ not (mkVerifiedValidator initialState action ctx)

------------------------------------------------------------
-- Test Suite
------------------------------------------------------------

tests :: Tasty.TestTree
tests = Tasty.testGroup "Verified Accounts Tests"
    [ Tasty.testGroup "Property Tests"
        [ QC.testProperty "Valid updates succeed" prop_validUpdate
        , QC.testProperty "Invalid updates fail" prop_invalidUpdate
        , QC.testProperty "Verification checks correct" prop_checkVerification
        ]
    , Tasty.testGroup "Unit Tests"
        [ HUnit.testCase "Valid update" unit_ValidUpdate
        , HUnit.testCase "Invalid update" unit_InvalidUpdate
        , HUnit.testCase "Check verified" unit_CheckVerified
        , HUnit.testCase "Check unverified" unit_CheckUnverified
        ]
    ]

main :: IO ()
main = Tasty.defaultMain tests
