{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fewer imports" #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
 
module Plastik.Test.TestPlastikBurner where
 
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2, displayError, writeFileTextEnvelope)
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import GHC.Generics (Generic)
import qualified Plutus.V1.Ledger.Value as Value
import Plutus.V2.Ledger.Api
  ( Address (..),
    Credential (..),
    CurrencySymbol (..),
    Datum (..),
    PubKeyHash(..),
    ScriptContext,
    TokenName (..),
    TxOut (..),
    Validator,
    Value,
    getValue,
    mkValidatorScript,
    unValidatorScript,
    unsafeFromBuiltinData,
  )
import Plutus.V2.Ledger.Contexts (scriptContextTxInfo, txInfoOutputs)
import PlutusTx (Data (..), compile)
import qualified PlutusTx
import PlutusTx.Builtins (divideInteger, equalsInteger)
import PlutusTx.Builtins.Internal (BuiltinByteString, BuiltinData)
import PlutusTx.Prelude hiding (Semigroup (..), check, unless, (<$>),(<*>))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Prelude (FilePath, IO, Show(..), String, print, putStrLn)
import qualified PlutusTx.Builtins as Builtins 
import           PlutusTx.Builtins           (error, emptyByteString)
import           PlutusTx.Builtins.Internal  (BuiltinData, BuiltinString(..))
import qualified Data.ByteString.Char8 as BSC
import qualified PlutusTx.AssocMap           as Map 
import qualified Test.Tasty                 as Tasty
import qualified Test.Tasty.HUnit           as HUnit
import qualified Plutus.V1.Ledger.Interval  as P.Interval
import qualified Test.Tasty.QuickCheck      as QC
import qualified Plutus.V2.Ledger.Contexts  as V2
import qualified Plutus.V2.Ledger.Tx        as V2
import qualified Plutus.V1.Ledger.Interval  as P.Interval
import qualified Plutus.V1.Ledger.Value     as V1
import qualified Data.ByteString            as BS
import Test.QuickCheck.Instances.ByteString ()
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
    , forAll
    )
import Control.Applicative (Applicative(..), (<$>), (<*>))
--------------------------------------------------------------------------------
-- On-Chain State (Datum) for PlastikBurner
--------------------------------------------------------------------------------
 
data BurnerState = BurnerState
  { owner :: PubKeyHash,
    tokenName :: BuiltinByteString,
    tokenSymbol :: BuiltinByteString,
    year :: Integer,
    maxBurnYear :: Integer,
    plasticProduction :: Integer,
    burnPerKg :: Integer
  }
  deriving (Show, Generic)
 
PlutusTx.unstableMakeIsData ''BurnerState
 
--------------------------------------------------------------------------------
-- Redeemer Actions for PlastikBurner
--------------------------------------------------------------------------------
 
data BurnerRedeemer
  = CurrentBurnRate Integer -- pass token balance
  | GetBalance CurrencySymbol TokenName -- pass asset identifiers
  | SetPlasticProduction PubKeyHash Integer -- caller and new production
  | SetMaxBurnYear PubKeyHash Integer -- caller and new max burn
  deriving (Show, Generic)
 
PlutusTx.unstableMakeIsData ''BurnerRedeemer
 
--------------------------------------------------------------------------------
-- On-Chain Validator with inlined helpers
--------------------------------------------------------------------------------
 
{-# INLINEABLE safeDiv #-}
-- Use Plutus built-ins for zero-check and division
safeDiv :: Integer -> Integer -> Integer
safeDiv x y =
  if equalsInteger y 0
    then 0
    else divideInteger x y
 
{-# INLINEABLE currentBurnRate #-}
currentBurnRate :: BurnerState -> Integer -> Integer
currentBurnRate s tokenBalance =
  if tokenBalance < maxBurnYear s
    then burnPerKg s
    else 0
 
{-# INLINEABLE getBalance #-}
getBalance :: Value -> CurrencySymbol -> TokenName -> Integer
getBalance v cs tn = Value.valueOf v cs tn
 
{-# INLINEABLE setPlasticProduction #-}
setPlasticProduction :: BurnerState -> PubKeyHash -> Integer -> Maybe BurnerState
setPlasticProduction s caller newProduction =
  if caller == owner s && newProduction > 0
    then
      Just
        s
          { plasticProduction = newProduction,
            burnPerKg = safeDiv (maxBurnYear s) newProduction
          }
    else Nothing
 
{-# INLINEABLE setMaxBurnYear #-}
setMaxBurnYear :: BurnerState -> PubKeyHash -> Integer -> Maybe BurnerState
setMaxBurnYear s caller newMaxBurn =
  if caller == owner s && newMaxBurn > 0
    then
      Just
        s
          { maxBurnYear = newMaxBurn,
            burnPerKg = safeDiv newMaxBurn (plasticProduction s)
          }
    else Nothing
 
{-# INLINEABLE mkBurnerValidator #-}
mkBurnerValidator :: BurnerState -> BurnerRedeemer -> ScriptContext -> Bool
mkBurnerValidator datum redeemer ctx =
  let info = scriptContextTxInfo ctx
      totalBalance = foldMap txOutValue (txInfoOutputs info)
   in case redeemer of
        CurrentBurnRate tb ->
          let rate = currentBurnRate datum tb
           in rate >= 0
        GetBalance cs tn ->
          let bal = getBalance totalBalance cs tn
           in bal >= 0
        SetPlasticProduction caller newProd ->
          case setPlasticProduction datum caller newProd of
            Just _ -> True
            Nothing -> False
        SetMaxBurnYear caller newMax ->
          case setMaxBurnYear datum caller newMax of
            Just _ -> True
            Nothing -> False
 
--------------------------------------------------------------------------------
-- Wrapping Validator for Plutus
--------------------------------------------------------------------------------
 
{-# INLINEABLE wrapValidator #-}
wrapValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapValidator datumBS redeemerBS contextBS =
  let st = unsafeFromBuiltinData datumBS :: BurnerState
      rd = unsafeFromBuiltinData redeemerBS :: BurnerRedeemer
      ctx = unsafeFromBuiltinData contextBS :: ScriptContext
   in if mkBurnerValidator st rd ctx
        then ()
        else traceError "Plastik burn validation failed"
 
validator :: Validator
validator = mkValidatorScript $$(compile [||wrapValidator||])
 
writePlutusScript :: FilePath -> Validator -> IO ()
writePlutusScript file val = do
  createDirectoryIfMissing True (takeDirectory file)
  result <-
    writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing $
      PlutusScriptSerialised
        . SBS.toShort
        . LBS.toStrict
        . serialise
        $ unValidatorScript val
  case result of
    Left err -> print (displayError err)
    Right () -> putStrLn "Successfully wrote Plutus script to file."
 
writeScript :: IO ()
writeScript = writePlutusScript "Plastik/PlastikBurner.plutus" validator
 
-- main :: IO ()
-- main = writeScript
 

-- Test Public Key Hashes
alicePKH, bobPKH, charliePKH :: PubKeyHash
alicePKH = PubKeyHash (Builtins.toBuiltin (BSC.pack "Alice"))
bobPKH = PubKeyHash (Builtins.toBuiltin (BSC.pack "Bob"))
charliePKH = PubKeyHash (Builtins.toBuiltin (BSC.pack "Charlie"))

-- Arbitrary Instances
instance Arbitrary PubKeyHash where
  arbitrary = PubKeyHash . Builtins.toBuiltin . BSC.pack <$> vectorOf 10 arbitrary

instance Arbitrary CurrencySymbol where
  arbitrary = CurrencySymbol . toBuiltin <$> (arbitrary :: Gen BS.ByteString)

instance Arbitrary TokenName where
  arbitrary = TokenName . toBuiltin <$> (arbitrary :: Gen BS.ByteString)

instance Arbitrary BurnerState where
  arbitrary = do
    own <- arbitrary
    tokenName <- Builtins.toBuiltin . BSC.pack <$> arbitrary
    tokenSymbol <- Builtins.toBuiltin . BSC.pack <$> arbitrary
    year <- getPositive <$> arbitrary
    maxBurnYear <- getPositive <$> arbitrary
    plasticProduction <- getPositive <$> arbitrary
    let burnPerKg = safeDiv maxBurnYear plasticProduction
    return BurnerState 
        { owner = own
        , tokenName = tokenName
        , tokenSymbol = tokenSymbol
        , year = year
        , maxBurnYear = maxBurnYear
        , plasticProduction = plasticProduction
        , burnPerKg = burnPerKg    
        }

instance Arbitrary BurnerRedeemer where
  arbitrary = oneof
    [ CurrentBurnRate <$> arbitrary
    , GetBalance <$> arbitrary <*> arbitrary
    , SetPlasticProduction <$> arbitrary <*> (getPositive <$> arbitrary)
    , SetMaxBurnYear <$> arbitrary <*> (getPositive <$> arbitrary)
    ]

-- Mock Context
mockContext :: [PubKeyHash] -> [V2.TxOut] -> V2.ScriptContext
mockContext signers outputs = V2.ScriptContext txInfo purpose
  where
    txInfo = V2.TxInfo
      { V2.txInfoInputs = []
      , V2.txInfoOutputs = outputs
      , V2.txInfoFee = mempty
      , V2.txInfoMint = mempty
      , V2.txInfoDCert = []
      , V2.txInfoWdrl = Map.empty
      , V2.txInfoValidRange = P.Interval.always
      , V2.txInfoSignatories = signers
      , V2.txInfoData = Map.empty
      , V2.txInfoId = "mockTxId"
      }
    purpose = V2.Spending (V2.TxOutRef "mockTxId" 0)

-- QuickCheck Properties
prop_currentBurnRate_valid :: BurnerState -> Integer -> Property
prop_currentBurnRate_valid s tb =
  let redeemer = CurrentBurnRate tb
      ctx = mockContext [] []
   in mkBurnerValidator s redeemer ctx === True

prop_getBalance_valid :: BurnerState -> CurrencySymbol -> TokenName -> Property
prop_getBalance_valid s cs tn =
  let redeemer = GetBalance cs tn
      ctx = mockContext [] []
   in mkBurnerValidator s redeemer ctx === True

prop_setPlasticProduction_valid :: BurnerState -> Positive Integer -> Property
prop_setPlasticProduction_valid s (Positive newProd) =
  let redeemer = SetPlasticProduction (owner s) newProd
      ctx = mockContext [] []
   in mkBurnerValidator s redeemer ctx === True

prop_setPlasticProduction_invalidCaller :: BurnerState -> Positive Integer -> Property
prop_setPlasticProduction_invalidCaller s (Positive newProd) =
  forAll (arbitrary `suchThat` (/= owner s)) $ \nonOwner ->
    mkBurnerValidator s (SetPlasticProduction nonOwner newProd) (mockContext [] []) === False

prop_setPlasticProduction_invalidProd :: BurnerState -> Property
prop_setPlasticProduction_invalidProd s =
  forAll (arbitrary `suchThat` (<= 0)) $ \newProd ->
    mkBurnerValidator s (SetPlasticProduction (owner s) newProd) (mockContext [] []) === False

prop_setMaxBurnYear_valid :: BurnerState -> Positive Integer -> Property
prop_setMaxBurnYear_valid s (Positive newMax) =
  mkBurnerValidator s (SetMaxBurnYear (owner s) newMax) (mockContext [] []) === True

prop_setMaxBurnYear_invalidCaller :: BurnerState -> Positive Integer -> Property
prop_setMaxBurnYear_invalidCaller s (Positive newMax) =
  forAll (arbitrary `suchThat` (/= owner s)) $ \nonOwner ->
    mkBurnerValidator s (SetMaxBurnYear nonOwner newMax) (mockContext [] []) === False

prop_setMaxBurnYear_invalidMax :: BurnerState -> Property
prop_setMaxBurnYear_invalidMax s =
  forAll (arbitrary `suchThat` (<= 0)) $ \newMax ->
    mkBurnerValidator s (SetMaxBurnYear (owner s) newMax) (mockContext [] []) === False

-- Unit Tests
unit_currentBurnRate_belowMax :: HUnit.Assertion
unit_currentBurnRate_belowMax = do
  let s = BurnerState alicePKH "TOKEN" "TKN" 2023 1000 500 2
      redeemer = CurrentBurnRate 500
  HUnit.assertBool "Should validate when below max" $ mkBurnerValidator s redeemer (mockContext [] [])

unit_currentBurnRate_aboveMax :: HUnit.Assertion
unit_currentBurnRate_aboveMax = do
  let s = BurnerState alicePKH "TOKEN" "TKN" 2023 1000 500 2
      redeemer = CurrentBurnRate 1500
  HUnit.assertBool "Should validate when above max" $ mkBurnerValidator s redeemer (mockContext [] [])

unit_setPlasticProduction_valid :: HUnit.Assertion
unit_setPlasticProduction_valid = do
  let s = BurnerState alicePKH "TOKEN" "TKN" 2023 1000 500 2
      redeemer = SetPlasticProduction alicePKH 600
  HUnit.assertBool "Valid update should succeed" $ mkBurnerValidator s redeemer (mockContext [] [])

unit_setPlasticProduction_invalid :: HUnit.Assertion
unit_setPlasticProduction_invalid = do
  let s = BurnerState alicePKH "TOKEN" "TKN" 2023 1000 500 2
      redeemer = SetPlasticProduction bobPKH 600
  HUnit.assertBool "Invalid caller should fail" $ not (mkBurnerValidator s redeemer (mockContext [] []))

testSuite :: Tasty.TestTree
testSuite = Tasty.testGroup "PlastikBurner Tests"
  [ testProperty "CurrentBurnRate valid" prop_currentBurnRate_valid
  , testProperty "GetBalance valid" prop_getBalance_valid
  , testProperty "SetPlasticProduction valid" prop_setPlasticProduction_valid
  , testProperty "SetPlasticProduction invalid caller" prop_setPlasticProduction_invalidCaller
  , testProperty "SetPlasticProduction invalid production" prop_setPlasticProduction_invalidProd
  , testProperty "SetMaxBurnYear valid" prop_setMaxBurnYear_valid
  , testProperty "SetMaxBurnYear invalid caller" prop_setMaxBurnYear_invalidCaller
  , testProperty "SetMaxBurnYear invalid max" prop_setMaxBurnYear_invalidMax
  , HUnit.testCase "CurrentBurnRate below max" unit_currentBurnRate_belowMax
  , HUnit.testCase "CurrentBurnRate above max" unit_currentBurnRate_aboveMax
  , HUnit.testCase "SetPlasticProduction valid" unit_setPlasticProduction_valid
  , HUnit.testCase "SetPlasticProduction invalid" unit_setPlasticProduction_invalid
  ]

main :: IO ()
main = Tasty.defaultMain testSuite