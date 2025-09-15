{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Plastik.Test.TestPlastikRoleV2 where

import Control.Monad (void)
import qualified Data.ByteString.Char8 as BSC
import Data.List (nub)
import GHC.Generics (Generic)
import Prelude (IO, Show, putStrLn, undefined, String, (<>), (<$>))

import Plutus.V2.Ledger.Api
  ( Address (..)
  , Credential (PubKeyCredential)
  , Datum (..)
  , OutputDatum (..)
  , PubKeyHash (..)
  , ScriptContext (..)
  , TxId (..)
  , TxInfo (..)
  , TxOut (..)
  , TxOutRef (..)
  , Validator
  , mkValidatorScript
  , txOutAddress
  , txOutReferenceScript
  , txOutValue
  , unsafeFromBuiltinData
  , OutputDatum(..)
  )
import Plutus.V2.Ledger.Contexts
  ( ScriptPurpose (..)
  , TxInfo (..)
  , txInfoSignatories
  , txOutDatum
  )
import PlutusTx
  ( BuiltinData
  , compile
  , fromBuiltinData
  , toBuiltinData
  , unstableMakeIsData
  )
import qualified PlutusTx.AssocMap as AssocMap
import PlutusTx.Builtins (BuiltinByteString, toBuiltin)
import PlutusTx.Prelude hiding (Semigroup (..), nub, unless, (<$>))

import Test.QuickCheck
  ( Arbitrary (..)
  , Gen
  , Property
  , elements
  , forAll
  , listOf
  , listOf1
  , oneof
  , quickCheck
  , suchThat
  , (===)
  )
import Test.QuickCheck.Instances ()

-- | Datum storing the list of minters
data RoleDatum = RoleDatum {minters :: [PubKeyHash]}
  deriving (Show)
PlutusTx.unstableMakeIsData ''RoleDatum

-- | Redeemer defining actions
data RoleRedeemer = GrantMinter PubKeyHash | VerifyMinter PubKeyHash
  deriving (Show)
PlutusTx.unstableMakeIsData ''RoleRedeemer

{-# INLINABLE mkPlastikRoleValidator #-}
mkPlastikRoleValidator :: RoleDatum -> RoleRedeemer -> ScriptContext -> Bool
mkPlastikRoleValidator datum redeemer ctx =
    case redeemer of
        -- Granting a new minter role (Admin only)
        GrantMinter newMinter ->
            traceIfFalse "Admin signature missing" adminSigned &&
            traceIfFalse "Updated datum must contain new minter" (updatedDatumCorrect newMinter)
        
        -- Checking if the caller is a minter (for minting NFTs)
        VerifyMinter pk ->
            traceIfFalse "PubKey is not minters" (pk `elem` minters datum)

        _ -> False

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- List of signers in the transaction
    signers :: [PubKeyHash]
    signers = txInfoSignatories info

    -- Admin is assumed to be the first minter in the list
    admin :: Maybe PubKeyHash
    admin = case minters datum of
              []     -> Nothing
              (x:_) -> Just x

    -- Check that the admin signed the transaction
    adminSigned :: Bool
    adminSigned = case admin of
                    Nothing -> False
                    Just a  -> a `elem` signers

    -- This function now takes the newMinter as an argument.
    updatedDatumCorrect :: PubKeyHash -> Bool
    updatedDatumCorrect nm =
        any (\o -> case txOutDatum o of
                    OutputDatum (Datum d) -> case PlutusTx.fromBuiltinData d of
                        Just (RoleDatum updatedMinters) -> nm `elem` updatedMinters
                        _ -> False
                    _ -> False) 
            (txInfoOutputs info)

{-# INLINEABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator d r c =
  if mkPlastikRoleValidator (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData c)
    then ()
    else error ()

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkValidator ||])

------------------------------------------------------------
-- Test Data
------------------------------------------------------------
alicePKH, bobPKH, charliePKH, davePKH, evePKH :: PubKeyHash
alicePKH   = PubKeyHash (toBuiltin (BSC.pack "Alice"))
bobPKH     = PubKeyHash (toBuiltin (BSC.pack "Bob"))
charliePKH = PubKeyHash (toBuiltin (BSC.pack "Charlie"))
davePKH    = PubKeyHash (toBuiltin (BSC.pack "Dave"))
evePKH     = PubKeyHash (toBuiltin (BSC.pack "Eve"))

samplePKHs :: [PubKeyHash]
samplePKHs = [alicePKH, bobPKH, charliePKH, davePKH, evePKH]

------------------------------------------------------------
-- Generators
------------------------------------------------------------
genPubKeyHash :: Gen PubKeyHash
genPubKeyHash = elements samplePKHs

genRoleDatum :: Gen RoleDatum
genRoleDatum = RoleDatum <$> (nub <$> listOf1 genPubKeyHash)

genRoleRedeemer :: Gen RoleRedeemer
genRoleRedeemer =
  oneof
    [ GrantMinter <$> genPubKeyHash
    , VerifyMinter <$> genPubKeyHash
    ]

instance Arbitrary PubKeyHash where arbitrary = genPubKeyHash
instance Arbitrary RoleDatum where arbitrary = genRoleDatum
instance Arbitrary RoleRedeemer where arbitrary = genRoleRedeemer

-- Mock TxInfo and ScriptContext
mockTxInfo :: [PubKeyHash] -> [RoleDatum] -> TxInfo
mockTxInfo signers datums =
  TxInfo
    { txInfoInputs      = []
    , txInfoOutputs     = map mkOutput datums
    , txInfoFee         = mempty
    , txInfoMint        = mempty
    , txInfoDCert       = []
    , txInfoWdrl        = AssocMap.empty
    , txInfoValidRange  = undefined
    , txInfoSignatories = signers
    , txInfoData        = AssocMap.empty
    , txInfoId          = TxId (toBuiltin (BSC.pack ""))
    }
  where
    mkOutput d =
      TxOut
        { txOutAddress         = Address (PubKeyCredential alicePKH) Nothing
        , txOutValue           = mempty
        , txOutDatum           = OutputDatum (Datum $ toBuiltinData d)
        , txOutReferenceScript = Nothing
        }

mockScriptContext :: [PubKeyHash] -> [RoleDatum] -> ScriptContext
mockScriptContext signers datums =
  ScriptContext
    { scriptContextTxInfo   = mockTxInfo signers datums
    , scriptContextPurpose  = Spending (TxOutRef (TxId (toBuiltin (BSC.pack ""))) 0)
    }

------------------------------------------------------------
-- Test Case Generators
------------------------------------------------------------
genGrantMinterValid :: Gen (RoleDatum, RoleRedeemer, ScriptContext)
genGrantMinterValid = do
  rd@(RoleDatum ms) <- genRoleDatum `suchThat` \(RoleDatum xs) -> not (null xs) && length xs < length samplePKHs
  let admin = head ms
  newM <- elements (filter (`notElem` ms) samplePKHs)
  let redeemer     = GrantMinter newM
      updatedDatum = RoleDatum (newM : ms)
      ctx          = mockScriptContext [admin] [updatedDatum]
  return (rd, redeemer, ctx)

genGrantMinterInvalidNoAdminSig :: Gen (RoleDatum, RoleRedeemer, ScriptContext)
genGrantMinterInvalidNoAdminSig = do
  rd@(RoleDatum ms) <- genRoleDatum `suchThat` \(RoleDatum xs) ->
                        not (null xs) && length xs < length samplePKHs
  let nonAdmins = filter (`notElem` ms) samplePKHs
  non <- elements nonAdmins
  return (rd, GrantMinter non, mockScriptContext [non] [rd])

genGrantMinterInvalidNoUpdate :: Gen (RoleDatum, RoleRedeemer, ScriptContext)
genGrantMinterInvalidNoUpdate = do
  rd@(RoleDatum ms) <- genRoleDatum `suchThat` \(RoleDatum xs) -> not (null xs) && length xs < length samplePKHs
  let newM = head (filter (`notElem` ms) samplePKHs)
  return (rd, GrantMinter newM, mockScriptContext [head ms] [rd])

genVerifyMinterValid :: Gen (RoleDatum, RoleRedeemer, ScriptContext)
genVerifyMinterValid = do
  rd@(RoleDatum ms) <- genRoleDatum `suchThat` \(RoleDatum xs) -> not (null xs)
  let validMinter = head ms
  return (rd, VerifyMinter validMinter, mockScriptContext [validMinter] [])


genVerifyMinterInvalid :: Gen (RoleDatum, RoleRedeemer, ScriptContext)
genVerifyMinterInvalid = do
  rd@(RoleDatum ms) <- genRoleDatum `suchThat` \(RoleDatum xs) ->
                        length xs < length samplePKHs
  let nonMinters = filter (`notElem` ms) samplePKHs
  non <- elements nonMinters
  return (rd, VerifyMinter non, mockScriptContext [non] [])


------------------------------------------------------------
-- Original Properties
------------------------------------------------------------
prop_grantMinterValid :: Property
prop_grantMinterValid = forAll genGrantMinterValid $ \(d,r,c) ->
  mkPlastikRoleValidator d r c === True

prop_grantMinterInvalidNoAdminSig :: Property
prop_grantMinterInvalidNoAdminSig = forAll genGrantMinterInvalidNoAdminSig $ \(d,r,c) ->
  mkPlastikRoleValidator d r c === False

prop_grantMinterInvalidNoUpdate :: Property
prop_grantMinterInvalidNoUpdate = forAll genGrantMinterInvalidNoUpdate $ \(d,r,c) ->
  mkPlastikRoleValidator d r c === False

prop_verifyMinterValid :: Property
prop_verifyMinterValid = forAll genVerifyMinterValid $ \(d,r,c) ->
  mkPlastikRoleValidator d r c === True

prop_verifyMinterInvalid :: Property
prop_verifyMinterInvalid = forAll genVerifyMinterInvalid $ \(d,r,c) ->
  mkPlastikRoleValidator d r c === False

prop_grantMinterEmptyDatum :: Property
prop_grantMinterEmptyDatum =
  let rd = RoleDatum []
      ctx = mockScriptContext [alicePKH] [rd]
  in mkPlastikRoleValidator rd (GrantMinter alicePKH) ctx === False


prop_verifyMinterNoOutputs :: Property
prop_verifyMinterNoOutputs = forAll genRoleDatum $ \(RoleDatum ms) ->
  let validMinter = head ms
      rd          = RoleDatum ms
      ctx         = mockScriptContext [validMinter] []
  in mkPlastikRoleValidator rd (VerifyMinter validMinter) ctx === True

data DummyRedeemer = Dummy deriving Show
PlutusTx.unstableMakeIsData ''DummyRedeemer

prop_unexpectedRedeemerFails :: Property
prop_unexpectedRedeemerFails = forAll genRoleDatum $ \rd ->
  let dummyBD = toBuiltinData Dummy :: BuiltinData
      dummyRedeemer = unsafeFromBuiltinData dummyBD :: RoleRedeemer
      ctx     = mockScriptContext [head (minters rd)] []
  in mkPlastikRoleValidator rd dummyRedeemer ctx === False

------------------------------------------------------------
-- Test Execution
------------------------------------------------------------
runNamed :: String -> Property -> IO ()
runNamed name prop = do
  putStrLn $ "\n⎯⎯⎯⎯ " <> name <> " ⎯⎯⎯⎯"
  quickCheck prop

runTests :: IO ()
runTests = do
  putStrLn "\n=== Testing PlastikRoleV2 ==="
  runNamed "GrantMinter: succeeds when admin signs & datum updated"        prop_grantMinterValid
  runNamed "GrantMinter: fails when no admin signature"                   prop_grantMinterInvalidNoAdminSig
  runNamed "GrantMinter: fails when datum not updated"                    prop_grantMinterInvalidNoUpdate
  runNamed "GrantMinter: fails on empty datum"                            prop_grantMinterEmptyDatum
  runNamed "VerifyMinter: succeeds if a minter signs"                     prop_verifyMinterValid
  runNamed "VerifyMinter: fails if non-minter signs"                     prop_verifyMinterInvalid
  runNamed "VerifyMinter: still succeeds with no outputs"                 prop_verifyMinterNoOutputs
  runNamed "Unexpected redeemer always fails"                             prop_unexpectedRedeemerFails
  putStrLn "\n=== Completed all test cases ==="

main :: IO ()
main = runTests
