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

module Plastik.Test.TestWhiteList where

import           PlutusTx                    (Data (..), compile)
import qualified PlutusTx
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts   (txSignedBy)
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import qualified PlutusTx.AssocMap           as Map
import           Prelude                     (Show(..), String, IO)
import           GHC.Generics                (Generic)
import           PlutusTx.Builtins           (error)
import qualified Test.Tasty                 as Tasty
import qualified Test.Tasty.HUnit           as HUnit
import           Test.Tasty.QuickCheck      as QC

-- Module being tested
import           Plastik.WhiteList           (WhitelistDatum (..), WhitelistAction (..), mkWhitelistValidator, validator)

--------------------------------------------------------------------------------
-- Test Data and Helpers
--------------------------------------------------------------------------------

-- Define some test public key hashes
adminPKH, alicePKH, bobPKH, charliePKH :: PubKeyHash
adminPKH   = fromString "A0"
alicePKH   = fromString "A1"
bobPKH     = fromString "B2"
charliePKH = fromString "C3"

-- Helper to create a valid output with a given datum for testing
mkOutput :: WhitelistDatum -> TxOut
mkOutput d = TxOut (Address (ScriptCredential (ValidatorHash "mockhash")) Nothing)
                   (Ada.lovelaceValueOf 2_000_000)
                   (OutputDatum (Datum (PlutusTx.toBuiltinData d)))
                   Nothing

-- Helper to create a mock ScriptContext
mockContext :: [PubKeyHash] -> [TxOut] -> WhitelistDatum -> ScriptContext
mockContext signers outputs initialDatum =
    let
        inputTxOut = mkOutput initialDatum
        txInInfo = TxInInfo (TxOutRef "mockTxId" 0) inputTxOut
        txInfo = TxInfo
            { txInfoInputs = [txInInfo]
            , txInfoOutputs = outputs
            , txInfoFee = mempty
            , txInfoMint = mempty
            , txInfoDCert = []
            , txInfoWdrl = Map.empty
            , txInfoValidRange = P.Interval.always
            , txInfoSignatories = signers
            , txInfoData = Map.empty
            , txInfoId = "mockTxId"
            }
        purpose = Spending (txInInfoOutRef txInInfo)
    in ScriptContext txInfo purpose

--------------------------------------------------------------------------------
-- Arbitrary Instances for QuickCheck
--------------------------------------------------------------------------------

-- Generator for PubKeyHash
instance Arbitrary PubKeyHash where
    arbitrary = oneof [pure alicePKH, pure bobPKH, pure charliePKH]

-- Generator for WhitelistDatum
instance Arbitrary WhitelistDatum where
    arbitrary = WD . Map.fromList <$> listOf ((,) <$> arbitrary <*> arbitrary)

-- Generator for WhitelistAction
instance Arbitrary WhitelistAction where
    arbitrary = oneof
        [ UpdateEntries . Map.fromList <$> listOf ((,) <$> arbitrary <*> arbitrary)
        , Verify <$> arbitrary
        ]

--------------------------------------------------------------------------------
-- QuickCheck Properties
--------------------------------------------------------------------------------

-- A transaction to update entries must be signed by the admin.
prop_updateSignedByAdmin :: Property
prop_updateSignedByAdmin =
    forAll arbitrary $ \(datum, newEntriesMap) ->
        let
            newEntries = Map.fromList newEntriesMap
            action     = UpdateEntries newEntries
            -- Create a valid continuing output with the new state
            validOutput = [mkOutput (WD newEntries)]
            ctx        = mockContext [adminPKH] validOutput datum
        in
            mkWhitelistValidator adminPKH datum action ctx === True

-- A transaction to update entries without an admin signature must fail.
prop_updateFailsWithoutAdminSig :: Property
prop_updateFailsWithoutAdminSig =
    forAll arbitrary $ \(datum, newEntriesMap, nonAdminSigner) ->
        nonAdminSigner /= adminPKH ==> -- Ensure the signer is not the admin
            let
                newEntries = Map.fromList newEntriesMap
                action     = UpdateEntries newEntries
                output     = [mkOutput (WD newEntries)]
                ctx        = mockContext [nonAdminSigner] output datum
            in
                mkWhitelistValidator adminPKH datum action ctx === False

-- A verification check for a whitelisted address must succeed.
prop_verifyWhitelistedSucceeds :: Property
prop_verifyWhitelistedSucceeds =
    forAll arbitrary $ \pkh ->
        let
            -- Create a state where the target PKH is definitely whitelisted
            initialDatum = WD (Map.singleton pkh True)
            action       = Verify pkh
            -- The output must be identical to the input for a 'Verify' action
            output       = [mkOutput initialDatum]
            ctx          = mockContext [] output initialDatum
        in
            mkWhitelistValidator adminPKH initialDatum action ctx === True

-- A verification check for a non-whitelisted address must fail.
prop_verifyNotWhitelistedFails :: Property
prop_verifyNotWhitelistedFails =
    forAll arbitrary $ \(datum, pkhToVerify) ->
        -- Ensure the address is NOT in the whitelist, or if it is, its value is False
        (Map.lookup pkhToVerify (wdEntries datum) `elem` [Nothing, Just False]) ==>
            let
                action = Verify pkhToVerify
                output = [mkOutput datum] -- State must not change
                ctx    = mockContext [] output datum
            in
                mkWhitelistValidator adminPKH datum action ctx === False

-- Updating entries must produce a valid continuing output datum.
prop_updateRequiresValidOutputDatum :: Property
prop_updateRequiresValidOutputDatum =
    forAll arbitrary $ \(datum, newEntriesMap) ->
        let
            newEntries = Map.fromList newEntriesMap
            action     = UpdateEntries newEntries
            -- Create an INVALID output (e.g., wrong datum or no output)
            invalidOutput = [mkOutput (WD Map.empty)]
            ctx        = mockContext [adminPKH] invalidOutput datum
        in
            mkWhitelistValidator adminPKH datum action ctx === False

--------------------------------------------------------------------------------
-- HUnit Tests
--------------------------------------------------------------------------------

unit_tests :: Tasty.TestTree
unit_tests = Tasty.testGroup "Unit Tests for Whitelist Validator"
    [ HUnit.testCase "Admin can add an entry" $ do
        let initialDatum = WD Map.empty
            newEntries = Map.singleton alicePKH True
            action = UpdateEntries newEntries
            output = [mkOutput (WD newEntries)]
            ctx = mockContext [adminPKH] output initialDatum
        HUnit.assertBool "Admin update should be valid" (mkWhitelistValidator adminPKH initialDatum action ctx)

    , HUnit.testCase "Non-admin cannot add an entry" $ do
        let initialDatum = WD Map.empty
            newEntries = Map.singleton alicePKH True
            action = UpdateEntries newEntries
            output = [mkOutput (WD newEntries)]
            ctx = mockContext [bobPKH] output initialDatum -- Signed by Bob
        HUnit.assertBool "Non-admin update should be invalid" (not (mkWhitelistValidator adminPKH initialDatum action ctx))

    , HUnit.testCase "Check a verified user" $ do
        let initialDatum = WD (Map.fromList [(alicePKH, True), (bobPKH, True)])
            action = Verify bobPKH
            output = [mkOutput initialDatum] -- No change in datum
            ctx = mockContext [] output initialDatum
        HUnit.assertBool "Verification for whitelisted user should succeed" (mkWhitelistValidator adminPKH initialDatum action ctx)

    , HUnit.testCase "Check a non-verified user" $ do
        let initialDatum = WD (Map.fromList [(alicePKH, True)])
            action = Verify bobPKH -- Bob is not in the map
            output = [mkOutput initialDatum]
            ctx = mockContext [] output initialDatum
        HUnit.assertBool "Verification for non-whitelisted user should fail" (not (mkWhitelistValidator adminPKH initialDatum action ctx))
    ]

--------------------------------------------------------------------------------
-- Test Suite
--------------------------------------------------------------------------------

tests :: Tasty.TestTree
tests = Tasty.testGroup "Whitelist Smart Contract"
    [ Tasty.testGroup "Property-Based Tests"
        [ testProperty "Update must be signed by admin" prop_updateSignedByAdmin
        , testProperty "Update fails without admin signature" prop_updateFailsWithoutAdminSig
        , testProperty "Verification of whitelisted address succeeds" prop_verifyWhitelistedSucceeds
        , testProperty "Verification of non-whitelisted address fails" prop_verifyNotWhitelistedFails
        , testProperty "Update must have a valid continuing output" prop_updateRequiresValidOutputDatum
        ]
    , unit_tests
    ]

main :: IO ()
main = Tasty.defaultMain tests