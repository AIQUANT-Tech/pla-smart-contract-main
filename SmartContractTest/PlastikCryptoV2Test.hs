{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Plastik.PlastikCryptoV2Test (main) where

import Prelude                 (IO, Bool(..), String, ($), (==), Maybe(..), (<$>), (<*>), mempty, (+), (>=), pure, (&&), (-), (.))
import Test.Tasty              (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit        (testCase, (@?=))
import Test.Tasty.QuickCheck   (testProperty, forAll, Gen)
import Test.QuickCheck         (Arbitrary(..), vectorOf, elements, suchThat)

import Plutus.V2.Ledger.Api    (PubKeyHash (..), POSIXTime (..), ScriptContext (..), TxInfo (..), TxId (..), BuiltinByteString, toBuiltin)
import Plutus.V2.Ledger.Contexts
    ( scriptContextTxInfo
    , TxOutRef (..)
    , ScriptPurpose (Spending)
    )
import Plutus.V1.Ledger.Interval (to)
import qualified PlutusTx.AssocMap as AM
import qualified Data.ByteString.Char8 as C8
import PlutusTx.Builtins (BuiltinByteString, toBuiltin)


import Plastik.PlastikCryptoV2
    ( SellDatum (..)
    , NFTVoucher (..)
    , SellRequest (..)
    , PlastikRedeemer (..)
    , mkValidator
    )

-- Helper to build minimal ScriptContext
mkCtx :: [PubKeyHash] -> POSIXTime -> ScriptContext
mkCtx signers validUntil =
    let txinfo = TxInfo
          { txInfoInputs      = []
          , txInfoOutputs     = []
          , txInfoFee         = mempty
          , txInfoMint        = mempty
          , txInfoDCert       = mempty
          , txInfoWdrl        = AM.empty
          , txInfoValidRange  = to validUntil
          , txInfoSignatories = signers
          , txInfoData        = AM.empty
          , txInfoId          = TxId ""
          } 
    in ScriptContext txinfo (Spending $ TxOutRef (TxId "") 0)


-- Dummy keys and on-chain values
dummySeller, dummyBuyer, dummyAdmin :: PubKeyHash
dummySeller = PubKeyHash "S"
dummyBuyer  = PubKeyHash "B"
dummyAdmin  = PubKeyHash "A"

datum :: SellDatum
datum = SellDatum dummySeller "policy" "token"

voucher :: NFTVoucher
voucher = NFTVoucher "policy" "token" 1 "uri" dummySeller 5

validSell :: SellRequest
validSell = SellRequest
    { srTokenPolicy = "policy"
    , srTokenId     = "token"
    , srPrice       = 100
    , srAmount      = 1
    , srSeller      = dummySeller
    , piBuyer       = dummyBuyer
    , piPrice       = 100
    , piTimestamp   = POSIXTime 1_000_000
    }

wrongPriceSell, expiredSell :: SellRequest
wrongPriceSell = validSell { piPrice = 99 }
expiredSell    = validSell { piTimestamp = POSIXTime 0 }

-- Unit tests covering each branch and validation
unitTests :: TestTree
unitTests = testGroup "Unit: mkValidator"
  [ testCase "RedeemVoucher: success when creator signs" $
      mkValidator dummyAdmin datum (RedeemVoucher voucher) (mkCtx [dummySeller] 0) @?= True

  , testCase "RedeemVoucher: fail when creator missing" $
      mkValidator dummyAdmin datum (RedeemVoucher voucher) (mkCtx [] 0) @?= False

  , testCase "CompleteSale: success on correct inputs" $
      let deadline = piTimestamp validSell + 600_000
      in mkValidator dummyAdmin datum (CompleteSale validSell) (mkCtx [dummySeller, dummyBuyer, dummyAdmin] deadline) @?= True

  , testCase "CompleteSale: fail missing seller signature" $
      let deadline = piTimestamp validSell + 600_000
      in mkValidator dummyAdmin datum (CompleteSale validSell) (mkCtx [dummyBuyer, dummyAdmin] deadline) @?= False

  , testCase "CompleteSale: fail missing buyer signature" $
      let deadline = piTimestamp validSell + 600_000
      in mkValidator dummyAdmin datum (CompleteSale validSell) (mkCtx [dummySeller, dummyAdmin] deadline) @?= False

  , testCase "CompleteSale: fail missing admin signature" $
      let deadline = piTimestamp validSell + 600_000
      in mkValidator dummyAdmin datum (CompleteSale validSell) (mkCtx [dummySeller, dummyBuyer] deadline) @?= False

  , testCase "CompleteSale: fail on wrong price" $
      let deadline = piTimestamp validSell + 600_000
      in mkValidator dummyAdmin datum (CompleteSale wrongPriceSell) (mkCtx [dummySeller, dummyBuyer, dummyAdmin] deadline) @?= False

  , testCase "CompleteSale: fail past deadline" $
      mkValidator dummyAdmin datum (CompleteSale expiredSell) (mkCtx [dummySeller, dummyBuyer, dummyAdmin] (piTimestamp expiredSell + 600_000 + 1)) @?= False
  ]

-- Arbitrary instances
instance Arbitrary PubKeyHash where
  arbitrary = PubKeyHash <$> arbitrary

instance Arbitrary BuiltinByteString where
  arbitrary = toBuiltin . C8.pack <$> vectorOf 8 (elements ['a'..'z'])


instance Arbitrary NFTVoucher where
  arbitrary = NFTVoucher <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SellRequest where
  arbitrary = do
    policy <- arbitrary
    tok    <- arbitrary
    price  <- arbitrary `suchThat` (>= 0)
    amt    <- arbitrary `suchThat` (>= 0)
    seller <- arbitrary
    buyer  <- arbitrary
    ts     <- arbitrary `suchThat` (>= 1_000)
    pure $ SellRequest policy tok price amt seller buyer price (POSIXTime ts)

-- Generator for (Datum, SellRequest) pairs
genSale :: Gen (SellDatum, SellRequest)
genSale = do
  seller <- arbitrary
  policy <- arbitrary
  tok    <- arbitrary
  price  <- arbitrary `suchThat` (>= 0)
  amt    <- arbitrary `suchThat` (>= 0)
  buyer  <- arbitrary
  ts     <- arbitrary `suchThat` (>= 1_000)
  let dat = SellDatum seller policy tok
      req = SellRequest policy tok price amt seller buyer price (POSIXTime ts)
  pure (dat, req)

-- QuickCheck properties covering RedeemVoucher tests
redeemProps :: TestTree
redeemProps = testGroup "Properties: RedeemVoucher"
  [ testProperty "redeem succeeds only when creator signs" $
      forAll arbitrary $ \(v :: NFTVoucher) ->
        let ctxOK   = mkCtx [nvCreator v] 0
            ctxFail = mkCtx []         0
            dmyDat  = SellDatum dummySeller "" ""
        in (mkValidator dummyAdmin dmyDat (RedeemVoucher v) ctxOK == True)
           && (mkValidator dummyAdmin dmyDat (RedeemVoucher v) ctxFail == False)
  ]

-- QuickCheck properties covering CompleteSale tests
saleProps :: TestTree
saleProps = testGroup "Properties: CompleteSale"
  [ testProperty "success when all signatures present and within deadline" $
      forAll genSale $ \(dat, req) ->
        let baseTs = piTimestamp req
            ctxOK  = mkCtx [sdSeller dat, piBuyer req, dummyAdmin] (baseTs + 600_000)
        in mkValidator dummyAdmin dat (CompleteSale req) ctxOK == True

  , testProperty "fail when seller missing" $
      forAll genSale $ \(dat, req) ->
        let baseTs   = piTimestamp req
            ctxNoSel = mkCtx [piBuyer req, dummyAdmin]       (baseTs + 600_000)
        in mkValidator dummyAdmin dat (CompleteSale req) ctxNoSel == False

  , testProperty "fail when buyer missing" $
      forAll genSale $ \(dat, req) ->
        let baseTs   = piTimestamp req
            ctxNoBuy = mkCtx [sdSeller dat, dummyAdmin]      (baseTs + 600_000)
        in mkValidator dummyAdmin dat (CompleteSale req) ctxNoBuy == False

  , testProperty "fail when admin missing" $
      forAll genSale $ \(dat, req) ->
        let baseTs   = piTimestamp req
            ctxNoAdm = mkCtx [sdSeller dat, piBuyer req]     (baseTs + 600_000)
        in mkValidator dummyAdmin dat (CompleteSale req) ctxNoAdm == False

  , testProperty "fail on wrong price" $
      forAll genSale $ \(dat, req) ->
        let baseTs    = piTimestamp req
            wrongReq  = req { piPrice = piPrice req - 1 }
            ctxWrong  = mkCtx [sdSeller dat, piBuyer req, dummyAdmin] (baseTs + 600_000)
        in mkValidator dummyAdmin dat (CompleteSale wrongReq) ctxWrong == False

  , testProperty "fail past deadline" $
      forAll genSale $ \(dat, req) ->
        let baseTs  = piTimestamp req
            lateCtx = mkCtx [sdSeller dat, piBuyer req, dummyAdmin] (baseTs + 600_000 + 1)
        in mkValidator dummyAdmin dat (CompleteSale req) lateCtx == False
  ]

-- Main entry point
main :: IO ()
main = defaultMain $ testGroup "All Tests" [unitTests, redeemProps, saleProps]
