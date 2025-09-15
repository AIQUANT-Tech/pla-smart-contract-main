{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Plastik.PlastikCryptoV2
  ( validator,
    SellDatum (..),
    NFTVoucher (..),
    SellRequest (..),
    PlastikRedeemer (..),
    mkValidator,
    main,
  )
where

import Cardano.Api (PlutusScript (..), PlutusScriptV2, writeFileTextEnvelope)
import Cardano.Api.Shelley
import Codec.Serialise (serialise)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Plutus.V1.Ledger.Interval (contains, to)
import Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash (..), ScriptContext (..), TxInfo (..), Validator, mkValidatorScript)
import Plutus.V2.Ledger.Contexts (scriptContextTxInfo, txSignedBy)
import PlutusTx (applyCode, compile, fromBuiltinData, liftCode, makeIsDataIndexed, unsafeFromBuiltinData)
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Prelude (IO, Show (..), String, getLine, putStrLn)

-- DATUM
data SellDatum = SellDatum
  { sdSeller :: PubKeyHash,
    sdNftPolicy :: BuiltinByteString,
    sdNftId :: BuiltinByteString
  }
  deriving (Show)

makeIsDataIndexed ''SellDatum [('SellDatum, 0)]

-- REDEEMERS
data NFTVoucher = NFTVoucher
  { nvNftPolicy :: BuiltinByteString,
    nvNftId :: BuiltinByteString,
    nvAmount :: Integer,
    nvURI :: BuiltinByteString,
    nvCreator :: PubKeyHash,
    nvRoyalty :: Integer
  }
  deriving (Show)

makeIsDataIndexed ''NFTVoucher [('NFTVoucher, 0)]

data SellRequest = SellRequest
  { srTokenPolicy :: BuiltinByteString,
    srTokenId :: BuiltinByteString,
    srPrice :: Integer,
    srAmount :: Integer,
    srSeller :: PubKeyHash,
    piBuyer :: PubKeyHash,
    piPrice :: Integer
  }
  deriving (Show)

makeIsDataIndexed ''SellRequest [('SellRequest, 0)]

data PlastikRedeemer
  = RedeemVoucher NFTVoucher
  | CompleteSale SellRequest
  deriving (Show)

makeIsDataIndexed ''PlastikRedeemer [('RedeemVoucher, 0), ('CompleteSale, 1)]

-- VALIDATOR
{-# INLINEABLE mkValidator #-}
mkValidator :: PubKeyHash -> SellDatum -> PlastikRedeemer -> ScriptContext -> Bool
mkValidator adminPKH datum redeemer ctx =
  let info :: TxInfo
      info = scriptContextTxInfo ctx
   in case redeemer of
        RedeemVoucher v ->
          traceIfFalse "Voucher creator did not sign check again !" (txSignedBy info (nvCreator v))
        CompleteSale req ->
          traceIfFalse "Seller does not match with datum" (srSeller req == sdSeller datum)
            && traceIfFalse "Asset policy does not match !" (srTokenPolicy req == sdNftPolicy datum)
            && traceIfFalse "Asset ID does not match !" (srTokenId req == sdNftId datum)
            && traceIfFalse "Price does not match admin info" (srPrice req == piPrice req)
            && traceIfFalse "Seller did not sign" (txSignedBy info (srSeller req))
            && traceIfFalse "Buyer did not sign s check once" (txSignedBy info (piBuyer req))
            && traceIfFalse "Admin did not sign check once" (txSignedBy info adminPKH)

-- WRAPPER FOR ON-CHAIN USAGE
{-# INLINEABLE wrappedValidator #-}
wrappedValidator :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedValidator admin d r ctx =
  case (fromBuiltinData d, fromBuiltinData r) of
    (Just datum, Just redeemer) ->
      let context = unsafeFromBuiltinData ctx
       in if mkValidator admin datum redeemer context
            then ()
            else traceError "Validation failed"
    _ -> traceError "Deserialization failed, Check it again !"

-- VALIDATOR EXPORT
validator :: PubKeyHash -> Validator
validator adminPKH =
  mkValidatorScript $
    $$(compile [||\pkh -> wrappedValidator pkh||]) `applyCode` liftCode adminPKH

-- SCRIPT FILE OUTPUT
saveCryptoValidator :: IO ()
saveCryptoValidator = do
  putStrLn "Enter admin PubKeyHash (hex):"
  pkhHex <- getLine
  putStrLn "Enter output filepath (e.g. compiled/PlastikCryptoV2.plutus):"
  outPath <- getLine
  case B16.decode (C8.pack pkhHex) of
    Right raw -> do
      let pkh = PubKeyHash (toBuiltin raw)
          script = validator pkh
          scriptBin = serialise $ script
          shortBs = SBS.toShort (LBS.toStrict scriptBin)
          plutusScr = PlutusScriptSerialised shortBs :: PlutusScript PlutusScriptV2
      createDirectoryIfMissing True (takeDirectory outPath)
      res <- writeFileTextEnvelope outPath Nothing plutusScr
      putStrLn $ case res of
        Left err -> "Error: " ++ show err
        Right () -> "Validator written successfully!"
    Left err -> putStrLn $ "Invalid hex input: " ++ err

main :: IO ()
main = saveCryptoValidator
