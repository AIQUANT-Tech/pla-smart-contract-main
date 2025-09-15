{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
 
module Plastik.WhiteList where
 
import Cardano.Api
import Cardano.Api.Shelley
import Codec.Serialise
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import GHC.Generics (Generic)
import Plutus.V2.Ledger.Api
  ( OutputDatum (..),
    PubKeyHash (..),
    ScriptContext,
    ScriptPurpose (..),
    TxInfo (..),
    TxOut (..),
    Validator,
    getDatum,
    mkValidatorScript,
    unValidatorScript,
  )
import Plutus.V2.Ledger.Contexts (scriptContextTxInfo, txSignedBy)
import qualified PlutusTx
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import System.Directory
import System.FilePath
import Prelude (FilePath, IO, Show, print, putStrLn, getLine)
 
-- | Datum containing only whitelist entries
newtype WhitelistDatum = WD
  { wdEntries :: Map.Map PubKeyHash Bool
  }
  deriving (Show, Generic)
 
PlutusTx.unstableMakeIsData ''WhitelistDatum
 
-- | Redeemer actions with admin PKH provided at call time
data WhitelistAction
  = UpdateEntries (Map.Map PubKeyHash Bool)
  | Verify PubKeyHash
  deriving (Show, Generic)
 
PlutusTx.unstableMakeIsData ''WhitelistAction
 
{-# INLINEABLE mkWhitelistValidator #-}
mkWhitelistValidator :: PubKeyHash -> WhitelistDatum -> WhitelistAction -> ScriptContext -> Bool
mkWhitelistValidator adminPkh datum action ctx =
  case action of
    UpdateEntries newEntries -> validateUpdate newEntries
    Verify addr -> validateVerification addr
  where
    info = scriptContextTxInfo ctx
    currentEntries = wdEntries datum
 
    validateUpdate :: Map.Map PubKeyHash Bool -> Bool
    validateUpdate newEntries =
      traceIfFalse "Transaction not signed by admin" (txSignedBy info adminPkh)
        && traceIfFalse "Invalid output state" (outputDatumValid newEntries)
 
    outputDatumValid :: Map.Map PubKeyHash Bool -> Bool
    outputDatumValid newEntries =
      any
        ( \o -> case txOutDatum o of
            OutputDatum d ->
              case PlutusTx.fromBuiltinData (getDatum d) of
                Just (WD entries) -> entries == newEntries
                _ -> False
            _ -> False
        )
        (txInfoOutputs info)
 
    validateVerification :: PubKeyHash -> Bool
    validateVerification addr =
      traceIfFalse "Address not whitelisted" $
        maybe False id (Map.lookup addr currentEntries)
 
-- Boilerplate for Plutus validator
{-# INLINEABLE wrappedValidator #-}
wrappedValidator :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedValidator adminPkh d r c =
  let datum = PlutusTx.unsafeFromBuiltinData d
      action = PlutusTx.unsafeFromBuiltinData r
      context = PlutusTx.unsafeFromBuiltinData c
   in if mkWhitelistValidator adminPkh datum action context then () else error ()
 
validator :: PubKeyHash -> Validator
validator adminPkh =
  mkValidatorScript $
    $$(PlutusTx.compile [||wrappedValidator||])
      `PlutusTx.applyCode` PlutusTx.liftCode adminPkh
 
saveWhiteListValidator :: IO ()
saveWhiteListValidator = do
  putStrLn "Enter admin PubKeyHash (hex):"
  pkhHex <- getLine
  putStrLn "Enter output filepath:"
  outPath <- getLine
  case B16.decode (C8.pack pkhHex) of
    Right raw -> do
      let pkh = PubKeyHash (toBuiltin raw)
          script = validator pkh
          scriptBin = serialise script
          shortBs = SBS.toShort . LBS.toStrict $ scriptBin
          plutusScript = PlutusScriptSerialised shortBs
      createDirectoryIfMissing True (takeDirectory outPath)
      res <- writeFileTextEnvelope @(PlutusScript PlutusScriptV2) outPath Nothing plutusScript
      case res of
        Left err -> print $ displayError err
        Right () -> putStrLn "Successfully wrote WhiteList validator."
    Left err -> putStrLn $ "Invalid hex: " ++ err
 
main :: IO ()
main = saveWhiteListValidator
 