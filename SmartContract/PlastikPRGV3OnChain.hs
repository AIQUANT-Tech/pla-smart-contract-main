{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Plastik.PlastikPRGV3OnChain where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2, writeFileTextEnvelope)
import Codec.Serialise (serialise)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import GHC.Generics
import Plutus.V2.Ledger.Api
  ( BuiltinData,
    Datum (..),
    OutputDatum (..),
    PubKeyHash (..),
    ScriptContext,
    TxInfo (..),
    TxOut (..),
    Validator,
    mkValidatorScript,
    unsafeFromBuiltinData,
  )
import Plutus.V2.Ledger.Contexts (scriptContextTxInfo, txSignedBy)
import PlutusTx (applyCode, compile, liftCode, makeIsDataIndexed)
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Prelude (Eq, IO, Show, String, getLine, putStrLn, show, ($), (<>))

-- | Datum type with admin & pre-approved PKHs
-- Only the `prePKH` from the datum is allowed to redeem by providing it as redeemer.
data PlastikDatum = PlastikDatum
  { prePKH :: PubKeyHash,
    totalSupply :: Integer,
    salesMap :: [(PubKeyHash, Integer)],
    assetIdentifier :: BuiltinByteString
  }
  deriving (Show, Generic, Prelude.Eq)

-- | Expanded redeemer type with multiple actions
data PlastikRedeemer
  = PreRedeem {redeemerPKH :: PubKeyHash}
  | AddOrUpdateSale {salePKH :: PubKeyHash, saleAmount :: Integer}
  deriving (Show, Generic, Prelude.Eq)

makeIsDataIndexed ''PlastikDatum [('PlastikDatum, 0)]
makeIsDataIndexed
  ''PlastikRedeemer
  [ ('PreRedeem, 0),
    ('AddOrUpdateSale, 1)
  ]

{-# INLINEABLE mkPlastikValidator #-}
mkPlastikValidator :: PubKeyHash -> PlastikDatum -> PlastikRedeemer -> ScriptContext -> Bool
mkPlastikValidator adminPKH datum redeemer ctx =
  case redeemer of
    PreRedeem redeemerPKH ->
      traceIfFalse "Redeemer mismatch" (redeemerPKH == prePKH datum)
        && traceIfFalse "Signature missing" (txSignedBy info redeemerPKH)
        && traceIfFalse "Admin did not sign" (txSignedBy info adminPKH)
    AddOrUpdateSale pkh amount ->
      traceIfFalse "Admin signature missing" (txSignedBy info adminPKH)
        && checkOutputDatum pkh amount
  where
    info = scriptContextTxInfo ctx

    checkOutputDatum pkh amount =
      let ownOutput = getContinuingOutput ctx
          newDatum = case txOutDatum ownOutput of
            OutputDatum (Datum d) -> unsafeFromBuiltinData d
            _ -> traceError "Output datum missing"

          originalSales = salesMap datum
          updatedSales = updateSales originalSales pkh amount
       in traceIfFalse "Output datum prePKH mismatch" (prePKH newDatum == prePKH datum)
            && traceIfFalse "Output totalSupply mismatch" (totalSupply newDatum == totalSupply datum)
            && traceIfFalse "Output assetIdentifier mismatch" (assetIdentifier newDatum == assetIdentifier datum)
            && traceIfFalse "SalesMap not updated correctly" (salesMap newDatum == updatedSales)

    updateSales sales pkh amount =
      let filtered = filter (\(k, _) -> k /= pkh) sales
       in (pkh, amount) : filtered

    getContinuingOutput :: ScriptContext -> TxOut
    getContinuingOutput ctx' =
      case [ o | o <- txInfoOutputs (scriptContextTxInfo ctx'), case txOutDatum o of
                                                                  OutputDatum _ -> True
                                                                  _ -> False
           ] of
        [o] -> o
        _ -> traceError "Expected exactly one continuing output"

-- Deserialize datum and redeemer
{-# INLINEABLE wrappedValidator #-}
wrappedValidator :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedValidator adminBD d r ctxBD =
  let datum = unsafeFromBuiltinData @PlastikDatum d
      redeemer = unsafeFromBuiltinData @PlastikRedeemer r
      ctx = unsafeFromBuiltinData @ScriptContext ctxBD
   in if mkPlastikValidator adminBD datum redeemer ctx
        then ()
        else traceError "Validation failed"

-- | Compile the validator with an admin PKH parameter
validator :: PubKeyHash -> Validator
validator admin =
  mkValidatorScript Prelude.$
    $$(compile [||wrappedValidator||]) `applyCode` liftCode admin

-- | Export the script as a .plutus file
savePRGValidator :: IO ()
savePRGValidator = do
  putStrLn "Enter admin PubKeyHash (hex):"
  pkhHex <- getLine
  putStrLn "Enter output filepath (e.g. compiled/PlastikPRGV3.plutus):"
  outPath <- getLine
  case B16.decode (C8.pack pkhHex) of
    Left err -> putStrLn ("Invalid hex: " <> err)
    Right raw -> do
      let pkh = PubKeyHash (toBuiltin raw)
          script = validator pkh
          bs = serialise script
          shortBs = SBS.toShort (LBS.toStrict bs)
          plutusScr = PlutusScriptSerialised shortBs :: PlutusScript PlutusScriptV2

      createDirectoryIfMissing True (takeDirectory outPath)
      res <- writeFileTextEnvelope outPath Nothing plutusScr
      putStrLn Prelude.$ case res of
        Left e -> "Error writing script: " <> show e
        Right () -> "Validator written successfully!"

main :: IO ()
main = savePRGValidator
