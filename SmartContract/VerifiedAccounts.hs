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

module Plastik.VerifiedAccounts where

-- Use PlutusTx AssocMap

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2, displayError, writeFileTextEnvelope)
import Codec.Serialise (serialise)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import GHC.Generics (Generic)
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import Plutus.V2.Ledger.Api
  ( BuiltinData,
    Datum (..),
    OutputDatum (..),
    PubKeyHash (..),
    ScriptContext (..),
    TxOut (..),
    Validator,
    mkValidatorScript,
    txInfoOutputs,
    txOutDatum,
    unValidatorScript,
    unsafeFromBuiltinData,
  )
import Plutus.V2.Ledger.Contexts (scriptContextTxInfo, txSignedBy)
import PlutusTx (Data (..), applyCode, compile, liftCode)
import qualified PlutusTx
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Builtins (emptyByteString, error)
import PlutusTx.Builtins.Internal (BuiltinData)
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Prelude (FilePath, IO, Show (show), String, getLine, print, putStrLn)

--------------------------------------------------------------------------------
-- On-Chain State (Datum)
--------------------------------------------------------------------------------

-- | The state holds the contract owner, a set of addresses with the validator role (represented as Map keys),
-- and a map of addresses to their verified status.
data VerifiedAccountsState = VerifiedAccountsState
  { owner :: PubKeyHash,
    validators :: Map.Map PubKeyHash Bool, -- Using Map to represent a set (keys are validators)
    verifiedMap :: Map.Map PubKeyHash Bool
  }
  deriving (Show, Generic)

PlutusTx.unstableMakeIsData ''VerifiedAccountsState

--------------------------------------------------------------------------------
-- Redeemer Type
--------------------------------------------------------------------------------

data VerifyAction
  = UpdateVerification PubKeyHash Bool
  | CheckVerification PubKeyHash
  | UpdateValidator PubKeyHash Bool
  deriving (Show, Generic)

PlutusTx.unstableMakeIsData ''VerifyAction

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

{-# INLINEABLE updateVerification #-}
updateVerification :: Map.Map PubKeyHash Bool -> PubKeyHash -> Bool -> Map.Map PubKeyHash Bool
updateVerification m key newVal = Map.insert key newVal m

{-# INLINEABLE isVerified #-}
isVerified :: Map.Map PubKeyHash Bool -> PubKeyHash -> Bool
isVerified m key = case Map.lookup key m of
  Just b -> b
  Nothing -> False

{-# INLINEABLE getContinuingOutput #-}
getContinuingOutput :: ScriptContext -> TxOut
getContinuingOutput ctx =
  case [ o | o <- txInfoOutputs (scriptContextTxInfo ctx), case txOutDatum o of
                                                             OutputDatum _ -> True
                                                             _ -> False
       ] of
    [o] -> o
    _ -> traceError "Expected exactly one continuing output so attach it"

--------------------------------------------------------------------------------
-- On-Chain Validator
--------------------------------------------------------------------------------

{-# INLINEABLE mkVerifiedValidator #-}
mkVerifiedValidator :: PubKeyHash -> VerifiedAccountsState -> VerifyAction -> ScriptContext -> Bool
mkVerifiedValidator adminPkh oldState action ctx =
  let info = scriptContextTxInfo ctx
   in case action of
        UpdateVerification target newVal ->
          let validatorKeys = Map.keys (validators oldState)
              authorized = any (\v -> txSignedBy info v) validatorKeys
           in if not authorized
                then traceError "Not authorized: Only validators can update verification do it accordingly"
                else
                  let out = getContinuingOutput ctx
                      newDatum = case txOutDatum out of
                        OutputDatum (Datum d) -> case PlutusTx.fromBuiltinData d of
                          Just s -> s
                          Nothing -> traceError "Invalid output datum"
                        _ -> traceError "Output datum missing"
                   in -- Check state consistency
                      (adminPkh == owner newDatum)
                        && (validators oldState == validators newDatum)
                        && (verifiedMap newDatum == updateVerification (verifiedMap oldState) target newVal)
        UpdateValidator target newVal ->
          let signedByOwner = txSignedBy info adminPkh
           in if not signedByOwner
                then traceError "Authorization failed: Only owner can update validators"
                else
                  let out = getContinuingOutput ctx
                      newDatum = case txOutDatum out of
                        OutputDatum (Datum d) -> case PlutusTx.fromBuiltinData d of
                          Just s -> s
                          Nothing -> traceError "Invalid output datum"
                        _ -> traceError "Output datum missing"
                   in -- Check state consistency
                      (adminPkh == owner newDatum)
                        && (validators newDatum == Map.insert target newVal (validators oldState))
                        && (verifiedMap newDatum == verifiedMap oldState)
        CheckVerification target ->
          let out = getContinuingOutput ctx
              newDatum = case txOutDatum out of
                OutputDatum (Datum d) -> case PlutusTx.fromBuiltinData d of
                  Just s -> s
                  Nothing -> traceError "Invalid output datum check it....."
                _ -> traceError "Output datum missing check it...."
              verificationStatus =
                if isVerified (verifiedMap oldState) target
                  then trace "Debug: Person is VERIFIED" True
                  else trace "Debug: Person is NOT VERIFIED" False
           in (adminPkh == owner newDatum)
                && (validators oldState == validators newDatum)
                && (verifiedMap oldState == verifiedMap newDatum)
                && verificationStatus

{-# INLINEABLE wrapValidator #-}
wrapValidator :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapValidator adminPkh datum redeemer context =
  let state = unsafeFromBuiltinData datum :: VerifiedAccountsState
      action = unsafeFromBuiltinData redeemer :: VerifyAction
      ctx = unsafeFromBuiltinData context :: ScriptContext
   in if mkVerifiedValidator adminPkh state action ctx
        then ()
        else traceError "Verification update failed."

validator :: PubKeyHash -> Validator
validator adminPkh =
  mkValidatorScript $
    $$(compile [||wrapValidator||]) `applyCode` liftCode adminPkh

-- SCRIPT FILE OUTPUT
saveCryptoValidator :: IO ()
saveCryptoValidator = do
  putStrLn "Enter admin PubKeyHash (hex):"
  pkhHex <- getLine
  putStrLn "Enter output filepath :"
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
