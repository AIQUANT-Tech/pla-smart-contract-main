{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
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

module Plastik.PlastikTokenOnchain where

import           PlutusTx                    (compile, unstableMakeIsData, toData, applyCode, liftCode)
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           Plutus.V2.Ledger.Api        (PubKeyHash, ScriptContext(..), 
                                              TxInfo(..), TxOut(..), OutputDatum(..),
                                              Datum(..), TxInInfo(..), TxOutRef(..),
                                              Validator, mkValidatorScript,
                                              scriptContextTxInfo, unsafeFromBuiltinData,
                                              unValidatorScript, TxInInfo, ScriptPurpose(..),
                                              Address, ValidatorHash, fromBuiltin, toBuiltin,
                                              BuiltinByteString, PubKeyHash(..))
import qualified Plutus.V2.Ledger.Contexts   as Contexts
import           Plutus.V1.Ledger.Value      (flattenValue)
import qualified PlutusTx.AssocMap           as Map
import           Prelude                     (Show(..), Integer, (^), FilePath, IO, print, putStrLn, read, getLine)
import qualified Data.Set                    as Set
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Short       as SBS
import           Cardano.Api.Shelley         (PlutusScript (..), PlutusScriptV2, displayError, writeFileTextEnvelope)
import           System.Directory            (createDirectoryIfMissing)
import           System.FilePath             (takeDirectory)
import           Codec.Serialise             (serialise)
import           PlutusTx.Builtins           (error, emptyByteString)
import           PlutusTx.Builtins.Internal  (BuiltinData)
import qualified Data.ByteString.Char8       as BSC
import qualified PlutusTx.Builtins           as Builtins 
import qualified Data.ByteString.Base16      as B16
import qualified Data.ByteString.Char8       as C8

--------------------------------------------------------------------------------
-- Data Types
--------------------------------------------------------------------------------

data TokenState = TokenState
    { tokenName   :: BuiltinByteString
    , paused      :: Bool
    , lockers     :: Map.Map PubKeyHash Bool
    , balances    :: Map.Map PubKeyHash Integer
    , totalSupply :: Integer
    }
    deriving Show

PlutusTx.unstableMakeIsData ''TokenState

--------------------------------------------------------------------------------
-- Redeemer: Actions corresponding to each public function
--------------------------------------------------------------------------------

data TokenAction =
      Pause
    | Unpause
    | SetLocker PubKeyHash Bool
    | Transfer PubKeyHash Integer
    | TransferFrom PubKeyHash PubKeyHash Integer
    deriving Show

PlutusTx.unstableMakeIsData ''TokenAction

--------------------------------------------------------------------------------
-- State Transition Function
--------------------------------------------------------------------------------
{-# INLINABLE applyAction #-}
applyAction :: BuiltinByteString -> BuiltinByteString -> PubKeyHash -> TokenState -> TokenAction -> [PubKeyHash] -> Bool
applyAction policyId tokenName owner ts action signers =
    case action of
      Pause ->
        length signers == 1 && owner `elem` signers

      Unpause ->
        length signers == 1 && owner `elem` signers

      SetLocker pkh value ->
        length signers == 1 && owner `elem` signers

      Transfer recipient amt ->
        if paused ts
          then traceError "Token transfer: The token is now paused."
          else case signers of
                  [signer] ->
                    case Map.lookup signer (balances ts) of
                              Nothing -> traceError "No balance in sender's wallet"
                              Just senderBal ->
                                  if senderBal < amt
                                      then traceError "Insufficient funds in sender's wallet"
                                      else let recipientPrevBal = case Map.lookup recipient (balances ts) of
                                                      Nothing -> 0
                                                      Just x  -> x
                                               newBalances = Map.insert signer (senderBal - amt)
                                                              (Map.insert recipient (recipientPrevBal + amt) (balances ts))
                                               in True
                  _ -> traceError "Exactly one signer required"

      TransferFrom senderAddr recipient amt ->
        if paused ts
            then traceError "Token transferFrom: Token is now paused."
            else if length signers /= 2
                then traceError "Requires two signers - 1) Sender 2) Locker"
                else let lockersInSigners = filter (\s -> Map.lookup s (lockers ts) == Just True) signers
                     in if null lockersInSigners
                        then traceError "At least one locker must sign"
                        else if not (senderAddr `elem` signers)
                            then traceError "Sender must sign"
                            else case Map.lookup senderAddr (balances ts) of
                                    Nothing -> traceError "Sender has no balance"
                                    Just senderBal ->
                                        if senderBal < amt
                                            then traceError "Insufficient funds"
                                            else let recipientPrevBal = case Map.lookup recipient (balances ts) of
                                                              Nothing -> 0
                                                              Just x  -> x
                                                     newBalances = Map.insert senderAddr (senderBal - amt)
                                                                    (Map.insert recipient (recipientPrevBal + amt) (balances ts))
                                                     in True

--------------------------------------------------------------------------------
-- On-Chain Validator
--------------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinByteString -> BuiltinByteString -> PubKeyHash -> TokenState -> TokenAction -> ScriptContext -> Bool
mkValidator policyId tokenName owner ts action ctx =
    let info = scriptContextTxInfo ctx
        signers = txInfoSignatories info
    in applyAction policyId tokenName owner ts action signers

--------------------------------------------------------------------------------
-- Validator Wrapper
--------------------------------------------------------------------------------

{-# INLINEABLE wrapValidator #-}
wrapValidator :: BuiltinByteString -> BuiltinByteString -> PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapValidator policyId tokenName owner datum redeemer context =
  let ts = unsafeFromBuiltinData datum
      action = unsafeFromBuiltinData redeemer
      scriptContext = unsafeFromBuiltinData context
  in if mkValidator policyId tokenName owner ts action scriptContext
       then ()
       else traceError "Plastik token validation failed"

validator :: BuiltinByteString -> BuiltinByteString -> PubKeyHash -> Validator
validator policyId tokenName owner =
  mkValidatorScript $
    $$(compile [|| wrapValidator ||])
      `applyCode` liftCode policyId
      `applyCode` liftCode tokenName
      `applyCode` liftCode owner

--------------------------------------------------------------------------------
-- Script Generation
--------------------------------------------------------------------------------

savePlastikValidator :: IO ()
savePlastikValidator = do
  putStrLn "Enter policy ID :"
  policyIdHex <- getLine
  putStrLn "Enter token name (hex):"
  tokenNameHex <- getLine
  putStrLn "Enter owner's PubKeyHash (hex):"
  ownerPkhHex <- getLine
  putStrLn "Enter output filepath:"
  outPath <- getLine

  case (B16.decode (C8.pack policyIdHex), B16.decode (C8.pack tokenNameHex), B16.decode (C8.pack ownerPkhHex)) of
    (Right policyIdRaw, Right tokenNameRaw, Right ownerPkhRaw) -> do
      let policyIdBS = toBuiltin policyIdRaw
          tokenNameBS = toBuiltin tokenNameRaw
          ownerPkh = PubKeyHash (toBuiltin ownerPkhRaw)
          script = validator policyIdBS tokenNameBS ownerPkh
          scriptBin = serialise $ unValidatorScript script
          shortBs = SBS.toShort (LBS.toStrict scriptBin)
          plutusScr = PlutusScriptSerialised shortBs
      createDirectoryIfMissing True (takeDirectory outPath)
      res <- writeFileTextEnvelope @(PlutusScript PlutusScriptV2) outPath Nothing plutusScr
      case res of
        Left err -> print $ displayError err
        Right () -> putStrLn "Successfully wrote Plastik token validator."
    _ -> putStrLn "Error decoding hex input."

main :: IO ()
main = savePlastikValidator
