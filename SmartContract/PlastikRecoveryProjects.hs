{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module PlastikV2.PlastikRecoveryProjects (
    validator,
    writeScript,
    PRPState(..),
    PRPAction(..)
) where

import           PlutusTx.Prelude         hiding (Semigroup(..), unless)
import           PlutusTx                 (Data (..), compile, unstableMakeIsData, liftCode, applyCode)
import           PlutusTx.Lift            (makeLift)
import           Plutus.V2.Ledger.Api     (
                                          PubKeyHash,
                                          Validator,
                                          ScriptContext,
                                          txOutDatum,
                                          toBuiltinData,
                                          mkValidatorScript,
                                          unsafeFromBuiltinData,
                                          Datum(..),
                                          OutputDatum(..),
                                          TxOut,
                                          TxOutRef,
                                          unValidatorScript
                                         )
import           Plutus.V2.Ledger.Contexts (
                                          txSignedBy,
                                          scriptContextTxInfo,
                                          txInfoOutputs,
                                          txOutAddress,
                                          scriptContextPurpose,
                                          TxInInfo(..),
                                          txInInfoResolved,
                                          txInfoInputs
                                         )
import           PlutusTx.AssocMap        (Map)
import qualified PlutusTx.AssocMap as Map
import           GHC.Generics             (Generic)
import           Prelude                  (Show, FilePath, IO, print, putStrLn)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV2, displayError, writeFileTextEnvelope)
import           Codec.Serialise          (serialise)
import           System.Directory         (createDirectoryIfMissing)
import           System.FilePath          (takeDirectory)
import Plutus.Script.Utils.V1.Contexts (ScriptPurpose(..))
import Plutus.V1.Ledger.Api (FromData(..))

------------------------------------------------------------
-- On-Chain Data Types
------------------------------------------------------------

newtype PRPState = PRPState
    { projects :: Map PubKeyHash Bool }
    deriving (Show, Generic, Eq)

PlutusTx.unstableMakeIsData ''PRPState

data PRPAction =
      UpdateProject PubKeyHash Bool
    | CheckProject PubKeyHash
    deriving (Show, Generic)

PlutusTx.unstableMakeIsData ''PRPAction


------------------------------------------------------------
-- Helper Functions
------------------------------------------------------------

{-# INLINABLE getContinuingOutput #-}
getContinuingOutput :: ScriptContext -> TxOut
getContinuingOutput ctx =
    let
        ownAddress = case scriptContextPurpose ctx of
            Spending txOutRef -> txOutAddress $ txInInfoResolved $ findOwnInput ctx txOutRef
            _ -> traceError "Invalid script purpose"
        outputs = txInfoOutputs $ scriptContextTxInfo ctx
        validOutputs = [ o | o <- outputs
                           , txOutAddress o == ownAddress
                           , case txOutDatum o of
                               OutputDatum _ -> True
                               _             -> False
                        ]
    in case validOutputs of
        [o] -> o
        _   -> traceError "Expected exactly one continuing output"

{-# INLINABLE findOwnInput #-}
findOwnInput :: ScriptContext -> TxOutRef -> TxInInfo
findOwnInput ctx txOutRef =
    case find (\i -> txInInfoOutRef i == txOutRef) (txInfoInputs $ scriptContextTxInfo ctx) of
        Just i  -> i
        Nothing -> traceError "Own input not found"

------------------------------------------------------------
-- Validation Logic (parameterized by admin)
------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: PubKeyHash -> PRPState -> PRPAction -> ScriptContext -> Bool
mkValidator adminPKH oldState action ctx =
    let info     = scriptContextTxInfo ctx
        out      = getContinuingOutput ctx
        newState = case txOutDatum out of
                     OutputDatum (Datum d) -> case fromBuiltinData d of
                                                 Just s  -> s
                                                 Nothing -> traceError "Invalid output datum"
                     _                     -> traceError "Output datum missing"
    in case action of
        UpdateProject p b ->
            traceIfFalse "Not signed by admin" (txSignedBy info adminPKH) &&
            traceIfFalse "Invalid project update" (Map.insert p b (projects oldState) == projects newState)
        CheckProject p ->
            case Map.lookup p (projects oldState) of
                Just status -> status
                Nothing     -> False

{-# INLINABLE wrapValidator #-}
wrapValidator :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapValidator adminPKH datum redeemer context =
    let oldState  = unsafeFromBuiltinData datum   :: PRPState
        action    = unsafeFromBuiltinData redeemer :: PRPAction
        scriptCtx = unsafeFromBuiltinData context  :: ScriptContext
    in if mkValidator adminPKH oldState action scriptCtx
          then ()
          else traceError "Plastic Recovery Projects validation failed"

------------------------------------------------------------
-- Validator Script
------------------------------------------------------------

-- | Create a parameterized validator with a fixed admin PubKeyHash
validator :: PubKeyHash -> Validator
validator adminPKH = mkValidatorScript $
    $$(compile [|| wrapValidator ||])
    `applyCode`
    liftCode adminPKH

writePlutusScript :: FilePath -> PubKeyHash -> IO ()
writePlutusScript file adminPKH = do
  createDirectoryIfMissing True (takeDirectory file)
  result <- writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing $
              PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise $
                unValidatorScript (validator adminPKH)
  case result of
    Left err -> print $ displayError err
    Right () -> putStrLn "Successfully wrote parameterized Plutus script to file."

writeScript :: PubKeyHash -> IO ()
writeScript adminPKH = writePlutusScript "src/PlastikV2/PlastikRecoveryProjects.plutus" adminPKH
