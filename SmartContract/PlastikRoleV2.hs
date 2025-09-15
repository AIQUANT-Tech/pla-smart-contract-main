{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}

module Plastik.PlastikRoleV2 where

import           Plutus.V2.Ledger.Api       (BuiltinData, ScriptContext(..), TxInfo, Validator,
                                            mkValidatorScript, PubKeyHash(..), Datum(..),
                                            TxOut(..), scriptContextTxInfo,  
                                            OutputDatum(..), 
                                            txOutDatum, unValidatorScript, 
                                            fromBuiltinData, toBuiltinData)
import           PlutusTx
import           PlutusTx.Prelude           hiding (Semigroup(..), unless, elem)
import           Cardano.Api                (PlutusScript(..), PlutusScriptV2, FileError(..), writeFileTextEnvelope)
import           Cardano.Api.Shelley        
import           System.IO                  (hPutStrLn, stderr)
import           System.Directory           (createDirectoryIfMissing)
import           System.FilePath            (takeDirectory)
import           Prelude                    (FilePath, IO, Show (show), putStrLn, getLine, print)
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy       as LBS
import           Codec.Serialise            (serialise)
import Plutus.V2.Ledger.Contexts (scriptContextTxInfo, ownCurrencySymbol, txSignedBy, TxOut(..))
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LBS
import           Plutus.V2.Ledger.Contexts (txInfoSignatories, scriptContextTxInfo, txInfoOutputs, txOutDatum)

-- | Datum storing the list of minters
data RoleDatum = RoleDatum { minters :: [PubKeyHash] }
    deriving Show

PlutusTx.unstableMakeIsData ''RoleDatum

-- | Redeemer defining actions
data RoleRedeemer = GrantMinter PubKeyHash | VerifyMinter PubKeyHash
    deriving Show

PlutusTx.unstableMakeIsData ''RoleRedeemer

-- | RoleParams data type for script configuration
data RoleParams = RoleParams
    { admin :: PubKeyHash
    }

PlutusTx.unstableMakeIsData ''RoleParams

-- | Helper function to check if an element exists in a list
{-# INLINABLE customElem #-}
customElem :: Eq a => a -> [a] -> Bool
customElem x xs = any (x ==) xs

-- | Validator function
{-# INLINABLE mkPlastikRoleValidator #-}
mkPlastikRoleValidator :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkPlastikRoleValidator adminBuiltin datumBuiltin redeemerBuiltin ctxBuiltin =
    case (fromBuiltinData adminBuiltin, fromBuiltinData datumBuiltin, fromBuiltinData redeemerBuiltin, fromBuiltinData ctxBuiltin) of
        (Just adminParam, Just datum, Just redeemer, Just ctx) -> 
            if checkValidator adminParam datum redeemer ctx 
            then () 
            else traceError "Validation failed"
        _ -> traceError "Invalid data"

-- | Main validation logic
checkValidator :: PubKeyHash -> RoleDatum -> RoleRedeemer -> ScriptContext -> Bool
checkValidator adminParam datum redeemer ctx =
    case redeemer of
        GrantMinter newMinter ->
            traceIfFalse "ADMIN_SIGNATURE_MISSING" (txSignedBy info adminParam) &&
            traceIfFalse "MINTER_EXISTS" (not $ customElem newMinter currentMinters) &&
            traceIfFalse "UPDATE_Minter_MISSING" (customElem newMinter updatedMinters) 
        
        VerifyMinter pk ->
            traceIfFalse "NOT_AUTHORIZED" (customElem pk currentMinters)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    currentMinters :: [PubKeyHash]
    currentMinters = minters datum

    updatedMinters :: [PubKeyHash]
    updatedMinters = concatMap extractMinters (txInfoOutputs info)

    extractMinters :: Plutus.V2.Ledger.Contexts.TxOut -> [PubKeyHash]
    extractMinters o = case txOutDatum o of
        OutputDatum (Datum d) -> case fromBuiltinData d of
                                  Just (RoleDatum ms) -> ms
                                  _                   -> []
        NoOutputDatum         -> []
        OutputDatumHash _     -> []

-- | Create validator script
validator :: PubKeyHash -> Validator
validator adminParam = mkValidatorScript $
    $$(PlutusTx.compile [|| mkPlastikRoleValidator ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode (dataToBuiltinData $ builtinDataToData $ toBuiltinData adminParam)

-- | Serialize Plutus script
writePlutusScript :: FilePath -> Validator -> IO ()
writePlutusScript file validator = do
  createDirectoryIfMissing True (takeDirectory file)
  result <- writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing $
    PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise $
      unValidatorScript validator
  case result of
    Left err -> hPutStrLn stderr $ displayError err
    Right () -> putStrLn "Successfully wrote Plutus script to file."

-- | Get Plutus script serialized
scriptSerialised :: RoleParams -> PlutusScript PlutusScriptV2
scriptSerialised params = PlutusScriptSerialised $
  SBS.toShort $ LBS.toStrict $ serialise $
    unValidatorScript $ validator (admin params)

-- | Function to write serialized script to a file
writeSerialisedScript :: FilePath -> RoleParams -> IO (Either (FileError ()) ())
writeSerialisedScript filePath params = do
    createDirectoryIfMissing True (takeDirectory filePath)
    let serialisedScript = scriptSerialised params
    writeFileTextEnvelope filePath Nothing serialisedScript

-- | Main function to interact with the user
writeScript :: IO ()
writeScript = do
  putStrLn "Enter Admin PubKeyHash (hex-encoded):"
  rid <- getLine
  putStrLn "Enter output file path (e.g., Plastik/PlastikRoleV2.plutus):"
  outFile <- getLine

  case B16.decode (C8.pack rid) of
    Right recipientBytes -> do
      let adminPKH = PubKeyHash (toBuiltin recipientBytes)
      writePlutusScript outFile (validator adminPKH)
    Left _ -> putStrLn "Error: Invalid hex input for admin PubKeyHash."

-- | Main entry point
main :: IO ()
main = writeScript
