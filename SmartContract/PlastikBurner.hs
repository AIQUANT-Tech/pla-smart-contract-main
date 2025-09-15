{-# LANGUAGE BlockArguments #-}
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
 
module Plastik.PlastikBurner where
 
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2, displayError, writeFileTextEnvelope)
import Codec.Serialise (serialise)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base16 as B16
import GHC.Generics (Generic)
import qualified Plutus.V1.Ledger.Value as Value
import Plutus.V2.Ledger.Api
  ( Address (..),
    Credential (..),
    Datum (..),
    PubKeyHash(..),
    ScriptContext,
    OutputDatum (..),
    TxOut (..),
    Validator,
    Value,
    getValue,
    mkValidatorScript,
    unValidatorScript,
    unsafeFromBuiltinData,
  )
import Plutus.V2.Ledger.Contexts (scriptContextTxInfo, txInfoOutputs)
import PlutusTx (Data (..), compile)
import qualified PlutusTx
import PlutusTx.Builtins (divideInteger, equalsInteger)
import PlutusTx.Builtins.Internal (BuiltinByteString, BuiltinData)
import PlutusTx.Prelude hiding (Semigroup (..), check, unless)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Prelude (FilePath, IO, Show, show, String, print, putStrLn, getLine)
 
--------------------------------------------------------------------------------
-- On-Chain State (Datum) for PlastikBurner (Parameterized)
--------------------------------------------------------------------------------
 
data BurnerState = BurnerState
  {  
    tokenName         :: BuiltinByteString,
    policyId          :: BuiltinByteString,
    totalSupply       :: Integer,
    year              :: Integer,
    maxBurnYear       :: Integer,
    plasticProduction :: Integer,
    burnPerKg         :: Integer  -- Scaled value (multiplied by 10^9)
  }
  deriving (Show, Generic)
 
PlutusTx.unstableMakeIsData ''BurnerState
 
--------------------------------------------------------------------------------
-- Redeemer Actions for PlastikBurner
--------------------------------------------------------------------------------
 
data BurnerRedeemer 
  = CurrentBurnRate PubKeyHash
  | SetPlasticProduction PubKeyHash Integer
  | SetMaxBurnYear PubKeyHash Integer
  deriving (Show, Generic)
 
PlutusTx.unstableMakeIsData ''BurnerRedeemer
 
--------------------------------------------------------------------------------
-- Parameterized Validator Logic
--------------------------------------------------------------------------------
 
{-# INLINEABLE safeDiv #-}
safeDiv :: Integer -> Integer -> Integer
safeDiv x y =
  if y == 0
    then 0
    else (x * 1000000000) `divide` y -- Scale by 1e9
 
{-# INLINABLE getContinuingOutput #-}
getContinuingOutput :: ScriptContext -> TxOut
getContinuingOutput ctx =
    case [ o | o <- txInfoOutputs (scriptContextTxInfo ctx)
             , case txOutDatum o of
                 OutputDatum _ -> True
                 _             -> False
           ] of
      [o] -> o
      _   -> traceError "Expected exactly only one continuing output....."
 
{-# INLINEABLE currentBurnRate #-}
currentBurnRate :: PubKeyHash -> BurnerState -> PubKeyHash -> Integer
currentBurnRate ownerParam s caller =
  if (caller == ownerParam)
    then if totalSupply s < maxBurnYear s
      then burnPerKg s
      else 0
  else traceError "Owner is not the Caller of the Contract......"
 
{-# INLINEABLE setPlasticProduction #-}
setPlasticProduction :: PubKeyHash -> BurnerState -> PubKeyHash -> Integer -> Maybe BurnerState
setPlasticProduction ownerParam s caller newProduction =
  if caller == ownerParam && newProduction > 0
    then Just s
      { plasticProduction = newProduction,
        burnPerKg = safeDiv (maxBurnYear s) newProduction
      }
    else Nothing
 
{-# INLINEABLE setMaxBurnYear #-}
setMaxBurnYear :: PubKeyHash -> BurnerState -> PubKeyHash -> Integer -> Maybe BurnerState
setMaxBurnYear ownerParam s caller newMaxBurn =
  if caller == ownerParam && newMaxBurn > 0
    then Just s
      { maxBurnYear = newMaxBurn,
        burnPerKg = safeDiv newMaxBurn (plasticProduction s)
      }
    else Nothing
 
{-# INLINEABLE mkBurnerValidator #-}
mkBurnerValidator :: PubKeyHash -> BuiltinByteString -> BuiltinByteString -> BurnerState -> BurnerRedeemer -> ScriptContext -> Bool
mkBurnerValidator ownerParam _ _ datum redeemer ctx =
  let info = scriptContextTxInfo ctx
   in case redeemer of
        CurrentBurnRate caller ->
          let out = getContinuingOutput ctx
              newDatum = case txOutDatum out of
                            OutputDatum (Datum d) -> case PlutusTx.fromBuiltinData d of
                                                        Just s  -> s
                                                        Nothing -> traceError "Invalid output datum"
                            _ -> traceError "Output datum is missing from ...."
              rate = currentBurnRate ownerParam datum caller
           in (rate == burnPerKg newDatum)
        SetPlasticProduction caller newProd ->
          let out = getContinuingOutput ctx
              newDatum = case txOutDatum out of
                          OutputDatum (Datum d) -> case PlutusTx.fromBuiltinData d of
                                                      Just s  -> s
                                                      Nothing -> traceError "Invalid output datum format"
                          _ -> traceError "Output datum missing - expected inline datum"
            in case setPlasticProduction ownerParam datum caller newProd of
                Just newDatum' -> 
                  let 
                      burnYearCheck = traceIfFalse 
                        "MaxBurnYear modified Incorrectly" 
                        (maxBurnYear newDatum' == maxBurnYear newDatum)
                    
                      burnRateCheck = traceIfFalse 
                        "BurnPerKg modified without changing plastic production" 
                        (burnPerKg newDatum' == burnPerKg newDatum)
                  in burnYearCheck && burnRateCheck
              
                Nothing -> 
                  trace "SetPlasticProduction failed - invalid parameters or permissions" False
        SetMaxBurnYear caller newMax ->
          let out = getContinuingOutput ctx
              newDatum = case txOutDatum out of
                            OutputDatum (Datum d) -> case PlutusTx.fromBuiltinData d of
                                                        Just s  -> s
                                                        Nothing -> traceError "Invalid output datum"
                            _ -> traceError "Output datum is missing"
           in case setMaxBurnYear ownerParam datum caller newMax of
                Just newDatum' -> (plasticProduction newDatum' == plasticProduction newDatum) && (burnPerKg newDatum' == burnPerKg newDatum)
                Nothing -> False
 
--------------------------------------------------------------------------------
-- Wrapping Parameterized Validator
--------------------------------------------------------------------------------
 
{-# INLINEABLE wrapValidator #-}
wrapValidator :: PubKeyHash -> BuiltinByteString -> BuiltinByteString -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapValidator ownerParam tokenNameParam policyIdParam datum redeemer context =
  let st = unsafeFromBuiltinData datum :: BurnerState
      rd = unsafeFromBuiltinData redeemer :: BurnerRedeemer
      ctx = unsafeFromBuiltinData context :: ScriptContext
   in if mkBurnerValidator ownerParam tokenNameParam policyIdParam st rd ctx
        then ()
        else traceError "Plastik burn validation failed..."
 
validator :: PubKeyHash -> BuiltinByteString -> BuiltinByteString -> Validator
validator ownerParam tokenNameParam policyIdParam = mkValidatorScript $
    $$(compile [|| wrapValidator ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode ownerParam
    `PlutusTx.applyCode`
    PlutusTx.liftCode tokenNameParam
    `PlutusTx.applyCode`
    PlutusTx.liftCode policyIdParam

saveRedeemValidator :: IO ()
saveRedeemValidator = do
  putStrLn "Enter admin PubKeyHash (hex):"
  pkhHex <- getLine
  putStrLn "Enter token name (hex):"
  tokenNameHex <- getLine
  putStrLn "Enter policy ID (hex):"
  policyIdHex <- getLine
  putStrLn "Enter output file path:"
  outPath <- getLine

  case (B16.decode (C8.pack pkhHex), B16.decode (C8.pack tokenNameHex), B16.decode (C8.pack policyIdHex)) of
    (Right rawPkh, Right rawTokenName, Right rawPolicyId) -> do
      let adminPkh = PubKeyHash (toBuiltin rawPkh)
          tokenNameParam = toBuiltin rawTokenName
          policyIdParam = toBuiltin rawPolicyId
          script = validator adminPkh tokenNameParam policyIdParam
          serializedScript = serialise script
          shortBS = SBS.toShort . LBS.toStrict $ serializedScript
          plutusScript = PlutusScriptSerialised shortBS

      createDirectoryIfMissing True (takeDirectory outPath)
      result <- writeFileTextEnvelope @(PlutusScript PlutusScriptV2) outPath Nothing plutusScript
      case result of
        Left err -> print $ displayError err
        Right () -> putStrLn "Successfully wrote PlastikBurner validator."
    (Left err, _, _) -> putStrLn $ "Invalid PubKeyHash hex: " ++ show err
    (_, Left err, _) -> putStrLn $ "Invalid token name hex: " ++ show err
    (_, _, Left err) -> putStrLn $ "Invalid policy ID hex: " ++ show err
 
main :: IO ()
main = saveRedeemValidator
 