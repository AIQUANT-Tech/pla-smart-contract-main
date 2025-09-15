{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}
 
module PlastikToken.PlastikTokenMintingPolicy (policy, writeScript, main) where
 
-- Imports
import GHC.Generics (Generic)
import Plutus.V2.Ledger.Api
  ( MintingPolicy
  , mkMintingPolicyScript
  , ScriptContext
  , TxInfo(..)
  , PubKeyHash(..)
  , unMintingPolicyScript
  , BuiltinData
  )
import Plutus.V2.Ledger.Contexts
  ( txSignedBy
  , ownCurrencySymbol
  , scriptContextTxInfo
  )
import Plutus.V1.Ledger.Value (flattenValue, TokenName(TokenName))
import PlutusTx
  ( compile
  , applyCode
  , liftCode
  , unsafeFromBuiltinData
  , ToData
  , FromData
  , makeLift
  )
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import PlutusTx.Builtins (toBuiltin)
import Codec.Serialise (serialise)
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Prelude (IO, putStrLn, getLine, FilePath, print, (<>))
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised), displayError, writeFileTextEnvelope)
import Cardano.Api (PlutusScriptV2)
 
-- On-chain: PlastikToken minting policy
 
newtype PlastikParams = PlastikParams PubKeyHash
  deriving stock Generic
  deriving anyclass (ToData, FromData)
 
makeLift ''PlastikParams
 
{-# INLINEABLE mkPlastikPolicy #-}
mkPlastikPolicy :: PlastikParams -> () -> ScriptContext -> Bool
mkPlastikPolicy (PlastikParams pkh) _ ctx =
  if not (txSignedBy info pkh) then traceError "unauthorized"
  else case flattenValue (txInfoMint info) of
    [(cs, tn, amt)]
      | cs == ownCurrencySymbol ctx && tn == TokenName "PlastikToken" && amt > 0 -> True
      | otherwise -> traceError "invalid amount or token"
    _ -> traceError "invalid mint"
  where
    info = scriptContextTxInfo ctx
 
{-# INLINEABLE plastikUntyped #-}
plastikUntyped :: PlastikParams -> BuiltinData -> BuiltinData -> ()
plastikUntyped params _ ctxData =
  let ctx = unsafeFromBuiltinData ctxData
  in if mkPlastikPolicy params () ctx then () else traceError "Policy failed"
 
policy :: PubKeyHash -> MintingPolicy
policy pkh = mkMintingPolicyScript $
  $$(compile [|| \p -> plastikUntyped p ||])
    `applyCode` liftCode (PlastikParams pkh)
 
-- Off-chain: Write Plutus minting policy to file
writePlutusMintingPolicy :: FilePath -> MintingPolicy -> IO ()
writePlutusMintingPolicy file mp = do
  createDirectoryIfMissing True (takeDirectory file)
  let script = serialise (unMintingPolicyScript mp)
      shortScript = SBS.toShort (LBS.toStrict script)
  result <- writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing (PlutusScriptSerialised shortScript)
  case result of
    Left err -> print $ displayError err
    Right () -> putStrLn $ "Successfully wrote Plutus minting policy to: " <> file
 
-- Function to interactively write the policy script
writeScript :: IO ()
writeScript = do
  putStrLn "Enter the public key hash (hex):"
  pkhHex <- getLine
  putStrLn "Enter the output file path (e.g., scripts/PlastikToken.plutus):"
  outFile <- getLine
  case B16.decode (C8.pack pkhHex) of
    Right bytes -> do
      let pkh = PubKeyHash (toBuiltin bytes)
      writePlutusMintingPolicy outFile (policy pkh)
    Left _ -> putStrLn "Error: Invalid hex input for PubKeyHash."
 
-- CLI entry point
main :: IO ()
main = writeScript
 
 