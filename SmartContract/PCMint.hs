{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}

module Plastik.PCMint (policy, nftCode, nftPolicy, savePolicy, main) where

-- Imports
import Plutus.V2.Ledger.Api
  ( MintingPolicy
  , ScriptContext (..)
  , TokenName
  , TxId (..)
  , TxInfo (..)
  , TxInInfo (..)
  , TxOutRef (..)
  , PubKeyHash (..)
  , mkMintingPolicyScript
  , BuiltinData
  , unsafeFromBuiltinData
  , toBuiltin
  , unMintingPolicyScript
  , TokenName (unTokenName)
  )
import qualified Data.ByteString.Char8      as BS8
import           Plutus.V1.Ledger.Value     (flattenValue)
import           Plutus.V2.Ledger.Api       (BuiltinData, CurrencySymbol,
                                             MintingPolicy,
                                             ScriptContext (scriptContextTxInfo),
                                             TokenName (unTokenName),
                                             TxId (TxId, getTxId),
                                             TxInInfo (txInInfoOutRef),
                                             TxInfo (txInfoInputs, txInfoMint),
                                             TxOutRef (TxOutRef, txOutRefId, txOutRefIdx),
                                             mkMintingPolicyScript)
import qualified PlutusTx
import           PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import           PlutusTx.Prelude           (Bool (False), Eq ((==)), any,
                                             traceIfFalse, ($), (&&))
import           Prelude                    (IO, Show (show), String)
import Plutus.V1.Ledger.Value (flattenValue)
import PlutusTx
  ( applyCode
  , compile
  , liftCode
  )
import System.FilePath (takeDirectory)
import PlutusTx.Prelude
  ( Bool(..)
  , traceIfFalse
  , (==)
  , ($)
  , any
  , (&&)
  , elem
  , error
  )
import System.Environment (getArgs)
import System.FilePath (takeDirectory)
import Prelude (IO, Show (..), String, putStrLn, read)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LBS
import Codec.Serialise (serialise)
import System.Directory (createDirectoryIfMissing)
import Text.Printf (printf)
import System.Directory (createDirectoryIfMissing)
import GHC.Generics (Generic)
import Plutus.V2.Ledger.Api
  ( MintingPolicy
  , mkMintingPolicyScript
  , ScriptContext
  , TxInfo(..)
  , unMintingPolicyScript
  , BuiltinData
  , TxOutRef
  , TokenName(..)
  , CurrencySymbol
  , PubKeyHash(..)
  )
import Plutus.V2.Ledger.Contexts (scriptContextTxInfo, ownCurrencySymbol, txSignedBy, TxOut(..))
import Plutus.V1.Ledger.Value (flattenValue)
import PlutusTx
  ( compile
  , applyCode
  , liftCode
  , unsafeFromBuiltinData
  , makeLift
  , FromData
  , ToData
  , unstableMakeIsData
  , toBuiltinData
  )
import PlutusTx.Prelude hiding (Semigroup(..))
import Codec.Serialise (serialise)
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as C8
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Prelude (IO, putStrLn, getLine, FilePath, print, (<>), Show, read)
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised), displayError, writeFileTextEnvelope)
import Cardano.Api (PlutusScriptV2)
import PlutusTx.Prelude (Bool (..), Integer, Maybe (..), traceError, traceIfFalse, (&&), (>), (>=), (<=), (==))
import PlutusTx.Prelude (Bool (..), fromBuiltin, Either (..), Eq (..), Integer, Maybe (..), any, divide, toBuiltin, traceError, traceIfFalse, ($), (&&), (*), (+), (/=), (<=), (==), (>), filter)
import Cardano.Api (PlutusScriptV2, writeFileTextEnvelope)
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised), displayError)

--------------------------------------------------------------------------------
-- Minting Logic
--------------------------------------------------------------------------------

{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: PubKeyHash -> TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkNFTPolicy pkh oref nftName () ctx =
  traceIfFalse "UTxO not consumed"   hasUTxO &&
  traceIfFalse "Wrong amount minted" checkMintedAmount &&
  traceIfFalse "Unauthorized minter" signedByPKH
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
      [(_, tn', amt)] -> tn' == nftName && amt == 1
      _               -> False

    signedByPKH :: Bool
    signedByPKH = pkh `elem` txInfoSignatories info

--------------------------------------------------------------------------------
-- Script Wrapper
--------------------------------------------------------------------------------

{-# INLINABLE mkWrappedNFTPolicy #-}
mkWrappedNFTPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData
                   -> BuiltinData -> ()
mkWrappedNFTPolicy pkh' tid ix nftName' _ ctx =
  let pkh      = unsafeFromBuiltinData pkh'
      oref     = TxOutRef (TxId (unsafeFromBuiltinData tid)) (unsafeFromBuiltinData ix)
      nftName  = unsafeFromBuiltinData nftName'
      context  = unsafeFromBuiltinData ctx
  in if mkNFTPolicy pkh oref nftName () context
        then ()
        else error ()


nftCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
nftCode = $$(compile [|| mkWrappedNFTPolicy ||])

nftPolicy :: PubKeyHash -> TxOutRef -> TokenName -> MintingPolicy
nftPolicy pkh oref nftName =
  mkMintingPolicyScript $
    nftCode
      `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData pkh)
      `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ getTxId $ txOutRefId oref)
      `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ txOutRefIdx oref)
      `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData nftName)

--------------------------------------------------------------------------------
-- Script Utilities
--------------------------------------------------------------------------------

policy :: PubKeyHash -> TxOutRef -> TokenName -> MintingPolicy
policy = nftPolicy

writePlutusMintingPolicy :: FilePath -> MintingPolicy -> IO ()
writePlutusMintingPolicy file mp = do
  let script :: PlutusScript PlutusScriptV2
      script = PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise $ unMintingPolicyScript mp
  putStrLn "Serialisation complete. Writing to file..."
  result <- writeFileTextEnvelope file Nothing script
  case result of
    Left err -> do
      putStrLn $ "Error writing script: " ++ displayError err
    Right () -> do
      putStrLn $ "Policy written successfully to: " ++ file



savePolicy :: PubKeyHash -> TxOutRef -> TokenName -> IO ()
savePolicy pkh oref nftName = do
  let txidHex = C8.unpack $ B16.encode $ fromBuiltin $ getTxId $ txOutRefId oref
      txix    = show $ txOutRefIdx oref
      nameHex = C8.unpack $ B16.encode $ C8.pack $ show $ unTokenName nftName
      pkhHex  = C8.unpack $ B16.encode $ fromBuiltin $ getPubKeyHash pkh
      file    = printf "Plastik/PCMint-%s-%s-%s-%s.plutus" txidHex txix nameHex pkhHex
  createDirectoryIfMissing True (takeDirectory file)
  writePlutusMintingPolicy file (nftPolicy pkh oref nftName)
  putStrLn $ "Policy saved to: " ++ file

--------------------------------------------------------------------------------
-- CLI Entrypoint
--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    [pkhHex, txidHex, txixStr, nftHex] ->
      case (B16.decode (C8.pack pkhHex), B16.decode (C8.pack txidHex), B16.decode (C8.pack nftHex)) of
        (Right pkhBytes, Right txidBytes, Right nftBytes) -> do
          let pkh      = PubKeyHash $ toBuiltin pkhBytes
              txid     = TxId $ toBuiltin txidBytes
              txix     = read txixStr :: Integer
              nftName  = TokenName $ toBuiltin nftBytes
              oref     = TxOutRef txid txix
          savePolicy pkh oref nftName
        _ -> putStrLn "Error: Invalid hex input for PubKeyHash, TxId or NFT Name."
    _ -> putStrLn "Usage: <pubKeyHash-hex> <txid-hex> <txix> <nftName-hex>"