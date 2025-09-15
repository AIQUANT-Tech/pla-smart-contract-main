{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE NamedFieldPuns        #-}


module Plastik.PCMint (policy, writeScript, main) where

-- Imports
import GHC.Generics (Generic)
import Plutus.V2.Ledger.Api
  ( MintingPolicy
  , mkMintingPolicyScript
  , ScriptContext
  , TxInfo(..)
  , unMintingPolicyScript
  , BuiltinData
  , TxOutRef
  , TokenName
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
import PlutusTx.Prelude hiding (Semigroup(..), unless)
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
import PlutusTx.Prelude (Bool (..), fromBuiltin, Either (..), Eq (..), Integer, Maybe (..), any, divide, toBuiltin, traceError, traceIfFalse, ($), (&&), (*), (+), (++), (/=), (<=), (==), (>), filter)------------------------------------------------------------------------------
-- | Datum for NFT Minting
------------------------------------------------------------------------------

data NFTDatum = NFTDatum
  { recipient :: PubKeyHash
  , amount      :: Integer
  , nftData     :: BuiltinByteString
  } deriving (Generic, Show)

unstableMakeIsData ''NFTDatum
makeLift ''NFTDatum

------------------------------------------------------------------------------
-- | On-Chain Minting Logic
------------------------------------------------------------------------------

{-# INLINEABLE mkPlastikCreditNFTPolicy #-}
mkPlastikCreditNFTPolicy :: NFTDatum -> () -> ScriptContext -> Bool
mkPlastikCreditNFTPolicy NFTDatum{recipient, amount} _ ctx =
  traceIfFalse "Incorrect number of NFTs minted" checkMinted &&
  traceIfFalse "Missing Required Signature from recipient" signedByRecipient
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownCS :: CurrencySymbol
    ownCS = ownCurrencySymbol ctx

    checkMinted :: Bool
    checkMinted =
      let minted = flattenValue $ txInfoMint info
          matching = filter (\(cs, _, amt) -> cs == ownCS && amt == 1) minted
      in length matching == amount

    signedByRecipient :: Bool
    signedByRecipient = txSignedBy info recipient

------------------------------------------------------------------------------
-- | Untyped Policy Wrapper
------------------------------------------------------------------------------

{-# INLINEABLE untyped #-}
untyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
untyped datum _ ctx =
  case unsafeFromBuiltinData datum of
    d -> if mkPlastikCreditNFTPolicy d () (unsafeFromBuiltinData ctx)
         then ()
         else traceError "Policy check failed"

------------------------------------------------------------------------------
-- | Compile the Minting Policy
------------------------------------------------------------------------------

policy :: NFTDatum -> MintingPolicy
policy datum = mkMintingPolicyScript $
  $$(compile [|| \d -> untyped d ||]) `applyCode` liftCode (toBuiltinData datum)

------------------------------------------------------------------------------
-- | Off-Chain Script Writer
------------------------------------------------------------------------------

writePlutusMintingPolicy :: FilePath -> MintingPolicy -> IO ()
writePlutusMintingPolicy file mp = do
  createDirectoryIfMissing True (takeDirectory file)
  let script = serialise (unMintingPolicyScript mp)
      shortScript = SBS.toShort (LBS.toStrict script)
  result <- writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing (PlutusScriptSerialised shortScript)
  case result of
    Left err -> print $ displayError err
    Right () -> putStrLn $ "Successfully wrote NFT policy to: " <> file

------------------------------------------------------------------------------
-- | CLI Tool: Write Script Interactively
------------------------------------------------------------------------------

writeScript :: IO ()
writeScript = do
  putStrLn "Enter recipient PubKeyHash (hex-encoded):"
  rid <- getLine
  putStrLn "Enter amount (how many NFTs to mint):"
  amtStr <- getLine
  putStrLn "Enter nftData (hex-encoded, e.g., 436172626f6e):"
  desc <- getLine
  putStrLn "Enter output file path (e.g., Plastik/PCMint.plutus):"
  outFile <- getLine

  case (B16.decode (C8.pack rid), B16.decode (C8.pack desc)) of
    (Right recipientBytes, Right dataBytes) -> do
      let recipientHex =  PubKeyHash (toBuiltin recipientBytes)
          dataHex      = toBuiltin dataBytes
          amt          = read amtStr :: Integer
          datum        = NFTDatum recipientHex amt dataHex
      writePlutusMintingPolicy outFile (policy datum)
    _ -> putStrLn "Error: Invalid hex input for recipientId or nftData."

------------------------------------------------------------------------------
-- | Main Entrypoint
------------------------------------------------------------------------------

main :: IO ()
main = writeScript



-- {-# LANGUAGE DataKinds             #-}
-- {-# LANGUAGE NoImplicitPrelude     #-}
-- {-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE TemplateHaskell       #-}
-- {-# LANGUAGE ScopedTypeVariables   #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TypeApplications      #-}
-- {-# LANGUAGE DeriveAnyClass        #-}
-- {-# LANGUAGE DeriveGeneric         #-}

-- module Plastik.PCMint (policy, nftCode, nftPolicy, savePolicy, main) where

-- -- Imports
-- import Plutus.V2.Ledger.Api
--   ( MintingPolicy
--   , ScriptContext (..)
--   , TokenName
--   , TxId (..)
--   , TxInfo (..)
--   , TxInInfo (..)
--   , TxOutRef (..)
--   , PubKeyHash (..)
--   , CurrencySymbol
--   , mkMintingPolicyScript
--   , BuiltinData
--   , unsafeFromBuiltinData
--   , toBuiltin
--   , unMintingPolicyScript
--   )
-- import Plutus.V1.Ledger.Value (flattenValue)
-- import PlutusTx
--   ( applyCode
--   , compile
--   , liftCode
--   , makeLift
--   )
-- import PlutusTx.Prelude
--   ( Bool(..)
--   , traceIfFalse
--   , (==)
--   , ($)
--   , any
--   , (&&)
--   , elem
--   , error
--   )
-- import Prelude (IO, Show (..), String, putStrLn, getArgs, read, (++))
-- import qualified Data.ByteString.Char8 as C8
-- import qualified Data.ByteString.Base16 as B16
-- import qualified Data.ByteString.Short as SBS
-- import qualified Data.ByteString.Lazy as LBS
-- import Codec.Serialise (serialise)
-- import System.Directory (createDirectoryIfMissing)
-- import System.FilePath (takeDirectory)
-- import Text.Printf (printf)

-- --------------------------------------------------------------------------------
-- -- Minting Logic
-- --------------------------------------------------------------------------------

-- {-# INLINABLE mkNFTPolicy #-}
-- mkNFTPolicy :: PubKeyHash -> TxOutRef -> TokenName -> () -> ScriptContext -> Bool
-- mkNFTPolicy pkh oref tn () ctx =
--   traceIfFalse "UTxO not consumed"   hasUTxO &&
--   traceIfFalse "Wrong amount minted" checkMintedAmount &&
--   traceIfFalse "Unauthorized minter" signedByPKH
--   where
--     info :: TxInfo
--     info = scriptContextTxInfo ctx

--     hasUTxO :: Bool
--     hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

--     checkMintedAmount :: Bool
--     checkMintedAmount = case flattenValue (txInfoMint info) of
--       [(_, tn', amt)] -> tn' == tn && amt == 1
--       _               -> False

--     signedByPKH :: Bool
--     signedByPKH = pkh `elem` txInfoSignatories info

-- --------------------------------------------------------------------------------
-- -- Script Wrapper
-- --------------------------------------------------------------------------------

-- {-# INLINABLE mkWrappedNFTPolicy #-}
-- mkWrappedNFTPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
-- mkWrappedNFTPolicy pkh' tid ix tn' _ _ =
--   let pkh  = unsafeFromBuiltinData pkh'
--       oref = TxOutRef (TxId (unsafeFromBuiltinData tid)) (unsafeFromBuiltinData ix)
--       tn   = unsafeFromBuiltinData tn'
--   in if mkNFTPolicy pkh oref tn () (unsafeFromBuiltinData undefined)
--         then ()
--         else error ()

-- nftCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
-- nftCode = $$(compile [|| mkWrappedNFTPolicy ||])

-- nftPolicy :: PubKeyHash -> TxOutRef -> TokenName -> MintingPolicy
-- nftPolicy pkh oref tn =
--   mkMintingPolicyScript $
--     nftCode
--       `applyCode` liftCode pkh
--       `applyCode` liftCode (toBuiltin $ getTxId $ txOutRefId oref)
--       `applyCode` liftCode (toBuiltin $ txOutRefIdx oref)
--       `applyCode` liftCode (toBuiltin tn)

-- --------------------------------------------------------------------------------
-- -- Script Utilities
-- --------------------------------------------------------------------------------

-- policy :: PubKeyHash -> TxOutRef -> TokenName -> MintingPolicy
-- policy = nftPolicy

-- writePlutusMintingPolicy :: FilePath -> MintingPolicy -> IO ()
-- writePlutusMintingPolicy file mp = do
--   createDirectoryIfMissing True (takeDirectory file)
--   let script = serialise (unMintingPolicyScript mp)
--       shortScript = SBS.toShort (LBS.toStrict script)
--   LBS.writeFile file (LBS.fromStrict $ SBS.fromShort shortScript)
--   putStrLn $ "Policy written to: " ++ file

-- savePolicy :: PubKeyHash -> TxOutRef -> TokenName -> IO ()
-- savePolicy pkh oref tn = do
--   let txidHex = C8.unpack $ B16.encode $ getTxId $ txOutRefId oref
--       txix    = show $ txOutRefIdx oref
--       tnHex   = C8.unpack $ B16.encode $ toBuiltin tn
--       pkhHex  = C8.unpack $ B16.encode $ toBuiltin pkh
--       file    = printf "assets/nft-%s#%s-%s-%s.plutus" txidHex txix tnHex pkhHex
--   writePlutusMintingPolicy file (nftPolicy pkh oref tn)

-- --------------------------------------------------------------------------------
-- -- CLI Entrypoint
-- --------------------------------------------------------------------------------

-- main :: IO ()
-- main = do
--   args <- getArgs
--   case args of
--     [pkhHex, txidHex, txixStr, tokenHex] ->
--       case (B16.decode (C8.pack pkhHex), B16.decode (C8.pack txidHex), B16.decode (C8.pack tokenHex)) of
--         (Right (pkhBytes, _), Right (txidBytes, _), Right (tokenBytes, _)) -> do
--           let pkh  = PubKeyHash $ toBuiltin pkhBytes
--               txid = TxId $ toBuiltin txidBytes
--               txix = read txixStr :: Integer
--               tn   = TokenName $ toBuiltin tokenBytes
--               oref = TxOutRef txid (fromInteger txix)
--           savePolicy pkh oref tn
--         _ -> putStrLn "Error: Invalid hex input for PubKeyHash, TxId or TokenName."
--     _ -> putStrLn "Usage: <pubKeyHash-hex> <txid-hex> <txix> <tokenName-hex>"
