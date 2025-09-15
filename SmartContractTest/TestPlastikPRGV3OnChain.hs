{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Plastik.Test.TestPlastikPRGV3OnChain where

import Control.Applicative (Applicative (..), (<$>), (<*>))
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Plutus.V2.Ledger.Api (PubKeyHash (..), ScriptContext, Validator, mkValidatorScript)
import PlutusTx
import PlutusTx.AssocMap as Map (Map, empty, fromList, insert, lookup, member)
import qualified PlutusTx.Builtins as Builtins
import PlutusTx.Prelude hiding (Semigroup (..), check, fromEnum, unless)
import Test.QuickCheck
import Test.QuickCheck hiding ((===))
import Test.QuickCheck.Instances ()
import Prelude (Functor (..), IO, Show, print, putStrLn, undefined)

------------------------------------------------------------
-- On-Chain Data Types
------------------------------------------------------------

data AttachKey = AttachKey Integer PubKeyHash Integer
  deriving (Show, Generic)

PlutusTx.unstableMakeIsData ''AttachKey

instance Eq AttachKey where
  {-# INLINEABLE (==) #-}
  (AttachKey tid1 addr1 nft1) == (AttachKey tid2 addr2 nft2) =
    tid1 == tid2 && addr1 == addr2 && nft1 == nft2

deriving instance Ord AttachKey

data PRGState = PRGState
  { verifiedAccounts :: [PubKeyHash],
    whiteList :: [PubKeyHash],
    tokenURIs :: Map Integer BuiltinByteString,
    totalSupply :: Map Integer Integer,
    attachedTokens :: Map AttachKey Integer
  }
  deriving (Show, Eq, Generic)

PlutusTx.unstableMakeIsData ''PRGState

data PRGInput
  = MintToken
      { ipRecipient :: PubKeyHash,
        ipTokenId :: Integer,
        ipAmount :: Integer,
        ipTokenURI :: BuiltinByteString
      }
  | LazyMint
      { ipBuyer :: PubKeyHash,
        ipVoucherTokenId :: Integer,
        ipVoucherAmount :: Integer,
        ipVoucherCreator :: PubKeyHash,
        ipVoucherURI :: BuiltinByteString
      }
  | AttachPRG
      { ipSustainableUser :: PubKeyHash,
        ipTokenId :: Integer,
        ipAttachAmount :: Integer,
        ipNFTAddress :: PubKeyHash,
        ipNFTTokenId :: Integer
      }
  deriving (Show, Eq, Generic)

PlutusTx.unstableMakeIsData ''PRGInput

------------------------------------------------------------
-- Transition Function
------------------------------------------------------------
{-# INLINEABLE transition #-}
transition :: PRGState -> PRGInput -> Maybe PRGState
transition s input = case input of
  MintToken recipient tid amt uri ->
    if recipient `elem` verifiedAccounts s
      then
        let newURIs = Map.insert tid uri (tokenURIs s)
            prevSupply = fromMaybe 0 (Map.lookup tid (totalSupply s))
            newSupply = Map.insert tid (prevSupply + amt) (totalSupply s)
         in Just s {tokenURIs = newURIs, totalSupply = newSupply}
      else Nothing
  LazyMint buyer tid amt creator uri ->
    if creator `elem` verifiedAccounts s
      then
        let newURIs =
              if Map.member tid (tokenURIs s)
                then tokenURIs s
                else Map.insert tid uri (tokenURIs s)
            prevSupply = fromMaybe 0 (Map.lookup tid (totalSupply s))
            newSupply = Map.insert tid (prevSupply + amt) (totalSupply s)
         in Just s {tokenURIs = newURIs, totalSupply = newSupply}
      else Nothing
  AttachPRG _ tid amt nftAddr nftTId ->
    let key = AttachKey tid nftAddr nftTId
        prevAtt = fromMaybe 0 (Map.lookup key (attachedTokens s))
        newAttach = Map.insert key (prevAtt + amt) (attachedTokens s)
     in Just s {attachedTokens = newAttach}

------------------------------------------------------------
-- On-Chain Validator
------------------------------------------------------------
{-# INLINEABLE mkPRGValidator #-}
mkPRGValidator :: PRGState -> PRGInput -> ScriptContext -> Bool
mkPRGValidator datum input _ =
  case transition datum input of
    Just _ -> True
    Nothing -> traceError "Invalid state transition"

{-# INLINEABLE wrapValidator #-}
wrapValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapValidator datum redeemer context =
  let prgState :: PRGState = unsafeFromBuiltinData datum
      prgInput :: PRGInput = unsafeFromBuiltinData redeemer
      scriptContext :: ScriptContext = unsafeFromBuiltinData context
   in if mkPRGValidator prgState prgInput scriptContext
        then ()
        else traceError "Plastic Recovery Projects validation failed"

validator :: Validator
validator = mkValidatorScript $$(compile [||wrapValidator||])

------------------------------------------------------------
-- Test Data
------------------------------------------------------------

alicePKH, bobPKH, charliePKH :: PubKeyHash
alicePKH = PubKeyHash (Builtins.toBuiltin (BSC.pack "Alice"))
bobPKH = PubKeyHash (Builtins.toBuiltin (BSC.pack "Bob"))
charliePKH = PubKeyHash (Builtins.toBuiltin (BSC.pack "Charlie"))

samplePKHs :: [PubKeyHash]
samplePKHs = [alicePKH, bobPKH, charliePKH]

initialState :: PRGState
initialState =
  PRGState
    { verifiedAccounts = [alicePKH],
      whiteList = [],
      tokenURIs = Map.empty,
      totalSupply = Map.empty,
      attachedTokens = Map.empty
    }

------------------------------------------------------------
-- Arbitrary Instances
------------------------------------------------------------

instance Arbitrary AttachKey where
  arbitrary = AttachKey Control.Applicative.<$> arbitrary Control.Applicative.<*> genPubKeyHash Control.Applicative.<*> arbitrary

instance Arbitrary PRGInput where
  arbitrary =
    oneof
      [ genMint,
        genLazyMint,
        genAttach
      ]
    where
      genMint = MintToken Control.Applicative.<$> genPubKeyHash Control.Applicative.<*> arbitrary Control.Applicative.<*> (arbitrary `suchThat` (>= 0)) Control.Applicative.<*> genBuiltinBS
      genLazyMint =
        LazyMint Control.Applicative.<$> genPubKeyHash
          Control.Applicative.<*> arbitrary
          Control.Applicative.<*> (arbitrary `suchThat` (>= 0))
          Control.Applicative.<*> genPubKeyHash
          Control.Applicative.<*> genBuiltinBS
      genAttach =
        AttachPRG Control.Applicative.<$> genPubKeyHash
          Control.Applicative.<*> arbitrary
          Control.Applicative.<*> (arbitrary `suchThat` (>= 0))
          Control.Applicative.<*> genPubKeyHash
          Control.Applicative.<*> arbitrary

instance Arbitrary PRGState where
  arbitrary = do
    verified <- sublistOf samplePKHs
    whiteL <- sublistOf samplePKHs
    tURIs <- Map.fromList Control.Applicative.<$> listOf ((,) Control.Applicative.<$> arbitrary Control.Applicative.<*> genBuiltinBS)
    tSupply <- Map.fromList Control.Applicative.<$> listOf ((,) Control.Applicative.<$> arbitrary Control.Applicative.<*> (arbitrary `suchThat` (>= 0)))
    aTokens <- Map.fromList Control.Applicative.<$> listOf ((,) Control.Applicative.<$> arbitrary Control.Applicative.<*> (arbitrary `suchThat` (>= 0)))
    return
      PRGState
        { verifiedAccounts = verified,
          whiteList = whiteL,
          tokenURIs = tURIs,
          totalSupply = tSupply,
          attachedTokens = aTokens
        }

------------------------------------------------------------
-- Generators for Non-Discarded Tests
------------------------------------------------------------

genMintTokenValid :: Gen (PRGState, PRGInput)
genMintTokenValid = do
  recipient <- elements samplePKHs
  let s = initialState {verifiedAccounts = [recipient]}
  tid <- arbitrary
  amt <- arbitrary `suchThat` (>= 0)
  uri <- genBuiltinBS
  return (s, MintToken recipient tid amt uri)

genMintTokenInvalid :: Gen (PRGState, PRGInput)
genMintTokenInvalid = do
  recipient <- elements samplePKHs
  let s = initialState {verifiedAccounts = []}
  tid <- arbitrary
  amt <- arbitrary `suchThat` (>= 0)
  uri <- genBuiltinBS
  return (s, MintToken recipient tid amt uri)

genLazyMintValid :: Gen (PRGState, PRGInput)
genLazyMintValid = do
  creator <- elements samplePKHs
  let s = initialState {verifiedAccounts = [creator]}
  buyer <- genPubKeyHash
  vtid <- arbitrary
  vamt <- arbitrary `suchThat` (>= 0)
  vuri <- genBuiltinBS
  return (s, LazyMint buyer vtid vamt creator vuri)

genLazyMintInvalid :: Gen (PRGState, PRGInput)
genLazyMintInvalid = do
  creator <- elements samplePKHs
  let s = initialState {verifiedAccounts = []}
  buyer <- genPubKeyHash
  vtid <- arbitrary
  vamt <- arbitrary `suchThat` (>= 0)
  vuri <- genBuiltinBS
  return (s, LazyMint buyer vtid vamt creator vuri)

genAttachPRG :: Gen (PRGState, PRGInput)
genAttachPRG = do
  s <- arbitrary
  user <- genPubKeyHash
  tid <- arbitrary
  amt <- arbitrary `suchThat` (>= 0)
  nftA <- genPubKeyHash
  nftTId <- arbitrary
  return (s, AttachPRG user tid amt nftA nftTId)

genPubKeyHash :: Gen PubKeyHash
genPubKeyHash = elements samplePKHs

genBuiltinBS :: Gen BuiltinByteString
genBuiltinBS = toBuiltin . BSC.pack Control.Applicative.<$> arbitrary

------------------------------------------------------------
-- Properties (no discards)
------------------------------------------------------------

prop_mint_valid :: Property
prop_mint_valid = forAll genMintTokenValid $ \(s, input) ->
  Data.Maybe.isJust (transition s input)

prop_mint_invalid :: Property
prop_mint_invalid = forAll genMintTokenInvalid $ \(s, input) ->
  Data.Maybe.isNothing (transition s input)

prop_lazyMint_valid :: Property
prop_lazyMint_valid = forAll genLazyMintValid $ \(s, input) ->
  Data.Maybe.isJust (transition s input)

prop_lazyMint_invalid :: Property
prop_lazyMint_invalid = forAll genLazyMintInvalid $ \(s, input) ->
  Data.Maybe.isNothing (transition s input)

prop_attach_valid :: Property
prop_attach_valid = forAll genAttachPRG $ \(s, input) ->
  Data.Maybe.isJust (transition s input)

prop_mint_updates :: Property
prop_mint_updates = forAll genMintTokenValid $ \(s, MintToken recipient tid amt uri) ->
  let newState = fromJust (transition s (MintToken recipient tid amt uri))
      newURI = Map.lookup tid (tokenURIs newState)
      prev = fromMaybe 0 (Map.lookup tid (totalSupply s))
      newSup = Map.lookup tid (totalSupply newState)
   in newURI === Just uri .&&. newSup === Just (prev + amt)

prop_lazyMint_uri :: Property
prop_lazyMint_uri = forAll genLazyMintValid $ \(s, LazyMint _ tid _ creator uri) ->
  let newState = fromJust (transition s (LazyMint undefined tid undefined creator uri))
      oldURI = Map.lookup tid (tokenURIs s)
      newURI = Map.lookup tid (tokenURIs newState)
   in if Map.member tid (tokenURIs s)
        then newURI === oldURI
        else newURI === Just uri

prop_lazyMint_supply :: Property
prop_lazyMint_supply = forAll genLazyMintValid $ \(s, LazyMint _ tid amt creator _) ->
  let newState = fromJust (transition s (LazyMint undefined tid amt creator undefined))
      prev = fromMaybe 0 (Map.lookup tid (totalSupply s))
      newSup = fromMaybe 0 (Map.lookup tid (totalSupply newState))
   in newSup === prev + amt

prop_attach_amount :: Property
prop_attach_amount = forAll genAttachPRG $ \(s, AttachPRG _ tid amt nftA nftTId) ->
  let key = AttachKey tid nftA nftTId
      newState = fromJust (transition s (AttachPRG undefined tid amt nftA nftTId))
      prevAtt = fromMaybe 0 (Map.lookup key (attachedTokens s))
      newAtt = fromMaybe 0 (Map.lookup key (attachedTokens newState))
   in newAtt === prevAtt + amt

------------------------------------------------------------
-- Test Execution
------------------------------------------------------------

runTests :: IO ()
runTests = do
  putStrLn "Running QuickCheck tests..."
  quickCheck prop_mint_valid
  quickCheck prop_mint_invalid
  quickCheck prop_lazyMint_valid
  quickCheck prop_lazyMint_invalid
  quickCheck prop_attach_valid
  quickCheck prop_mint_updates
  quickCheck prop_lazyMint_uri
  quickCheck prop_lazyMint_supply
  quickCheck prop_attach_amount

main :: IO ()
main = runTests