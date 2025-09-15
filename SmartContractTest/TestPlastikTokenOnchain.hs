{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fewer imports" #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module PT.TestPlastikTokenOnchain where

-- On-chain and utility imports
import           Cardano.Api.Shelley         (PlutusScript(..), PlutusScriptV2, displayError, writeFileTextEnvelope)
import           Codec.Serialise             (serialise)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           Data.Char (chr)
import           Data.List                   (isInfixOf)
import           Plutus.V2.Ledger.Api        (PubKeyHash(..), ScriptContext, Validator, mkValidatorScript, scriptContextTxInfo, txInfoSignatories, unValidatorScript, unsafeFromBuiltinData)
import           PlutusTx                    (compile, unstableMakeIsData)
import qualified PlutusTx.Builtins as Builtins
import           PlutusTx.Builtins.Internal  (BuiltinData)
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import qualified Prelude          as P
import           Prelude                    ((^), Char, FilePath, IO, Int, String, fromIntegral, print, putStrLn)
import           System.Directory            (createDirectoryIfMissing)

-- Testing imports
import           System.FilePath             (takeDirectory)
import           Test.QuickCheck            (Arbitrary(..), Gen, Testable (..), choose, vectorOf, forAll, conjoin)
import           Test.Tasty                 (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit           ((@?=), Assertion, assertFailure, testCase)
import qualified Test.Tasty.QuickCheck as QC
import           Test.Tasty.QuickCheck      ((===), (==>), Property, testProperty)
import Control.Exception (try, SomeException, evaluate)
import Test.QuickCheck.Monadic (monadicIO, assert, PropertyM, run)
import Test.QuickCheck (ioProperty)



--------------------------------------------------------------------------------
-- Helper functions for association lists
--------------------------------------------------------------------------------

{-# INLINEABLE lookupAssoc #-}
lookupAssoc :: Eq a => a -> [(a, b)] -> Maybe b
lookupAssoc _ [] = Nothing
lookupAssoc key ((k, v):xs) = if key==k then Just v else lookupAssoc key xs

{-# INLINEABLE insertAssoc #-}
insertAssoc :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
insertAssoc key val [] = [(key,val)]
insertAssoc key val ((k,v):xs) = if key==k then (key,val):xs else (k,v):insertAssoc key val xs

{-# INLINEABLE findWithDefaultAssoc #-}
findWithDefaultAssoc :: Eq a => b -> a -> [(a,b)] -> b
findWithDefaultAssoc def key xs = case lookupAssoc key xs of Nothing -> def; Just v -> v

--------------------------------------------------------------------------------
-- Core on-chain types and logic
--------------------------------------------------------------------------------

data TokenState = TokenState
  { owner       :: PubKeyHash
  , tokenName   :: BuiltinByteString
  , tokenId     :: Integer
  , tokenAddr   :: PubKeyHash
  , paused      :: Bool
  , lockers     :: [(PubKeyHash,Bool)]
  , balances    :: [(PubKeyHash,Integer)]
  , totalSupply :: Integer
  } deriving (P.Show)

PlutusTx.unstableMakeIsData ''TokenState

{-# INLINEABLE decimals #-}
decimals :: Integer
decimals = 9

{-# INLINEABLE initialSupply #-}
initialSupply :: Integer
initialSupply = 1000000000 * (10 ^ decimals)

data TokenAction = Pause | Unpause | SetLocker PubKeyHash Bool | Transfer PubKeyHash Integer | TransferFrom PubKeyHash PubKeyHash Integer
  deriving (P.Show)

PlutusTx.unstableMakeIsData ''TokenAction

{-# INLINEABLE applyAction #-}
applyAction :: TokenState -> TokenAction -> PubKeyHash -> TokenState
applyAction ts action signer = case action of
  Pause -> if signer==owner ts then ts{paused=True} else traceError "pause: Only owner can pause"
  Unpause -> if signer==owner ts then ts{paused=False} else traceError "unpause: Only owner can unpause"
  SetLocker pk flag -> if signer==owner ts then ts{lockers=insertAssoc pk flag (lockers ts)} else traceError "setLocker: Only owner"
  Transfer to amt -> if lookupAssoc signer (lockers ts)/=Just True then traceError "transfer: Only lockers" else
                     let bal = maybe (traceError "transfer: Insufficient balance") id (lookupAssoc signer (balances ts)) in
                     if bal<amt then traceError "transfer: Insufficient funds" else
                       let prev = findWithDefaultAssoc 0 to (balances ts)
                           newB = insertAssoc signer (bal-amt) $ insertAssoc to (prev+amt) (balances ts)
                       in ts{balances=newB}
  TransferFrom f t amt -> if lookupAssoc signer (lockers ts)/=Just True then traceError "transferFrom: Only lockers" else
                          let bal = maybe (traceError "transferFrom: Sender has no balance") id (lookupAssoc f (balances ts)) in
                          if bal<amt then traceError "transferFrom: Insufficient funds" else
                            let prev = findWithDefaultAssoc 0 t (balances ts)
                                newB = insertAssoc f (bal-amt) $ insertAssoc t (prev+amt) (balances ts)
                            in ts{balances=newB}

{-# INLINEABLE mkValidator #-}
mkValidator :: TokenState -> TokenAction -> ScriptContext -> Bool
mkValidator ts action ctx =
  let info = scriptContextTxInfo ctx
      signers = txInfoSignatories info
      signer = case signers of [s]->s; _-> traceError "mkValidator: Expected exactly one signer"
      _ = if paused ts then traceError "mkValidator: Token paused" else ()
      _ = applyAction ts action signer
  in True

{-# INLINEABLE wrapValidator #-}
wrapValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapValidator d r c = let ts=unsafeFromBuiltinData d; act=unsafeFromBuiltinData r; ctx=unsafeFromBuiltinData c in if mkValidator ts act ctx then () else traceError "validation failed"

validator :: Validator
validator = mkValidatorScript $$(compile [||wrapValidator||])

writePlutusScript :: FilePath -> Validator -> IO ()
writePlutusScript file val = do
  createDirectoryIfMissing True (takeDirectory file)
  res <- writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing
           $ PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise
           $ unValidatorScript val
  case res of Left e->print (displayError e); Right()->putStrLn "Wrote script"

--------------------------------------------------------------------------------
-- Smoke tests (IO) and Automated test suite
--------------------------------------------------------------------------------

-- Sample keys
alicePKH,bobPKH,charliePKH :: PubKeyHash
alicePKH   = PubKeyHash $ Builtins.toBuiltin (BSC.pack "Alice")
bobPKH     = PubKeyHash $ Builtins.toBuiltin (BSC.pack "Bob")
charliePKH = PubKeyHash $ Builtins.toBuiltin (BSC.pack "Charlie")

-- Initial states
initialState :: TokenState
initialState = TokenState alicePKH "Plastik" 1 alicePKH False [(alicePKH,True)] [(alicePKH,initialSupply)] initialSupply
pausedState = initialState{paused=True}

-- Exception helper
assertError :: a -> Assertion
assertError action =
  E.handle (\(_ :: E.ErrorCall) -> return ()) $ do
    _ <- E.evaluate action
    assertFailure "Expected an on-chain error"

-- HUnit unit-tests
unitTests :: TestTree
unitTests = testGroup "applyAction unit tests"
  [ testCase "owner pause" $ paused (applyAction initialState Pause alicePKH) @?= True
  , testCase "non-owner pause fails" $ assertError $ applyAction initialState Pause bobPKH
  , testCase "owner unpause" $ paused (applyAction pausedState Unpause alicePKH) @?= False
  , testCase "non-owner unpause fails" $ assertError $ applyAction pausedState Unpause bobPKH 
  , testCase "setLocker by owner" $ lookupAssoc charliePKH (lockers $ applyAction initialState (SetLocker charliePKH True) alicePKH) @?= Just True
  , testCase "setLocker fails non-owner" $ assertError $ applyAction initialState (SetLocker bobPKH False) bobPKH  
  , testCase "transfer updates bal" $ lookupAssoc bobPKH (balances $ applyAction initialState (Transfer bobPKH 500) alicePKH) @?= Just 500
  , testCase "transfer fails if not locker" $ assertError $ applyAction initialState{lockers=[]} (Transfer bobPKH 1) alicePKH  
  , testCase "transfer insufficient" $ assertError  $ applyAction initialState (Transfer bobPKH (initialSupply+1)) alicePKH   
  , testCase "transferFrom updates" $ let st=applyAction initialState (SetLocker bobPKH True) alicePKH; st2=st{balances=[(alicePKH,initialSupply),(bobPKH,300)]} in lookupAssoc charliePKH (balances $ applyAction st2 (TransferFrom bobPKH charliePKH 200) bobPKH) @?= Just 200
  , testCase "transferFrom fails non-locker" $ assertError  $ applyAction initialState (TransferFrom alicePKH bobPKH 1) bobPKH  
  , testCase "transferFrom no bal" $ assertError $ applyAction initialState{lockers=[(bobPKH,True)],balances=[(alicePKH,initialSupply)]} (TransferFrom bobPKH alicePKH 1) bobPKH
  , testCase "transferFrom insufficient" $ assertError  $ applyAction initialState{lockers=[(bobPKH,True)],balances=[(bobPKH,5)]} (TransferFrom bobPKH alicePKH 10) bobPKH 
  ]

-- QuickCheck properties
instance Arbitrary PubKeyHash where
  arbitrary = do
    bytes <- vectorOf 10 (choose (65, 122) :: Gen Int)  
    return . PubKeyHash . toBuiltin . BSC.pack $ map (chr . fromIntegral) bytes

prop_pause_idemp :: Property
prop_pause_idemp = let ts1=applyAction initialState Pause alicePKH; ts2=applyAction ts1 Pause alicePKH in property $ paused ts1 && paused ts2

prop_unpause :: Property
prop_unpause = let ts=applyAction pausedState Unpause alicePKH in property $ not (paused ts)

prop_transfer_supply :: PubKeyHash -> Integer -> Property
prop_transfer_supply rec amt = (amt>=0 && amt<=initialSupply) ==> let ts=applyAction initialState (Transfer rec amt) alicePKH; sumB=foldr (+) 0 (map snd (balances ts)) in sumB===totalSupply ts where totalSupply (TokenState{totalSupply=s})=s

prop_setLocker_multiple :: Property
prop_setLocker_multiple = 
  let ts1 = applyAction initialState (SetLocker charliePKH True) alicePKH
      ts2 = applyAction ts1 (SetLocker bobPKH True) alicePKH
  in conjoin
       [ lookupAssoc bobPKH (lockers ts2) === Just True
       , lookupAssoc charliePKH (lockers ts2) === Just True
       ]

-- | Check that transferring more than the total supply throws.
prop_transferInsufficientFunds :: Property
prop_transferInsufficientFunds =
  forAll (choose (initialSupply + 1, initialSupply + 100)) $ \amt ->
    (amt > initialSupply) ==>
      ioProperty $ do
        eres <- try (evaluate (applyAction initialState (Transfer bobPKH amt) alicePKH))
                  :: IO (Either SomeException TokenState)
        return (isLeft eres)

-- Combine and run all tests
tests :: TestTree
tests = testGroup "PlastikTokenOnchain Tests" 
  [ unitTests
  , testProperty "pause idempotent" prop_pause_idemp
  , testProperty "unpause works" prop_unpause
  , testProperty "transfer preserves supply" prop_transfer_supply
  , testProperty "setLocker works for multiple" prop_setLocker_multiple
  , testProperty "transfer fails for insufficient funds" prop_transferInsufficientFunds
  ]

main :: IO ()
main = defaultMain tests
