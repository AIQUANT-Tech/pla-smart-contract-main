-- {-# LANGUAGE DataKinds             #-}
-- {-# LANGUAGE DeriveAnyClass        #-}
-- {-# LANGUAGE DeriveGeneric         #-}
-- {-# LANGUAGE NoImplicitPrelude     #-}
-- {-# LANGUAGE TemplateHaskell       #-}
-- {-# LANGUAGE ScopedTypeVariables   #-}
-- {-# LANGUAGE TypeApplications      #-}

-- module Plastik.PlastikRecoveryProjects where

-- import           PlutusTx.Prelude         hiding (Semigroup(..), unless)
-- import           PlutusTx                 (Data (..), compile, unstableMakeIsData)
-- -- import qualified PlutusTx
-- import           Plutus.V2.Ledger.Api     (PubKeyHash, Validator, ScriptContext, txOutDatum, toBuiltinData,
--                                           mkValidatorScript, unsafeFromBuiltinData,Datum(..), DatumHash(..), OutputDatum(..), TxOutRef, ScriptPurpose(..), unValidatorScript)
-- import           Plutus.V2.Ledger.Contexts (txSignedBy, scriptContextTxInfo,
--                                           txInfoOutputs, txOutAddress,
--                                           scriptContextPurpose, TxInInfo(..),
--                                           txInInfoResolved, txInfoInputs)
-- -- import           Plutus.V2.Ledger.Tx      (DatumHash, txOutDatum, TxInInfo, txInInfoResolved, txInfoInputs)
-- -- import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
-- import           PlutusTx.AssocMap        (Map)  -- Changed to PlutusTx's Map
-- import qualified PlutusTx.AssocMap as Map 
-- import           GHC.Generics             (Generic)
-- import           Prelude                  (Show, FilePath, IO, print, putStrLn )
-- -- import           PlutusTx.Builtins           (error, emptyByteString)
-- -- import           PlutusTx.Builtins.Internal  (BuiltinData)
-- import qualified Data.ByteString.Lazy as LBS
-- import qualified Data.ByteString.Short as SBS
-- import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2, displayError, writeFileTextEnvelope)
-- import Codec.Serialise (serialise)
-- import System.Directory (createDirectoryIfMissing)
-- import System.FilePath (takeDirectory)

-- ------------------------------------------------------------
-- -- On-Chain Data Types
-- ------------------------------------------------------------

-- data PRPState = PRPState
--     { admin    :: PubKeyHash
--     , projects :: Map PubKeyHash Bool
--     } deriving (Show, Generic)

-- PlutusTx.unstableMakeIsData ''PRPState

-- data PRPAction = 
--       AddPlasticRecoveryProject PubKeyHash Bool
--     | IsPlasticRecoveryProject PRPState PubKeyHash
--     deriving Show

-- PlutusTx.unstableMakeIsData ''PRPAction

-- ------------------------------------------------------------
-- -- Validation Logic
-- ------------------------------------------------------------

-- -- {-# INLINABLE isPlasticRecoveryProject #-}
-- -- isPlasticRecoveryProject :: PRPState -> PubKeyHash -> Bool
-- -- isPlasticRecoveryProject st p = case Map.lookup p (projects st) of
-- --     Just status -> status
-- --     Nothing     -> False

   
-- {-# INLINABLE findOwnInput #-}
-- findOwnInput :: ScriptContext -> TxOutRef -> TxInInfo
-- findOwnInput ctx txOutRef =
--     case find (\i -> txInInfoOutRef i == txOutRef) (txInfoInputs $ scriptContextTxInfo ctx) of
--         Just input -> input
--         Nothing    -> traceError "Own input not found"


-- {-# INLINABLE validateStateUpdate #-}
-- validateStateUpdate :: PRPState -> PubKeyHash -> Bool -> ScriptContext -> Bool
-- validateStateUpdate oldState newProject newStatus ctx =
--     let newProjects = Map.insert newProject newStatus (projects oldState)
--         newState = oldState { projects = newProjects }
--         info = scriptContextTxInfo ctx
--         ownAddress = case scriptContextPurpose ctx of
--             Spending txOutRef -> txOutAddress (txInInfoResolved (findOwnInput ctx txOutRef))
--             _                 -> traceError "Invalid script purpose"
        
--         outputs = txInfoOutputs info
--         validOutputs = filter (\o -> 
--             txOutAddress o == ownAddress && 
--             case txOutDatum o of
--                 OutputDatum d -> d == Datum (toBuiltinData newState)  -- Direct datum comparison
--                 _             -> False
--             ) outputs
--         outputCheck = case validOutputs of
--             [_] -> True
--             _   -> False
--     in outputCheck
 

-- {-# INLINABLE mkValidator #-}
-- mkValidator :: PRPState -> PRPAction -> ScriptContext -> Bool
-- mkValidator st action ctx = 
--     let info = scriptContextTxInfo ctx
--     in case action of
--         AddPlasticRecoveryProject p b ->
--             traceIfFalse "Not signed by admin" (txSignedBy info (admin st)) &&
--             validateStateUpdate st p b ctx
--         IsPlasticRecoveryProject st p ->
--             case Map.lookup p (projects st) of
--                 Just status -> status
--                 Nothing     -> False


-- {-# INLINABLE wrapValidator #-}
-- wrapValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
-- wrapValidator datum redeemer context =
--   let prpState :: PRPState = unsafeFromBuiltinData datum
--       prpAction :: PRPAction = unsafeFromBuiltinData redeemer
--       scriptContext :: ScriptContext = unsafeFromBuiltinData context
--    in if mkValidator prpState prpAction scriptContext
--         then ()
--         else traceError "Plastic Recovery Projects validation failed"

-- ------------------------------------------------------------
-- -- Validator Script
-- ------------------------------------------------------------

-- validator :: Validator
-- validator = mkValidatorScript $$(compile [|| wrapValidator ||])


-- writePlutusScript :: FilePath -> Validator -> IO ()
-- writePlutusScript file validator = do
--   -- let script = PlutusScriptSerialised . SBS.toShort . LBS.toStrict $ serialise validator

--   createDirectoryIfMissing True (takeDirectory file)
--   result <-
--     writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing $
--       PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise $
--         Plutus.V2.Ledger.Api.unValidatorScript
--           validator
--   case result of
--     Left err -> print $ displayError err
--     Right () -> putStrLn "Successfully wrote Plutus script to file."

-- writeScript :: IO ()
-- writeScript = writePlutusScript "Plastik/Test/TestPlastikRecoveryProjects.plutus" validator

-- main :: IO ()
-- main = do
--     writeScript

-- adminPkh :: PubKeyHash
-- adminPkh = PubKeyHash (Builtins.toBuiltin (BSC.pack "Alice"))

-- project1Pkh :: PubKeyHash
-- project1Pkh = PubKeyHash (Builtins.toBuiltin (BSC.pack "Bob1"))

-- project2Pkh :: PubKeyHash
-- project2Pkh = PubKeyHash (Builtins.toBuiltin (BSC.pack "Bob2"))


-- initialState :: PRPState
-- initialState = PRPState adminPkh Map.empty

-- main :: IO ()
-- main = do
--     -- Write the validator script to a file
--     -- writeScript
--     putStrLn "Successfully compiled.\n"

--     -- Test Case 1: Admin successfully adds a new project (Expected: Pass)
--     putStrLn "Test Case 1: Admin adds a new project into an empty map."
--     let action = AddPlasticRecoveryProject project1Pkh True

--     -- Calculate expected new state
--     let newProjects = Map.insert project1Pkh True (projects initialState)
--         expectedUpdatedState = initialState { projects = newProjects }

--     print $ "Initial State: " ++ show initialState
--     print $ "Action: " ++ show action
--     print $ "Expected Updated State: " ++ show expectedUpdatedState

--         -- Mock context where admin signs and output has correct state
--     print "Test Case 1: Validation passed"


--     -- Test Case 2: Update existing project status
--     putStrLn "Test Case 2: Update existing project"
--     let updatedState = expectedUpdatedState
--         updateAction = AddPlasticRecoveryProject project1Pkh False

--     let newProjects1 = Map.insert project1Pkh False (projects expectedUpdatedState)
--         expectedUpdatedState1 = expectedUpdatedState { projects = newProjects1 }
--     print $ "Current State: " ++ show expectedUpdatedState
--     print $ "Action: " ++ show updateAction
--     print $ "Expected Updated State: " ++ show expectedUpdatedState1

--     print "Test Case 2: Validation passed"

--     -- Test Case 3: Add a new project status
--     putStrLn "Test Case 3: Add a new project"
--     let updatedState2 = expectedUpdatedState1
--         updateAction2 = AddPlasticRecoveryProject project2Pkh True

--     let newProjects2 = Map.insert project2Pkh True (projects expectedUpdatedState1)
--         expectedUpdatedState2 = expectedUpdatedState1 { projects = newProjects2 }
--     print $ "Current State: " ++ show expectedUpdatedState1
--     print $ "Action: " ++ show updateAction2
--     print $ "Expected Updated State: " ++ show expectedUpdatedState2

--     print "Test Case 3: Validation passed"




{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Plastik.Test.TestPlastikRecoveryProjects where

import           PlutusTx.Prelude         hiding (Semigroup(..), unless, (<$>),(<*>), pure)
import           PlutusTx                 (Data (..), compile, unstableMakeIsData)
-- import qualified PlutusTx
import           Plutus.V2.Ledger.Api     (PubKeyHash, PubKeyHash(..), Validator, ScriptContext, txOutDatum, toBuiltinData,
                                          mkValidatorScript, unsafeFromBuiltinData,Datum(..), DatumHash(..), OutputDatum(..), ScriptPurpose(..), unValidatorScript, TxOut(..), TxInfo(..), Address(..), always, ScriptContext, Credential(ScriptCredential), ValidatorHash(..))
import           Plutus.V2.Ledger.Contexts (txSignedBy, scriptContextTxInfo,
                                          txInfoOutputs, txOutAddress,
                                          scriptContextPurpose, TxInInfo(..),
                                          txInInfoResolved, txInfoInputs, TxOutRef(..), ScriptContext(..))
-- import           Plutus.V2.Ledger.Tx      (DatumHash, txOutDatum, TxInInfo, txInInfoResolved, txInfoInputs)
-- import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import           PlutusTx.AssocMap        (Map)  -- Changed to PlutusTx's Map
import qualified PlutusTx.AssocMap as Map 
import           GHC.Generics             (Generic)
import           Prelude                  (Show(..), FilePath, IO, print, putStrLn)
import           PlutusTx.Builtins           (error, emptyByteString)
import           PlutusTx.Builtins.Internal  (BuiltinData)
import qualified Data.ByteString.Lazy as LBS 
import qualified Data.ByteString.Short as SBS
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2, displayError, writeFileTextEnvelope)
import Codec.Serialise (serialise)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import qualified PlutusTx.Builtins as Builtins  -- For creating test PubKeyHash
import qualified Data.ByteString.Char8 as BSC
import           Test.QuickCheck            (Arbitrary (..), Gen, Property, elements, forAll,
                                              listOf, listOf1, oneof, suchThat, (===), quickCheck, forAllBlind,resize)
import           Test.QuickCheck.Instances  ()

-- import           Plastik.PlastikRoleV2      (RoleDatum (..), RoleRedeemer (..),
--                                               mkPlastikRoleValidator)
import qualified PlutusTx.AssocMap as AssocMap
-- import Test.QuickCheck (withMaxSuccess)
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, assertBool, Assertion)
import Test.Tasty.QuickCheck
    ( testProperty
    , Arbitrary (..)
    , Positive (..)
    , Property
    , chooseInt
    , elements
    , (===)
    , (==>)
    , Gen
    , (.&&.)
    , property
    , listOf1
    , vectorOf
    , suchThat
    , conjoin
    )

import Control.Applicative (Applicative(..), (<$>), (<*>))
import Control.Monad       (Monad(..))
import qualified Plutus.V2.Ledger.Api as V2
import qualified Data.ByteString      as BS
import           Test.QuickCheck      (vectorOf)
------------------------------------------------------------
-- On-Chain Data Types
------------------------------------------------------------

data PRPState = PRPState
    { admin    :: PubKeyHash
    , projects :: Map PubKeyHash Bool
    } deriving (Show, Generic)

PlutusTx.unstableMakeIsData ''PRPState

data PRPAction = 
      AddPlasticRecoveryProject PubKeyHash Bool
    | IsPlasticRecoveryProject PRPState PubKeyHash
    deriving Show

PlutusTx.unstableMakeIsData ''PRPAction

------------------------------------------------------------
-- Validation Logic
------------------------------------------------------------

-- {-# INLINABLE isPlasticRecoveryProject #-}
-- isPlasticRecoveryProject :: PRPState -> PubKeyHash -> Bool
-- isPlasticRecoveryProject st p = case Map.lookup p (projects st) of
--     Just status -> status
--     Nothing     -> False

   
{-# INLINABLE findOwnInput #-}
findOwnInput :: ScriptContext -> TxOutRef -> TxInInfo
findOwnInput ctx txOutRef =
    case find (\i -> txInInfoOutRef i == txOutRef) (txInfoInputs $ scriptContextTxInfo ctx) of
        Just input -> input
        Nothing    -> traceError "Own input not found"


{-# INLINABLE validateStateUpdate #-}
validateStateUpdate :: PRPState -> PubKeyHash -> Bool -> ScriptContext -> Bool
validateStateUpdate oldState newProject newStatus ctx =
    let newProjects = Map.insert newProject newStatus (projects oldState)
        newState = oldState { projects = newProjects }
        info = scriptContextTxInfo ctx
        ownAddress = case scriptContextPurpose ctx of
            Spending txOutRef -> txOutAddress (txInInfoResolved (findOwnInput ctx txOutRef))
            _                 -> traceError "Invalid script purpose"
        
        outputs = txInfoOutputs info
        validOutputs = filter (\o -> 
            txOutAddress o == ownAddress && 
            case txOutDatum o of
                OutputDatum d -> d == Datum (toBuiltinData newState)  -- Direct datum comparison
                _             -> False
            ) outputs
        outputCheck = case validOutputs of
            [_] -> True
            _   -> False
    in outputCheck
 

{-# INLINABLE mkValidator #-}
mkValidator :: PRPState -> PRPAction -> ScriptContext -> Bool
mkValidator st action ctx = 
    let info = scriptContextTxInfo ctx
    in case action of
        AddPlasticRecoveryProject p b ->
            traceIfFalse "Not signed by admin" (txSignedBy info (admin st)) &&
            validateStateUpdate st p b ctx
        IsPlasticRecoveryProject st p -> 
            case Map.lookup p (projects st) of
                Just status -> status
                Nothing     -> False



{-# INLINABLE wrapValidator #-}
wrapValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapValidator datum redeemer context =
  let prpState :: PRPState = unsafeFromBuiltinData datum
      prpAction :: PRPAction = unsafeFromBuiltinData redeemer
      scriptContext :: ScriptContext = unsafeFromBuiltinData context
   in if mkValidator prpState prpAction scriptContext
        then ()
        else traceError "Plastic Recovery Projects validation failed"

------------------------------------------------------------
-- Validator Script
------------------------------------------------------------

validator :: Validator
validator = mkValidatorScript $$(compile [|| wrapValidator ||])


------------------------------------------------------------
-- Test Data
------------------------------------------------------------

alicePKH, bobPKH, charliePKH :: PubKeyHash
alicePKH = PubKeyHash (Builtins.toBuiltin (BSC.pack "Alice"))
bobPKH = PubKeyHash (Builtins.toBuiltin (BSC.pack "Bob"))
charliePKH = PubKeyHash (toBuiltin (BSC.pack "Charlie"))

initialState :: PRPState
initialState = PRPState
    { admin = alicePKH
    , projects = Map.empty
    }

------------------------------------------------------------
-- Mock Context Creation
------------------------------------------------------------

-- | Creates a mock ScriptContext with specific signatories and outputs.
mockContext :: [PubKeyHash] -> [TxOut] -> ScriptContext
mockContext signers outputs =
    let txInfo = TxInfo
            { txInfoInputs = [TxInInfo (TxOutRef "" 0) (TxOut (addressFromValidator validator) mempty (OutputDatum $ Datum $ toBuiltinData initialState) Nothing) ]
            , txInfoOutputs = outputs
            , txInfoFee = mempty
            , txInfoMint = mempty
            , txInfoDCert = []
            , txInfoWdrl = Map.empty
            , txInfoValidRange = always
            , txInfoSignatories = signers
            , txInfoData = Map.empty
            , txInfoId = ""
            }
        purpose = Spending (TxOutRef "" 0)
    in ScriptContext txInfo purpose

-- Helper to generate a contract address (simplified)
addressFromValidator :: Validator -> Address
addressFromValidator _ = Address (ScriptCredential (ValidatorHash (toBuiltin (BSC.pack "mock")))) Nothing

------------------------------------------------------------
-- Generators
------------------------------------------------------------

instance Arbitrary PRPState where
    arbitrary = do
        adm <- elements [alicePKH, bobPKH, charliePKH]
        -- Explicitly sequence the tuple generation
        prjPairs <- listOf $ (,) <$> arbitrary <*> arbitrary
        return $ PRPState adm (Map.fromList prjPairs)

instance Arbitrary PRPAction where
    arbitrary = oneof
        [ AddPlasticRecoveryProject <$> arbitrary <*> arbitrary
        , IsPlasticRecoveryProject <$> arbitrary <*> arbitrary
        ]

-- instance Arbitrary PubKeyHash where
--     arbitrary = elements [alicePKH, bobPKH, charliePKH]


instance Arbitrary V2.PubKeyHash where
    arbitrary = do
        -- Generate random 28-byte values (typical for Ed25519 hashes)
        bytes <- vectorOf 28 arbitrary
        pure $ V2.PubKeyHash (Builtins.toBuiltin (BS.pack bytes))
        
------------------------------------------------------------
-- QuickCheck Properties
------------------------------------------------------------

-- | Adding a project with admin signature succeeds
prop_addByAdmin :: Property
prop_addByAdmin = forAll arbitrary $ \(st, project, status) ->
    let action = AddPlasticRecoveryProject project status
        ctx = mockContext [admin st] [validOutput st project status]
     in mkValidator st action ctx === True
  where
    validOutput st p b =
        TxOut (addressFromValidator validator) mempty (OutputDatum $ Datum $ toBuiltinData newState) Nothing
        where newState = st { projects = Map.insert p b (projects st) }

-- | Adding a project without admin signature fails
prop_addByNonAdmin :: Property
prop_addByNonAdmin = forAll arbitrary $ \(st, project, status, signer) ->
    signer /= admin st ==>
        let action = AddPlasticRecoveryProject project status
            ctx = mockContext [signer] []
         in mkValidator st action ctx === False

-- | Checking an existing project returns its status
prop_checkExistingProject :: Property
prop_checkExistingProject = forAll arbitrary $ \(st, project, status) ->
    let newState = st { projects = Map.insert project status (projects st) }
        action = IsPlasticRecoveryProject newState project
        ctx = mockContext [] []
     in mkValidator newState action ctx === status

-- | Checking a non-existent project returns False
prop_checkNonExistentProject :: Property
prop_checkNonExistentProject = forAll genNonExistentCase $ \(st, project) ->
    let action = IsPlasticRecoveryProject st project
        ctx = mockContext [] []
    in mkValidator st action ctx === False
  where
    genNonExistentCase = do
        st <- arbitrary  -- Generate a random state
        -- Generate project NOT in st.projects
        project <- arbitrary `suchThat` (\p -> not $ Map.member p (projects st))
        return (st, project)


------------------------------------------------------------
-- Unit Suite
------------------------------------------------------------

unit_AdminAddsProject :: Assertion
unit_AdminAddsProject = do
    let st = initialState
        action = AddPlasticRecoveryProject bobPKH True
        newState = st { projects = Map.singleton bobPKH True }
        validOutput = TxOut (addressFromValidator validator) mempty (OutputDatum $ Datum $ toBuiltinData newState) Nothing
        ctx = mockContext [alicePKH] [validOutput]
    assertBool "Admin should add project" (mkValidator st action ctx)

unit_NonAdminFailsToAdd :: Assertion
unit_NonAdminFailsToAdd = do
    let st = initialState
        action = AddPlasticRecoveryProject bobPKH True
        ctx = mockContext [bobPKH] []
    assertBool "Non-admin should fail" (not (mkValidator st action ctx))

unit_CheckProjectStatus :: Assertion
unit_CheckProjectStatus = do
    let st = initialState { projects = Map.singleton bobPKH True }
        action = IsPlasticRecoveryProject st bobPKH
        ctx = mockContext [] []
    assertBool "Check existing project" (mkValidator st action ctx)

unit_CheckNonExistentProject :: Assertion
unit_CheckNonExistentProject = do
    let st = initialState
        action = IsPlasticRecoveryProject st charliePKH
        ctx = mockContext [] []
    assertBool "Check non-existent project" (not (mkValidator st action ctx))

------------------------------------------------------------
-- Test Suite
------------------------------------------------------------
    
tests :: TestTree
tests = testGroup "Plastik Recovery Projects Tests"
    [ testGroup "Property Tests"
        [ testProperty "Admin can add project" prop_addByAdmin
        , testProperty "Non-admin cannot add project" prop_addByNonAdmin
        , testProperty "Check existing project status" prop_checkExistingProject
        , testProperty "Check non-existent project" prop_checkNonExistentProject
        ]
    , testGroup "Unit Tests"
        [ testCase "Admin adds project" unit_AdminAddsProject
        , testCase "Non-admin fails to add" unit_NonAdminFailsToAdd
        , testCase "Check project status" unit_CheckProjectStatus
        , testCase "Check non-existent project" unit_CheckNonExistentProject
        ]
    ]

main :: IO ()
main = defaultMain tests