{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week05.MintAndValidatorAtomicity where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Ledger.Ada             as Ada
import           Prelude                (IO, Show (..), String, Semigroup (..))
import           Text.Printf            (printf)
import Wallet.Emulator.Wallet ( knownWallet )

------ VALIDATOR -----

-- The purpose of the redeemer is to set the result of the validation.
{-# INLINABLE mkValidator #-}
mkValidator :: () -> Bool -> ScriptContext -> Bool
mkValidator _ succeed _ = succeed

data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed = ()
    type instance RedeemerType Typed = Bool

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @Bool

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

------ MINTING POLICY -----

-- The purpose of the redeemer is to set the result of the minting validation.
{-# INLINABLE mkPolicy #-}
mkPolicy :: Bool -> ScriptContext -> Bool
mkPolicy succeed _ = succeed

policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkPolicy ||])

curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy 

------ OFFCHAIN -----

type ValidatorResult = Bool
type MintingResult   = Bool

data MintParams = MintParams
    { mpTokenName  :: !TokenName
    , mpAmount     :: !Integer
    , mpValResult  :: !ValidatorResult
    , mpMintResult :: !MintingResult
    } deriving (Generic, ToJSON, FromJSON)

type AtomicitySchema = Endpoint "give" Integer .\/ Endpoint "mint-and-grab" MintParams

mintAndGrab :: MintParams -> Contract w AtomicitySchema Text ()
mintAndGrab MintParams{..} = do 
    utxos <- utxosAt scrAddress
    let orefs   = fst <$> Map.toList utxos
        mintVal = Value.singleton curSymbol mpTokenName mpAmount
        mintRed = Redeemer $ PlutusTx.toBuiltinData mpMintResult
        valRed  = Redeemer $ PlutusTx.toBuiltinData mpValResult
        lookups = Constraints.unspentOutputs utxos 
                <> Constraints.otherScript validator 
                <> Constraints.mintingPolicy policy
        tx      = Constraints.mustMintValueWithRedeemer mintRed mintVal 
                <> mconcat [Constraints.mustSpendScriptOutput oref valRed | oref <- orefs] 
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show mintVal)
    Contract.logInfo @String $ "collected gifts"

give :: Integer -> Contract w AtomicitySchema Text ()
give amount = do
    let tx = mustPayToTheScript () $ Ada.lovelaceValueOf amount
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace" amount

endpoints :: Contract () AtomicitySchema Text ()
endpoints = selectList [mintAndGrab', give'] >> endpoints
  where
    give'        = endpoint @"give" give
    mintAndGrab' = endpoint @"mint-and-grab" mintAndGrab

successTest :: IO ()
successTest = runEmulatorTraceIO $ do
    let tn = "ABC"
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    callEndpoint @"give" h1 50_000_000
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint-and-grab" h2 $ MintParams
        { mpTokenName  = tn
        , mpAmount     = 444
        , mpMintResult = True
        , mpValResult  = True
        }
    void $ Emulator.waitNSlots 2

failTest1 :: IO ()
failTest1 = runEmulatorTraceIO $ do
    let tn = "ABC"
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    callEndpoint @"give" h1 50_000_000
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint-and-grab" h2 $ MintParams
        { mpTokenName  = tn
        , mpAmount     = 444
        , mpMintResult = False
        , mpValResult  = True
        }
    void $ Emulator.waitNSlots 2

failTest2 :: IO ()
failTest2 = runEmulatorTraceIO $ do
    let tn = "ABC"
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    callEndpoint @"give" h1 50_000_000
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint-and-grab" h2 $ MintParams
        { mpTokenName  = tn
        , mpAmount     = 444
        , mpMintResult = True
        , mpValResult  = False
        }
    void $ Emulator.waitNSlots 2

failTest3 :: IO ()
failTest3 = runEmulatorTraceIO $ do
    let tn = "ABC"
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    callEndpoint @"give" h1 50_000_000
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint-and-grab" h2 $ MintParams
        { mpTokenName  = tn
        , mpAmount     = 444
        , mpMintResult = True
        , mpValResult  = False
        }
    void $ Emulator.waitNSlots 2
