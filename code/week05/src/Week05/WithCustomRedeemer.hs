{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week05.WithCustomRedeemer where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Prelude                (IO, Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

import           Week05.WithCustomRedeemerTypes
 
{-# INLINABLE mkNftPolicy #-}
mkNftPolicy :: CustomRedeemer -> ScriptContext -> Bool
mkNftPolicy CustomRedeemer{nftName} _ = 
    let expected = TokenName "kurt"
    in traceIfFalse "Kurt is sad because the nft is not called kurt :(" $ nftName == expected 

policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkNftPolicy ||])

curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy


data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmount    :: !Integer
    } deriving (Generic, ToJSON, FromJSON)

type FreeSchema = Endpoint "mint" MintParams

mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do
    let tn = mpTokenName mp
        redeemer = Redeemer $ PlutusTx.toBuiltinData $ CustomRedeemer tn "a description"
        val      = Value.singleton curSymbol (mpTokenName mp) (mpAmount mp)
        lookups  = Constraints.mintingPolicy policy
        tx       = Constraints.mustMintValueWithRedeemer redeemer val
    ledgerTx <- submitTxConstraintsWith @Scripts.Any lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () FreeSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

happyKurt :: IO ()
happyKurt = runEmulatorTraceIO $ do
    let tn = "kurt"
    h1 <- activateContractWallet (knownWallet 1) endpoints
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 555
        }
    void $ Emulator.waitNSlots 1

sadKurt :: IO ()
sadKurt = runEmulatorTraceIO $ do
    let tn = "bob"
    h1 <- activateContractWallet (knownWallet 1) endpoints
    callEndpoint @"mint" h1 $ MintParams
        { mpTokenName = tn
        , mpAmount    = 555
        }
    void $ Emulator.waitNSlots 1   

