{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

module Week04.Homework where

import Data.Aeson            (FromJSON, ToJSON)
import Data.Functor          (void)
import Data.Text             (Text)
import GHC.Generics          (Generic)
import Ledger
import Ledger.Ada            as Ada
import Ledger.Constraints    as Constraints
import Plutus.Contract       as Contract
import Plutus.Trace.Emulator as Emulator
import Wallet.Emulator.Wallet 

data PayParams = PayParams
    { ppRecipient :: PaymentPubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

payContract :: Contract () PaySchema Text ()
payContract = do
    pp <- awaitPromise $ endpoint @"pay" return
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    Contract.handleError (Contract.logError @Text) (void $ submitTx tx)
    payContract

-- payContract :: Contract () PaySchema Text ()
-- payContract = do
--     pp <- awaitPromise $ endpoint @"pay" return
--     let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
--     Contract.handleError errorHandler (void $ submitTx tx)
--     payContract

-- errorHandler :: Text -> Contract () PaySchema e () 
-- errorHandler = Contract.logError

-- A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with Wallet 2 as
-- recipient, but with amounts given by the two arguments. There should be a delay of one slot
-- after each endpoint call.
payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace v1 v2 = do
    hdl <- activateContractWallet w1 payContract
    callEndpoint @"pay" hdl PayParams 
                              { ppRecipient = mockWalletPaymentPubKeyHash w2
                              , ppLovelace  = v1
                              }
    void $ Emulator.waitNSlots 1
    callEndpoint @"pay" hdl PayParams 
                              { ppRecipient = mockWalletPaymentPubKeyHash w2
                              , ppLovelace  = v2
                              }
    void $ Emulator.waitNSlots 1

w1 :: Wallet
w1 = knownWallet 1

w2 :: Wallet
w2 = knownWallet 2

payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 10_000_000 20_000_000

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000_000_000 20_000_000
