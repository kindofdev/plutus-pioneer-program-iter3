{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

module Main
    ( main
    ) where

import Control.Monad (void)
import Ledger.Ada qualified as Ada
import Ledger.Index (ValidationError (ScriptFailure))
import Ledger.Scripts (ScriptError (EvaluationError))
import Plutus.Contract (Contract, ContractError)
import Plutus.Contract.Test
import Plutus.Trace.Emulator qualified as Trace
import PlutusTx.Numeric qualified as Numeric
import Test.Tasty

import Week02.Homework2

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "homework 2 tests"
    [ checkPredicate "give and grab - success"
        (     walletFundsChange w1 (Numeric.negate $ Ada.adaValueOf 5)
         .&&. walletFundsChange w2 (Ada.adaValueOf 5)
         .&&. assertNoFailedTransactions
         .&&. assertNotDone theContract (Trace.walletInstanceTag w1) "w1 should be not done"
         .&&. assertNotDone theContract (Trace.walletInstanceTag w2) "w2 should be not done"
        )
        $ do
            hdl1 <- Trace.activateContractWallet w1 theContract
            hdl2 <- Trace.activateContractWallet w2 theContract
            Trace.callEndpoint @"give" hdl1 5_000_000
            void $ Trace.waitNSlots 1
            Trace.callEndpoint @"grab" hdl2 (MyRedeemer True True)
            void $ Trace.waitNSlots 1

    , checkPredicate "give and grab - invalid redeemer"
        (     walletFundsChange w1 (Numeric.negate $ Ada.adaValueOf 5)
         .&&. walletFundsChange w2 mempty
         .&&. assertFailedTransaction (\_ err _ -> 
             case err of { ScriptFailure (EvaluationError ["Wrong value for redeemer", "PT5"] _) -> True; _ -> False })
         .&&. assertNotDone theContract (Trace.walletInstanceTag w1) "w1 should be not done"
        )
        $ do
            hdl1 <- Trace.activateContractWallet w1 theContract
            hdl2 <- Trace.activateContractWallet w2 theContract
            Trace.callEndpoint @"give" hdl1 5_000_000
            void $ Trace.waitNSlots 1
            Trace.callEndpoint @"grab" hdl2 (MyRedeemer True False)
            void $ Trace.waitNSlots 1
    ]        

theContract :: Contract () GiftSchema ContractError ()
theContract = endpoints
