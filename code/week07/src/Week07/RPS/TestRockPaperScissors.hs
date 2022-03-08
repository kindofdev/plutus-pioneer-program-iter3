{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Week07.RPS.TestRockPaperScissors
    ( test
    , test'
    , GameChoice (..)
    ) where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import           Ledger.TimeSlot
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude
import           Prelude                    (IO, Show (..))
import           Wallet.Emulator.Wallet

import           Week07.RPS.RockPaperScissors

import Week07.RPS.Utils

test :: IO ()
test = do
    test' Rock     Paper
    test' Paper    Rock
    test' Rock     Scissors 
    test' Scissors Rock
    test' Scissors Paper
    test' Paper Scissors 

test' :: GameChoice -> GameChoice -> IO ()
test' c1 c2 = runEmulatorTraceIO $ myTrace c1 c2

myTrace :: GameChoice -> GameChoice -> EmulatorTrace ()
myTrace c1 c2 = do
    Extras.logInfo $ "first move: " ++ show c1 ++ ", second move: " ++ show c2

    let w1 = knownWallet 1
    let w2 = knownWallet 2

    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints

    let (ppkh1, spkh1) = unsafeCredentials (mockWalletAddress w1)
        (ppkh2, spkh2) = unsafeCredentials (mockWalletAddress w2)
        stake     = 5_000_000
        deadline1 = slotToEndPOSIXTime def 5
        deadline2 = slotToEndPOSIXTime def 10

        fp = FirstParams
                { fpFirstPpkh      = ppkh1
                , fpFirstSpkh      = spkh1
                , fpSecondPpkh     = ppkh2
                , fpSecondSpkh     = spkh2
                , fpStake          = stake
                , fpPlayDeadline   = deadline1
                , fpRevealDeadline = deadline2
                , fpNonce          = "SECRETNONCE"
                , fpChoice         = c1
                }

    callEndpoint @"first" h1 fp

    tt <- getTT h1

    let sp = SecondParams
                { spFirstPpkh      = ppkh1
                , spFirstSpkh      = spkh1
                , spSecondPpkh     = ppkh2
                , spSecondSpkh     = spkh2
                , spStake          = stake
                , spPlayDeadline   = deadline1
                , spRevealDeadline = deadline2
                , spChoice         = c2
                , spToken          = tt
                }

    void $ Emulator.waitNSlots 3

    callEndpoint @"second" h2 sp

    void $ Emulator.waitNSlots 10
  where
    getTT :: ContractHandle (Last ThreadToken) GameSchema Text -> EmulatorTrace ThreadToken
    getTT h = do
        void $ Emulator.waitNSlots 1
        Last m <- observableState h
        case m of
            Nothing -> getTT h
            Just tt -> Extras.logInfo ("read thread token " ++ show tt) >> return tt
