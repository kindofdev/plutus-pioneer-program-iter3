{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Week07.RPS.Monitor
    ( monitor
    ) where

import           Data.Functor       (void)
import qualified Data.Map        as Map
import           Data.Monoid        (Last (..))
import           Data.Text          (Text)
import           Ledger             ( ChainIndexTxOut(_ciTxOutValue), Address, Value )
import           Plutus.Contract as Contract ( tell, logInfo, utxosAt, waitNSlots, Contract, Empty )
import           Text.Printf        (printf)

monitor :: Address -> Contract (Last Value) Empty Text a
monitor addr = do
    Contract.logInfo @String $ printf "started monitoring address %s" $ show addr
    go
  where
    go = do
        utxos <- utxosAt addr
        Contract.logInfo @String $ printf "UTXOs found %s" $ show $ Map.size utxos
        let v = foldMap _ciTxOutValue utxos
        tell $ Last $ Just v
        void $ waitNSlots 1
        go
