{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Week06.Monitor
    ( monitor
    ) where

import           Data.Functor    (void)
import qualified Data.Map        as Map
import           Data.Monoid     (Last (..))
import           Data.Text       (Text)
import           Plutus.Contract as Contract
import           Ledger
import           Text.Printf     (printf)

monitor :: Address -> Contract (Last Value) Empty Text a
monitor addr = do
    Contract.logInfo @String $ printf "started monitoring address %s" $ show addr
    go
  where
    go = do
        utxos <- utxosAt addr
        Contract.logInfo @String $ printf "UTXOs found %s" $ show $ Map.size utxos
        -- let v = Map.foldl' (\w o -> w <> _ciTxOutValue o) mempty utxos
        let v = foldMap _ciTxOutValue utxos  -- alternative
        tell $ Last $ Just v
        void $ waitNSlots 1  -- for whatever reason blocks here - commenting out works
        go
