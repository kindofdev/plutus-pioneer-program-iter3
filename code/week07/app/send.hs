{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main
    ( main
    ) where

import Control.Concurrent                      (threadDelay)
import Control.Exception                       (throwIO)
import Control.Monad                           (when, void)
import Data.Aeson                              (FromJSON (..))
import Data.Aeson.Types                        (parseMaybe)
import Data.Maybe                              (fromMaybe)
import Data.Monoid                             (Last (..))
import Data.Text                               (pack)
import Network.HTTP.Req
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse (..))
import Plutus.PAB.Webserver.Types              (ContractInstanceClientState (..))
import Plutus.V1.Ledger.Value                  (Value, flattenValue)
import System.Environment                      (getArgs)
import System.IO                               (stdout, hFlush)
import Text.Printf                             (printf)
import Wallet.Emulator.Wallet                  (WalletId (..))
import Week07.RPS.PAB                          (Address, RPSContracts (..))
import Wallet.Types                            (ContractInstanceId (..))
import Week07.RPS.Utils                        (cidToString, contractActivationArgs, unsafeReadAddress, unsafeReadWalletId)

type Port      = Int
type Lovelaces = Integer

main :: IO ()
main = do
    [wid', port', addr', n'] <- getArgs
    let wid  = unsafeReadWalletId wid'
        addr = unsafeReadAddress addr'
        p    = read port' :: Int
        n    = read n'    :: Integer
    printf "sending %s lovelaces to address %s on wallet %s\n" (show n) (show addr) (show wid)
    cid                       <- send p wid addr n
    printf "started send-contract with contract id %s\n\n" $ cidToString cid

send :: Port -> WalletId -> Address -> Lovelaces -> IO ContractInstanceId
send p wid addr n = do
    v <- runReq defaultHttpConfig $ req
        POST
        (http "127.0.0.1" /: "api"  /: "contract" /: "activate")
        (ReqBodyJson $ contractActivationArgs wid $ Send addr n)
        jsonResponse
        (port p)
    let c = responseStatusCode v
    when (c /= 200) $
        throwIO $ userError $ printf "ERROR: %d\n" c
    return $ responseBody v
