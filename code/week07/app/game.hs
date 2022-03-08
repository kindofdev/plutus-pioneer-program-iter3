{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import Control.Exception                       (throwIO, handle)
import Control.Monad                           (unless)
import Data.Aeson                              (Result (..), fromJSON)
import Data.Monoid                             ()
import Data.Proxy                              (Proxy (..))
import Data.Text                               (pack)
import Data.UUID                               (UUID)
import Control.Monad.IO.Class                  (liftIO)
import Control.Concurrent                      (threadDelay)
import Ledger   
import Network.HTTP.Req   
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse (..))
import Plutus.PAB.Webserver.Types
import System.Environment                      (getArgs)
import Text.Printf                             (printf)
import Wallet.Emulator.Wallet                  (WalletId (..))
import Wallet.Types                            (ContractInstanceId (..))

import Week07.RPS.PAB              
import Week07.RPS.RockPaperScissors  
import Week07.RPS.Utils


main :: IO ()
main = do
    [wid1', wid2', addr1', addr2', playDL', revealDL', choice1', choice2', stake', skipPlayer2', p1', p2'] <- getArgs
    nonce <- randomBuiltinByteString

    let wid1           = unsafeReadWalletId wid1'
        wid2           = unsafeReadWalletId wid2'
        addr1          = unsafeReadAddress $ addr1'
        addr2          = unsafeReadAddress $ addr2'
        (ppkh1, spkh1) = unsafeCredentials addr1
        (ppkh2, spkh2) = unsafeCredentials addr2
        playDL         = read playDL'      :: Integer
        revealDL       = read revealDL'    :: Integer
        choice1        = read choice1'     :: GameChoice
        choice2        = read choice2'     :: GameChoice
        stake          = read stake'       :: Integer
        skipPlayer2    = read skipPlayer2' :: Bool
        p1             = read p1'          :: Int
        p2             = read p2'          :: Int
        fp             = FirstParams
                           { fpFirstPpkh      = ppkh1
                           , fpFirstSpkh      = spkh1
                           , fpSecondPpkh     = ppkh2
                           , fpSecondSpkh     = spkh2
                           , fpStake          = stake
                           , fpPlayDeadline   = POSIXTime playDL 
                           , fpRevealDeadline = POSIXTime revealDL  
                           , fpNonce          = nonce 
                           , fpChoice         = choice1
                           }

    printf "---> player1 starts a game %s\n" (show fp)
    cid1 <- player1Move p1 wid1 fp
    printf "---> player1 runs a contract instance id: %s\n" $ show cid1 

    unless skipPlayer2 $ do
        tt <- getTT p1 $ unContractInstanceId cid1
        let sp = SecondParams
                { spFirstPpkh      = ppkh1
                , spFirstSpkh      = spkh1
                , spSecondPpkh     = ppkh2
                , spSecondSpkh     = spkh2
                , spStake = stake
                , spPlayDeadline = POSIXTime playDL 
                , spRevealDeadline = POSIXTime revealDL  
                , spChoice = choice2 
                , spToken = tt
                }    
        
        threadDelay 10_000_000
        printf "---> player2 plays a game %s\n" (show sp)
        cid2 <- player2Move p2 wid2 sp
        printf "---> player2 runs contract instance id: %s\n" $ show cid2


getTT :: Int -> UUID -> IO ThreadToken
getTT p uuid = handle h $ runReq defaultHttpConfig $ do
    w <- req
        GET
        (http "127.0.0.1" /: "api" /: "contract" /: "instance" /: pack (show uuid) /: "status")
        NoReqBody
        (Proxy :: Proxy (JsonResponse (ContractInstanceClientState RPSContracts)))
        (port p)
    
    liftIO $ case fromJSON $ observableState $ cicCurrentState $ responseBody w of
        Success (Last (Just tt)) -> do
            printf "---> tt found: %s\n" $ show tt
            return tt
        Success (Last Nothing)   -> do
            printf "---> tt not found - trying again in 1 sec\n"
            threadDelay 1_000_000 
            getTT p uuid
        _                        -> throwIO $ userError "error decoding state"
            
  where
    h :: HttpException -> IO ThreadToken
    h ex = do
        printf "---> An exception occurred: %s\n" (show ex)
        threadDelay 1_000_000
        getTT p uuid


player1Move :: Int -> WalletId -> FirstParams -> IO ContractInstanceId
player1Move p wid fp = do
    v <- runReq defaultHttpConfig $ req
        POST
        (http "127.0.0.1" /: "api"  /: "contract" /: "activate")
        (ReqBodyJson $ contractActivationArgs wid $ Player1 fp)
        jsonResponse
        (port p)
    let c = responseStatusCode v
    if c == 200
        then return $ responseBody v
        else throwIO $ userError $ printf "---> ERROR: %d\n" c

player2Move :: Int -> WalletId -> SecondParams -> IO ContractInstanceId
player2Move p wid sp = do
    v <- runReq defaultHttpConfig $ req
        POST
        (http "127.0.0.1" /: "api"  /: "contract" /: "activate")
        (ReqBodyJson $ contractActivationArgs wid $ Player2 sp)
        jsonResponse
        (port p)
    let c = responseStatusCode v
    if c == 200
        then return $ responseBody v
        else throwIO $ userError $ printf "---> ERROR: %d\n" c