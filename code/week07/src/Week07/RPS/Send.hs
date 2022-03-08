{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Week07.RPS.Send
    ( sendToPubKeyAddress
    ) where

import           Data.Text            (Text)
import           Ledger               ( pubKeyHashAddress, getCardanoTxId, Address )             
import Plutus.Contract as Contract
    ( logError,
      logInfo,
      awaitTxConfirmed,
      ownPaymentPubKeyHash,
      submitTxConstraintsWith,
      utxosAt,
      Contract ) 
import qualified Ledger.Ada            as Ada
import qualified Ledger.Constraints    as Constraints
import qualified Ledger.Typed.Scripts  as Scripts
import           Text.Printf           (printf)

import           Week07.RPS.Utils      ( getCredentials )

sendToPubKeyAddress :: Address -> Integer -> Contract w s Text ()
sendToPubKeyAddress addr n = do
    Contract.logInfo @String $ printf "sending %s lovelaces to address %s" (show n) (show addr)
    ppkh  <- ownPaymentPubKeyHash 
    utxos <- utxosAt $ pubKeyHashAddress ppkh Nothing 
    let cred = getCredentials addr
        tx      = case cred of 
                      Nothing                  -> mempty 
                      Just (ppkh', Nothing)    -> Constraints.mustPayToPubKey ppkh' $ Ada.lovelaceValueOf n 
                      Just (ppkh', Just spkh') -> Constraints.mustPayToPubKeyAddress ppkh' spkh' $ Ada.lovelaceValueOf n
        lookups = Constraints.unspentOutputs utxos

    if tx == mempty
      then Contract.logError @String $ printf "Invalid address - ScriptCredential: %s" (show addr)
      else do 
          txid <- submitTxConstraintsWith @Scripts.Any lookups tx
          _ <- awaitTxConfirmed (getCardanoTxId txid)
          Contract.logInfo @String $ printf "sent %s lovelaces to address %s" (show n) (show addr)
