{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Week07.RPS.PAB 
    ( RPSContracts (..)
    , Address
    ) where

import           Data.Aeson                                (FromJSON, ToJSON)
import           Data.OpenApi.Schema                       (ToSchema)
import           GHC.Generics                              (Generic)
import           Ledger                                    ( Address, POSIXTime(POSIXTime), TxOutRef(TxOutRef), TxId(TxId) )                              
import           Plutus.Contract.StateMachine.ThreadToken  ( ThreadToken(..) )
import           Plutus.PAB.Effects.Contract.Builtin       (Empty, HasDefinitions (..), SomeBuiltin (..), endpointsToSchemas)
import           Prettyprinter                             (Pretty (..), viaShow)
import           Wallet.Emulator.Wallet                    ( knownWallet, mockWalletAddress )              

import qualified Week07.RPS.Monitor as Monitor
import qualified Week07.RPS.Send    as Send
import Week07.RPS.RockPaperScissors
    ( GameChoice(Rock),
      SecondParams(..),
      FirstParams(..),
      firstGame,
      secondGame )      
import Week07.RPS.Utils ( unsafeCredentials )


data RPSContracts = Player1 FirstParams 
                  | Player2 SecondParams 
                  | Monitor Address
                  | Send Address Integer
                  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON, ToSchema)

instance Pretty RPSContracts where
    pretty = viaShow

instance HasDefinitions RPSContracts where

    getDefinitions               = [Player1 exampleFirstParams, Player2 exampleSecondParams, Monitor exampleAddr]

    getContract (Player1 fp)   = SomeBuiltin $ firstGame @Empty fp
    getContract (Player2 sp)   = SomeBuiltin $ secondGame @() @Empty sp
    getContract (Monitor addr) = SomeBuiltin $ Monitor.monitor addr
    getContract (Send addr n)  = SomeBuiltin $ Send.sendToPubKeyAddress @() @Empty addr n

    getSchema = const $ endpointsToSchemas @Empty

exampleFirstParams :: FirstParams 
exampleFirstParams = FirstParams
    { fpFirstPpkh      = fst $ unsafeCredentials (mockWalletAddress $ knownWallet 1)
    , fpFirstSpkh      = snd $ unsafeCredentials (mockWalletAddress $ knownWallet 1)
    , fpSecondPpkh     = fst $ unsafeCredentials (mockWalletAddress $ knownWallet 2)
    , fpSecondSpkh     = snd $ unsafeCredentials (mockWalletAddress $ knownWallet 2)
    , fpStake          = 5_000_000
    , fpPlayDeadline   = POSIXTime 1_000_000 
    , fpRevealDeadline = POSIXTime 2_000_000  
    , fpNonce          = "SECRET_NONCE" 
    , fpChoice         = Rock 
    }

exampleSecondParams :: SecondParams 
exampleSecondParams = SecondParams
    { spFirstPpkh      = fst $ unsafeCredentials (mockWalletAddress $ knownWallet 1)
    , spFirstSpkh      = snd $ unsafeCredentials (mockWalletAddress $ knownWallet 1)
    , spSecondPpkh     = fst $ unsafeCredentials (mockWalletAddress $ knownWallet 2)
    , spSecondSpkh     = snd $ unsafeCredentials (mockWalletAddress $ knownWallet 2)
    , spStake          = 5_000_000
    , spPlayDeadline   = POSIXTime 1_000_000  
    , spRevealDeadline = POSIXTime 2_000_000  
    , spChoice         = Rock 
    , spToken          = exampleThreadToken 
    }

exampleAddr :: Address
exampleAddr = mockWalletAddress $ knownWallet 1

exampleThreadToken :: ThreadToken 
exampleThreadToken = ThreadToken 
    { ttOutRef = TxOutRef (TxId "myTxId") 0
    , ttCurrencySymbol = "myCurrencySymbol"
    }
