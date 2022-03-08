{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ImportQualifiedPost   #-}

module Week07.RPS.RockPaperScissors
    ( Game (..)
    , GameChoice (..)
    , FirstParams (..)
    , SecondParams (..)
    , GameSchema
    , Last (..)
    , ThreadToken 
    , Text
    , firstGame
    , secondGame
    , endpoints
    ) where

import           Control.Monad                hiding (fmap)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import           Ledger.Typed.Tx
import qualified Ledger.Typed.Scripts         as Scripts
import Plutus.Contract as Contract
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import           Data.OpenApi.Schema          (ToSchema)
import           Prelude                      (Semigroup (..), Show (..), String)
import qualified Prelude

import qualified Data.Map as Map


data Game = Game
    { gFirstPpkh      :: !PaymentPubKeyHash
    , gFirstSpkh      :: !(Maybe StakePubKeyHash)
    , gSecondPpkh     :: !PaymentPubKeyHash
    , gSecondSpkh     :: !(Maybe StakePubKeyHash)
    , gStake          :: !Integer
    , gPlayDeadline   :: !POSIXTime
    , gRevealDeadline :: !POSIXTime
    , gToken          :: !ThreadToken
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''Game

data GameChoice = Rock | Paper | Scissors
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord, Prelude.Read)

instance Eq GameChoice where
    {-# INLINABLE (==) #-}
    Rock     == Rock     = True
    Paper    == Paper    = True
    Scissors == Scissors = True
    _        == _        = False

PlutusTx.unstableMakeIsData ''GameChoice

{-#INLINABLE beats #-}
beats :: GameChoice -> GameChoice -> Bool
beats Rock     Scissors = True
beats Paper    Rock     = True
beats Scissors Paper    = True
beats _        _        = False

data GameDatum = GameDatum BuiltinByteString (Maybe GameChoice) | Finished
    deriving Show

instance Eq GameDatum where
    {-# INLINABLE (==) #-}
    GameDatum bs mc == GameDatum bs' mc' = (bs == bs') && (mc == mc')
    Finished        == Finished          = True
    _               == _                 = False

PlutusTx.unstableMakeIsData ''GameDatum

data GameRedeemer = Play GameChoice | Reveal BuiltinByteString GameChoice | ClaimFirst | ClaimSecond
    deriving Show

PlutusTx.unstableMakeIsData ''GameRedeemer

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE gameDatum #-}
gameDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe GameDatum
gameDatum o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

{-# INLINABLE transition #-}
transition :: Game -> State GameDatum -> GameRedeemer -> Maybe (TxConstraints Void Void, State GameDatum)
transition Game{..} s r = case (stateValue s, stateData s, r) of
    (v, GameDatum bs Nothing, Play c2)
        | lovelaces v == gStake             -> Just ( Constraints.mustBeSignedBy gSecondPpkh                        <>
                                                      Constraints.mustValidateIn (to gPlayDeadline)
                                                    , State (GameDatum bs $ Just c2) (lovelaceValueOf $ 2 * gStake)
                                                    )
    (v, GameDatum _ (Just c2), Reveal _ c1)
        | lovelaces v == (2 * gStake ) &&
          c1 `beats` c2                     -> Just ( Constraints.mustBeSignedBy gFirstPpkh                         <>
                                                      Constraints.mustValidateIn (to gRevealDeadline)               <>
                                                      mustPayTx gFirstPpkh gFirstSpkh v
                                                    , State Finished mempty
                                                    )
    (v, GameDatum _ (Just c2), Reveal _ c1)
        | lovelaces v == (2 * gStake ) &&
          c1 == c2                          -> Just ( Constraints.mustBeSignedBy gFirstPpkh                         <>
                                                      Constraints.mustValidateIn (to gRevealDeadline)               <>
                                                      mustPayTx gFirstPpkh gFirstSpkh (lovelaceValueOf gStake)      <>
                                                      mustPayTx gSecondPpkh gSecondSpkh (lovelaceValueOf gStake)
                                                    , State Finished mempty
                                                    )                                                 
    (v, GameDatum _ Nothing, ClaimFirst)
        | lovelaces v == gStake             -> Just ( Constraints.mustBeSignedBy gFirstPpkh                         <>
                                                      Constraints.mustValidateIn (from $ 1 + gPlayDeadline)         <>
                                                      mustPayTx gFirstPpkh gFirstSpkh v
                                                    , State Finished mempty
                                                    )
    (v, GameDatum _ (Just _), ClaimSecond)
        | lovelaces v == (2 * gStake )      -> Just ( Constraints.mustBeSignedBy gSecondPpkh                        <>
                                                      Constraints.mustValidateIn (from $ 1 + gRevealDeadline)       <>
                                                      mustPayTx gSecondPpkh gSecondSpkh v
                                                    , State Finished mempty
                                                    )
    _                                       -> Nothing
      

{-# INLINABLE mustPayTx #-}
mustPayTx :: PaymentPubKeyHash -> Maybe StakePubKeyHash -> Value -> TxConstraints i o
mustPayTx ppkh mSpkh v = case mSpkh of 
    Nothing   -> Constraints.mustPayToPubKey ppkh v
    Just spkh -> Constraints.mustPayToPubKeyAddress ppkh spkh v


{-# INLINABLE final #-}
final :: GameDatum -> Bool
final Finished = True
final _        = False

{-# INLINABLE check #-}
check :: GameDatum -> GameRedeemer -> ScriptContext -> Bool
check (GameDatum bs (Just _)) (Reveal nonce c1) _ =
    sha2_256 (nonce `appendByteString` choiceToBuiltinByteString c1) == bs
check _                       _              _     = True

{-# INLINABLE gameStateMachine #-}
gameStateMachine :: Game -> StateMachine GameDatum GameRedeemer
gameStateMachine game = StateMachine
    { smTransition  = transition game
    , smFinal       = final
    , smCheck       = check
    , smThreadToken = Just $ gToken game
    }

{-# INLINABLE mkGameValidator #-}
mkGameValidator :: Game -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkGameValidator game = mkValidator $ gameStateMachine game 

{-# INLINABLE choiceToBuiltinByteString #-}
choiceToBuiltinByteString :: GameChoice -> BuiltinByteString
choiceToBuiltinByteString Rock     = "rock"
choiceToBuiltinByteString Paper    = "paper"
choiceToBuiltinByteString Scissors = "scissors"

type Gaming = StateMachine GameDatum GameRedeemer

typedGameValidator :: Game -> Scripts.TypedValidator Gaming
typedGameValidator game = Scripts.mkTypedValidator @Gaming
    ($$(PlutusTx.compile [|| mkGameValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode game)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @GameDatum @GameRedeemer

gameValidator :: Game -> Validator
gameValidator = Scripts.validatorScript . typedGameValidator

gameAddress :: Game -> Ledger.Address
gameAddress = scriptAddress . gameValidator

gameClient :: Game -> StateMachineClient GameDatum GameRedeemer
gameClient game = mkStateMachineClient $ StateMachineInstance (gameStateMachine game) (typedGameValidator game)

data FirstParams = FirstParams
    { fpFirstPpkh      :: !PaymentPubKeyHash
    , fpFirstSpkh      :: !(Maybe StakePubKeyHash)
    , fpSecondPpkh     :: !PaymentPubKeyHash
    , fpSecondSpkh     :: !(Maybe StakePubKeyHash)
    , fpStake          :: !Integer
    , fpPlayDeadline   :: !POSIXTime
    , fpRevealDeadline :: !POSIXTime
    , fpNonce          :: !BuiltinByteString
    , fpChoice         :: !GameChoice
    } deriving (Prelude.Eq, Prelude.Ord, Show, Generic, FromJSON, ToJSON, ToSchema)

mapError' :: Contract w s SMContractError a -> Contract w s Text a
mapError' = mapError $ pack . show

waitUntilTimeHasPassed :: AsContractError e => POSIXTime -> Contract w s e ()
waitUntilTimeHasPassed t = do
    now <- currentTime
    logDebug @String $ "time: " <> show now  
    nowSlot <- currentSlot 
    logDebug @String $ "slot: " <> show nowSlot 

    t'  <- awaitTime t  
    logDebug @String $ "time after deadline: " <> show t'
    nowSlot' <- currentSlot 
    logDebug @String $ "slot after deadline:" <> show nowSlot' 

    void $ waitNSlots 1

firstGame :: forall s. FirstParams -> Contract (Last ThreadToken) s Text ()
firstGame FirstParams{..} = do
    tt        <- mapError' getThreadToken
    logInfo @String $ "tt to be mint: " <> show tt
    
    let ttUtxoRef = ttOutRef tt
    ttUtxoMap <- txOutFromRef ttUtxoRef >>= maybe (throwError "utxo not found") (return . Map.singleton ttUtxoRef) 

    let game = Game
            { gFirstPpkh      = fpFirstPpkh
            , gFirstSpkh      = fpFirstSpkh
            , gSecondPpkh     = fpSecondPpkh
            , gSecondSpkh     = fpSecondSpkh
            , gStake          = fpStake
            , gPlayDeadline   = fpPlayDeadline
            , gRevealDeadline = fpRevealDeadline
            , gToken          = tt
            }
        client      = gameClient game
        v           = lovelaceValueOf fpStake
        c1          = fpChoice 
        bs          = sha2_256 $ fpNonce `appendByteString` choiceToBuiltinByteString c1       
        initLookups = Constraints.unspentOutputs ttUtxoMap
        
    void $ mapError' $ runInitialiseWith initLookups mempty client (GameDatum bs Nothing) v

    logInfo @String $ "made first move: " ++ show c1
    tell $ Last $ Just tt

    waitUntilTimeHasPassed fpPlayDeadline 

    m <- mapError' $ getOnChainState client
    case m of
        Nothing     -> throwError "game output not found"
        Just (o, _) -> case tyTxOutData $ ocsTxOut o of

            GameDatum _ Nothing -> do
                logInfo @String "second player did not play"
                void $ mapError' $ runStep client ClaimFirst
                logInfo @String "first player reclaimed stake"

            GameDatum _ (Just c2) | c1 `beats` c2 -> do
                logInfo @String "second player played and lost"
                void $ mapError' $ runStep client $ Reveal fpNonce c1
                logInfo @String "first player revealed and won"

            GameDatum _ (Just c2) | c1 == c2 -> do
                logInfo @String "first player and second player draw"
                void $ mapError' $ runStep client $ Reveal fpNonce c1
                logInfo @String "first player revealed and draw"

            _ -> logInfo @String "second player played and won"

data SecondParams = SecondParams
    { spFirstPpkh      :: !PaymentPubKeyHash
    , spFirstSpkh      :: !(Maybe StakePubKeyHash)
    , spSecondPpkh     :: !PaymentPubKeyHash
    , spSecondSpkh     :: !(Maybe StakePubKeyHash)
    , spStake          :: !Integer
    , spPlayDeadline   :: !POSIXTime
    , spRevealDeadline :: !POSIXTime
    , spChoice         :: !GameChoice
    , spToken          :: !ThreadToken
    } deriving (Prelude.Eq, Prelude.Ord, Show, Generic, FromJSON, ToJSON, ToSchema)

secondGame :: forall w s. SecondParams -> Contract w s Text ()
secondGame SecondParams{..} = do
    let game = Game
            { gFirstPpkh      = spFirstPpkh
            , gFirstSpkh      = spFirstSpkh
            , gSecondPpkh     = spSecondPpkh
            , gSecondSpkh     = spSecondSpkh
            , gStake          = spStake 
            , gPlayDeadline   = spPlayDeadline
            , gRevealDeadline = spRevealDeadline
            , gToken          = spToken
            }
        client = gameClient game
    m <- mapError' $ getOnChainState client
    case m of
        Nothing     -> logInfo @String "no running game found"
        Just (o, _) -> case tyTxOutData $ ocsTxOut o of
            GameDatum _ Nothing -> do
                logInfo @String "running game found"
                void $ mapError' $ runStep client $ Play spChoice 
                logInfo @String $ "made second move: " ++ show spChoice

                waitUntilTimeHasPassed spRevealDeadline

                m' <- mapError' $ getOnChainState client
                case m' of
                    Nothing -> logInfo @String "first player won or draw"
                    Just _  -> do
                        logInfo @String "first player didn't reveal"
                        void $ mapError' $ runStep client ClaimSecond
                        logInfo @String "second player won"

            _                   -> throwError "unexpected datum"

type GameSchema = Endpoint "first" FirstParams .\/ Endpoint "second" SecondParams

endpoints :: Contract (Last ThreadToken) GameSchema Text ()
endpoints = awaitPromise (first `select` second) >> endpoints
  where
    first  = endpoint @"first"  firstGame
    second = endpoint @"second" secondGame
