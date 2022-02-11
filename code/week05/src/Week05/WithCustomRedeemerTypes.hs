{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE NoImplicitPrelude   #-}

module Week05.WithCustomRedeemerTypes where

import           PlutusTx.Prelude
import           Prelude          (Show (..))
import qualified PlutusTx
import           Ledger           (TokenName)

data CustomRedeemer = CustomRedeemer
    { nftName     :: TokenName 
    , description :: BuiltinByteString
    } deriving Show

PlutusTx.unstableMakeIsData ''CustomRedeemer 
