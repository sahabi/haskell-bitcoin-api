{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module Network.Bitcoin.Api.Types.ChainTips where

import qualified Data.HexString                     as HS
import qualified Data.Bitcoin.Block                 as Btc
import qualified Data.Bitcoin.Types                 as BT
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import           Data.Word              (Word32)
import           GHC.Generics           (Generic)
import qualified Data.Text           as T

-- | Contains information about a block header, as returned by the
--   "getblockheader" RPC command, without the actual header.
data Tip = Tip
   { height      :: Integer
   , hash        :: BT.BlockHash
   , branchlen   :: Integer
   , status      :: T.Text
   } deriving
        (Eq, Show)


instance FromJSON Tip where
  parseJSON = withObject "Tip" $ \o -> Tip
     <$> o .:  "height"
     <*> o .:  "hash"
     <*> o .:  "branchlen"
     <*> o .:  "status"

type ChainTips = [Tip]
