{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module Network.Bitcoin.Api.Types.BlockChainInfo where

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
data BlockChainInfo = BlockChainInfo
   { chain           :: T.Text
   , blocks          :: Integer
   , headers         :: Integer
   , bestblockhash   :: BT.BlockHash
   , difficulty      :: Double
   } deriving
        (Eq, Show) -- , Generic, FromJSON, ToJSON)


instance FromJSON BlockChainInfo where
  parseJSON = withObject "BlockChainInfo" $ \o -> BlockChainInfo
     <$> o .:  "chain"
     <*> o .:  "blocks"
     <*> o .:  "headers"
     <*> o .:  "bestblockhash"
     <*> o .:  "difficulty"

