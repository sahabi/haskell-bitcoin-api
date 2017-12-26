module Network.Bitcoin.Api.TestUtil (testClient) where

import qualified Data.Text                                    as T (pack)
import           Control.Lens                                 ((^.))
import           Network.Wreq.Lens                            (statusCode)

import           Network.Bitcoin.Api.Client

testClient :: (Client -> IO a) -> IO a
testClient = withClient "127.0.0.1" 8332 (T.pack "sahabi") (T.pack "5555")
