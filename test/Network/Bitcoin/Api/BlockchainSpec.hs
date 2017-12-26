module Network.Bitcoin.Api.BlockchainSpec where

import qualified Network.Bitcoin.Api.Blockchain     as Blockchain
import           Network.Bitcoin.Api.TestUtil (testClient)
import           Test.Hspec
import           Data.Char
import           Data.HexString
import           Data.Text    (unpack)

spec :: Spec
spec = do
  describe "when testing blockchain functions" $ do
      it "can request blockcount" $ do
        r <- testClient Blockchain.getBlockCount
        r `shouldSatisfy` (>= 100)

      it "can request the latest block" $ do
        r <- testClient Blockchain.getBestBlockHash
        (unpack $ toText r) `shouldStartWith` ['0']


      it "can request a block based on its hash" $ do
        hash <- testClient $ \client -> do
          (Blockchain.getBlockHash client) 1
        block <- testClient $ \client -> do
          Blockchain.getBlock client hash
        1 `shouldSatisfy` (== 1)

      it "can request blocks" $ do
         testClient $ \client -> do
           hashes <- mapM (Blockchain.getBlockHash client) [0..10]
           blocks <- mapM (Blockchain.getBlock client) hashes
           fromIntegral (length (blocks)) `shouldBe` 11
