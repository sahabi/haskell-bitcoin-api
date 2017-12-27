module Network.Bitcoin.Api.BlockchainSpec where

import           Control.Lens
import qualified Network.Bitcoin.Api.Blockchain     as Blockchain
import           Network.Bitcoin.Api.TestUtil (testClient)
import           Network.Bitcoin.Api.Types.BlockChainInfo
import           Test.Hspec
import           Data.Bitcoin.Block  (blockHeader, merkleRoot)
import           Data.HexString
import           Data.Text                   as T
import           Data.ByteString.Base16
import           Data.ByteString.Char8 as BS

spec :: Spec
spec = do
  describe "when testing blockchain functions" $ do

      it "can request the latest block" $ do
        r <- testClient Blockchain.getBestBlockHash
        (T.unpack $ toText r) `shouldStartWith` ['0']

      it "can request a block based on its hash (main)" $ do
        r <- testClient $ \client -> do
          Blockchain.getBlock client $ hexString $ BS.pack "00000000839a8e6886ab5951d76f411475428afc90947ee320161bbf18eb6048"
        o <- case r of
               Just x -> return (x^.blockHeader.merkleRoot)
        o `shouldSatisfy` (== 68808657956240486906676849581291559270304947746101859651009505967123642662414)

      it "can request multiple blocks successfully" $ do
        testClient $ \client -> do
          hs <- mapM (Blockchain.getBlockHash client) [0..10]
          bs <- mapM (Blockchain.getBlock client) hs
          fromIntegral (Prelude.length (bs)) `shouldBe` 11

      it "can request blockcount" $ do
        r <- testClient Blockchain.getBlockCount
        r `shouldSatisfy` (>= 100)

      it "can request info: the current state of the chain (main)" $ do
        r <- testClient Blockchain.getBlockChainInfo
        (T.unpack $ chain r) `shouldSatisfy` (== "main")

      it "can request a block based on its hash (main)" $ do
        h <- testClient $ \client -> do
          (Blockchain.getBlockHash client) 0
        r <- testClient $ \client -> do
          Blockchain.getBlock client h
        Prelude.putStrLn $ show r
        o <- case r of
               Just x -> return (x^.blockHeader.merkleRoot)
        o `shouldSatisfy` (== 26976096685621018606398710717950663031556786548057122896278395450162841280074)


