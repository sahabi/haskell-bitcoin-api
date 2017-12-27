module Network.Bitcoin.Api.BlockchainSpec where

import           Prelude                         as P
import           Control.Lens
import qualified Network.Bitcoin.Api.Blockchain     as BC
import           Network.Bitcoin.Api.TestUtil (testClient)
import           Network.Bitcoin.Api.Types.BlockChainInfo
import           Network.Bitcoin.Api.Types.ChainTips as CT
import           Network.Bitcoin.Api.Types.HeaderInfo  as HDI
import           Test.Hspec
import           Data.Bitcoin.Block  (blockHeader, merkleRoot)
import           Data.HexString
import           Data.Text                   as T
--import           Data.ByteString.Base16
import           Data.ByteString.Char8 as BS
import           Data.LargeWord
spec :: Spec
spec = do
  describe "when testing blockchain functions" $ do

      it "can request the latest block" $ do
        r <- testClient BC.getBestBlockHash
        (T.unpack $ toText r) `shouldStartWith` ['0']

      it "can request a block based on its hash (main)" $ do
        r <- testClient $ \client -> do
          BC.getBlock client $ hexString $ BS.pack "00000000839a8e6886ab5951d76f411475428afc90947ee320161bbf18eb6048"
        o <- case r of
               Just x -> return (x^.blockHeader.merkleRoot)
               Nothing -> return(0)
        o `shouldSatisfy` (== (68808657956240486906676849581291559270304947746101859651009505967123642662414 :: Word256))

      it "can request a block based on its hash (main)" $ do
        h <- testClient $ \client -> do
          (BC.getBlockHash client) 0
        r <- testClient $ \client -> do
          BC.getBlock client h
        P.putStrLn $ show r
        o <- case r of
               Just x -> return (x^.blockHeader.merkleRoot)
               Nothing -> return(0)
        o `shouldSatisfy` (== (26976096685621018606398710717950663031556786548057122896278395450162841280074 :: Word256))

      it "can request multiple blocks successfully" $ do
        testClient $ \client -> do
          hs <- mapM (BC.getBlockHash client) [0..10]
          bs <- mapM (BC.getBlock client) hs
          fromIntegral (P.length (bs)) `shouldBe` 11

      it "can request info: the current state of the chain (main)" $ do
        r <- testClient BC.getBlockChainInfo
        (T.unpack $ chain r) `shouldSatisfy` (== "main")

      it "can request blockcount" $ do
        r <- testClient BC.getBlockCount
        r `shouldSatisfy` (>= 100)

      it "can request the block hash from height" $ do
        r <- testClient $ \client -> do
          (BC.getBlockHash client) 1
        r `shouldSatisfy`  (== (hexString $ BS.pack "00000000839a8e6886ab5951d76f411475428afc90947ee320161bbf18eb6048"))

      it "can request the block header from the block hash" $ do
        h <- testClient $ \client -> do
          (BC.getBlockHash client) 0
        r <- testClient $ \client -> do
          (BC.getBlockHeader client) h
        case r of
          Just x -> x `shouldSatisfy` (== (hexString $ BS.pack "0100000000000000000000000000000000000000000000000000000000000000000000003ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a29ab5f49ffff001d1dac2b7c"))
          Nothing -> error "we got nothing"


      it "can request info: highest-height block of each local block chain" $ do
        r <- testClient BC.getChainTips
        (CT.height $ P.head r) `shouldSatisfy` (> 500000)


      it "can request info about a block header from block hash" $ do
        h <- testClient $ \client -> do
          (BC.getBlockHash client) 0
        r <- testClient $ \client -> do
          (BC.getBlockHeaderInfo client) h
        case r of
          Just x -> (HDI.height x) `shouldSatisfy` (== 0)
          Nothing -> error "RPC error"


