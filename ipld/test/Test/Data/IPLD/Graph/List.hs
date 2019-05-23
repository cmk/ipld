{-# LANGUAGE TemplateHaskell #-}

module Test.Data.IPLD.Graph.List where

import Crypto.Hash
import Crypto.Hash.IO (HashAlgorithm(..))
import Data.IPLD.Graph.Types
import Data.Eq.Deriving
import GHC.Generics
import Hedgehog
import Text.Show.Deriving
import qualified Data.Aeson as AE
import qualified Data.ByteString.Char8 as B
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Yaya.Unsafe.Fold as Unsafe


data Txn = Txn { from :: String, to :: String, amt :: Int }
  deriving (Eq, Ord, Show, Generic)

instance AE.ToJSON Txn
instance AE.FromJSON Txn

data MockBlockchain a = Block [Txn] a | GenesisBlock
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic1)

instance AE.ToJSON1 MockBlockchain
instance AE.FromJSON1 MockBlockchain
$(deriveShow1 ''MockBlockchain)
$(deriveEq1   ''MockBlockchain)

hashMockBlockchain :: HashAlgorithm h => MockBlockchain (Digest h) -> Digest h
hashMockBlockchain (Block txns d) = hashFinalize $ 
  hashUpdate (hashUpdates hashInit $ fmap (B.pack . show) txns) d
hashMockBlockchain GenesisBlock = hash B.empty

genBlockAlg :: MonadGen m => Int -> m (MockBlockchain Int)
genBlockAlg n = if n <= 0 then pure GenesisBlock else genBlock
  where
    genBlock = flip Block (n -1) <$> Gen.list (Range.constant 1 5) genTxn
    genTxn = do
      from' <- Gen.string (Range.singleton 10) Gen.alphaNum
      to' <- Gen.string (Range.singleton 10) Gen.alphaNum
      amt' <- Gen.integral (Range.constant 0 100000)
      pure $ Txn from' to' amt'


gen_txn_chain :: (MonadGen m, Steppable t MockBlockchain) => Int -> m t
gen_txn_chain = Unsafe.anaM genBlockAlg

