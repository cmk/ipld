{-# LANGUAGE TemplateHaskell #-}

module Test.Data.IPLD.Graph where

import Control.Monad.IO.Class
import Crypto.Hash
import Crypto.Hash.IO (HashAlgorithm(..))
import Data.IPLD.Store
import Data.IPLD.Graph.Types
import Data.IORef
import Hedgehog
import Test.Data.IPLD.Store 
import Test.Data.IPLD.Graph.Tree
import Test.Data.IPLD.Graph.List
import qualified Data.Map.Strict as M

storeTestDeep
  :: ( Functor f
     , Traversable f
     , HashAlgorithm h
     , Eq1 f
     , Show1 f
     , MonadIO m
     )
  => Mu f
  -> Store (PropertyT m) h f
  -> PropertyT m ()
storeTestDeep f (getCap, putCap) = do
    h <- uploadDeep putCap f
    r <- strictDeref $ lazyDeref' getCap h
    forget r === f

testHarness 
  :: (MonadIO m, Traversable f, Eq1 f, Show1 f) 
  => (f (Digest Blake2b_256) -> Digest Blake2b_256)
  -> Mu f -> PropertyT m ()
testHarness alg f = do
  ior <- liftIO $ newIORef M.empty
  storeTestDeep f $ inMemoryStore @Blake2b_256 ior alg

-- | generate a nested structure -> deep upload -> strict download -> check (==)
prop_rt_tree :: Property
prop_rt_tree = property $ 
  testHarness hashMockDirTree =<< forAll (gen_dir_tree 5)

prop_rt_list :: Property
prop_rt_list = property $ 
  testHarness hashMockBlockchain =<< forAll (gen_txn_chain 25)

props :: IO Bool
props = checkParallel $$(discover)
