{-# LANGUAGE TemplateHaskell #-}

module Test.Data.IPLD.Graph.Store where

import Crypto.Hash
import Crypto.Hash.IO (HashAlgorithm(..))

import           Control.Monad.IO.Class
import           Data.Functor.Compose
import           Data.IORef
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.IPLD.Graph.Store -- (PutCapability, GetCapability, Store, strictDeref, lazyDeref', uploadDeep)
import           Control.Monad.Trans.Maybe

import Control.Comonad.Trans.Cofree

type SSMap h f = Map (Digest h) (f (Digest h))

inMemoryStore
  :: forall h m f
   . ( Functor f
     , Foldable f
     , HashAlgorithm h
     , MonadIO m
     )
  => IORef (SSMap h f)
  -> (f (Digest h) -> Digest h)
  -> Store m h f
inMemoryStore ior hashF =
  ( GetCapability $ \p -> do
          ssmap <- liftIO $ readIORef ior
          lookup' p ssmap
  , PutCapability $ \x -> do
          let p = hashF x
          liftIO $ modifyIORef ior (M.insert p x)
          pure p
  )
  where
    lookup' :: Digest h -> SSMap h f -> MaybeT m (f (Cofree (Compose Maybe f) (Digest h)))
    lookup' p h = 
      let lr = M.lookup p h
          foo = cofree . (:< Compose Nothing)
       in MaybeT . return $ (fmap . fmap) foo lr

