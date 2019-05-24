module Data.IPLD.Store where

--import           Control.Exception.Safe (MonadThrow, throwString)
import Control.Comonad.Trans.Cofree
import Control.Monad.Trans.Free
import Control.Monad.Trans.Maybe
import Crypto.Hash 
-- import Data.IPLD.Graph.Hash
import Data.IPLD.Graph.Types
import Data.Functor.Compose
import Yaya.Fold
import Yaya.Unsafe.Fold (anaM)
import Yaya.Unsafe.Zoo (futu)
import Yaya.Zoo (cataM)


--TODO replace w/ multihash?
type Hashed f a = Cofree f (Digest a)
-- type Hashed f m a = CofreeT f m (Digest a)

type TreeF m f h = CofreeF (Compose m f) (Digest h)
--type Tree m f h = CofreeT f (MaybeT m) (Digest h)

--type Trie f m h = Hashed (Compose (MaybeT m) f) h

type Store m h f = (GetCapability m h f, PutCapability m h f)

type ShallowStore m h f = (GetCapabilityShallow m h f, PutCapability m h f)

data GetCapability m h f
  = GetCapability
  { gcGet :: Digest h -> MaybeT m (f (Hashed (Compose Maybe f) h))
  }

data GetCapabilityShallow m h f
  = GetCapabilityShallow
  { gcGetShallow :: Digest h -> MaybeT m (f (Digest h))
  }

data PutCapability m h f
  = PutCapability
  { gcPut :: f (Digest h) -> m (Digest h)
  }

uploadDeep
  :: (Traversable f, Monad m)
  => PutCapability m h f
  -> (Mu f)
  -> m (Digest h)
uploadDeep = cataM . gcPut

strictDeref
  :: (Traversable f, Monad m)
  => Hashed (Compose m f) h
  -> m (Hashed f h)
strictDeref = anaM coalg
  where   -- Hashed (Compose m f) h -> m (Tag f (Digest h) (Hashed (Compose m f) h))
    --coalg :: CoalgebraM m (Tag f (Digest h)) (Hashed (Compose m f) h)
    coalg a = do
      let p :< (Compose eff) = project a
      x <- eff
      return $ p :< x

-- | construct a potentially-infinite tree-shaped stream of further values constructed by
-- deref-ing hash pointers using a hash-addressed store. Allows for store returning multiple
-- layers of tree structure in a single response (to enable future optimizations) via 'Free'
lazyDeref
  :: Monad m
  => Functor f
  => GetCapability m h f 
  -> Digest h
  -> Hashed (Compose (MaybeT m) f) h
lazyDeref (GetCapability get) = futu alg
  where
{-
    alg :: GCoalgebra (Free (TreeF (MaybeT m) f h)
                      (TreeF (MaybeT m) f h)
                      (Digest h)
-}
    alg p = (p :<) . Compose $ fmap (cata helper) <$> get p 

    helper 
      :: (Functor f, Monad m)
      => Algebra (TreeF Maybe f h) 
                 (Free (TreeF (MaybeT m) f h) (Digest h))
    helper (h :< (Compose Nothing))  = free $ Pure h
    helper (h :< (Compose (Just x))) = free $ Free $ h :< (Compose $ pure $ x)


lazyDeref'
  :: forall m h f
   . ( Monad m
  --   , MonadThrow m
     , Functor f
     )
  => GetCapability m h f
  -> Digest h
  -> Hashed (Compose m f) h
lazyDeref' store = cata alg . lazyDeref store
  where
    alg :: Algebra (CofreeF (Compose (MaybeT m) f) (Digest h))
                   (Hashed (Compose m f) h)
    alg (h :< (Compose eff)) = embed . (h :<) . Compose $ do
      res <- runMaybeT eff
      maybe (error $ "lookup error: " ++ show h) pure res -- TODO fix this



{-

liftShallowStore
  :: forall m h f
   . (Monad m, Functor f)
  => ShallowStore m h f
  -> Store m h f
liftShallowStore (g,p) = (liftShallowGetCap g, p)

liftShallowGetCap
  :: forall m h f
   . (Monad m, Functor f)
  => GetCapabilityShallow m h f
  -> GetCapability m h f
liftShallowGetCap (GetCapabilityShallow g) = GetCapability g'
  where
    g' :: Hash h f -> m (Maybe (DerefRes f h))
    g' h =
        g h >>= \case
          Nothing -> pure Nothing
          Just x -> pure . Just $ fmap (\p' -> Fix $ Compose (p', Compose Nothing)) x


-- technically store is now a semigroup
-- TODO: write fallback vals to original cache! (or don't, it makes laziness more observable..)
withFallback :: Monad m => GetCapability m h f -> GetCapability m h f -> GetCapability m h f
withFallback main fallback = GetCapability get
  where
    get h = do
      gcGet main h >>= \case
        Just mainRes -> pure $ Just mainRes
        Nothing -> gcGet fallback h


liftStore :: (forall x. m x -> m' x) -> Store m h f -> Store m' h f
liftStore f (g,p) = (liftGetCap f g, liftPutCap f p)

liftGetCap :: (forall x. m x -> m' x) -> GetCapability m h f -> GetCapability m' h f
liftGetCap f (GetCapability g) = GetCapability (f . g)

liftPutCap :: (forall x. m x -> m' x) -> PutCapability m h f -> PutCapability m' h f
liftPutCap f (PutCapability g) = PutCapability (f . g)
-}
