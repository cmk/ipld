{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.IPLD.Graph.Types (
    module Data.IPLD.Graph.Types
  , module Export
) where

import Control.Arrow ((&&&))
import Control.Applicative
import Control.Monad ( liftM , ap, join )
import Data.Foldable
import Data.Functor.Classes
import Data.Traversable
import Prelude hiding ( foldl , foldr , mapM , mapM_ , concat , concatMap )
import Text.Show ()


import Control.Comonad (Comonad(..))

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Data
import GHC.Generics hiding (Infix, Prefix)

-- import qualified Control.Monad.Cotag.Church as CMFC
-- import Control.Monad.Trans.Except (ExceptT(..), runExceptT)


--import Control.Comonad.Trans.Cofree as Export (CofreeF(..))

--import Control.Recursion as Export (Base(..),Recursive(..),Corecursive(..), Fix(..), Mu(..), Nu(..), ListF(..), NonEmptyF(..))

import Yaya.Retrofit as Export hiding (recursiveEq, recursiveShowsPrec)
import Yaya.Fold.Native as Export (Fix(..))
import Yaya.Fold as Export (Mu(..), Nu(..))

import qualified Yaya.Unsafe.Fold as Unsafe
import Control.Monad.Trans.Free
import Control.Comonad.Trans.Cofree
--------------------------------------------------------------------------------



{-
import Yaya.Fold as Export (Mu(..), Nu(..), Projectable(..), Steppable(..), Recursive(..), Corecursive(..))
import Yaya.Fold as Export (CoalgebraPrism, AlgebraPrism, BialgebraIso, DistributiveLaw)
import Yaya.Fold as Export (GCoalgebraM, CoalgebraM, ElgotCoalgebra, GCoalgebra, Coalgebra, ElgotAlgebraM, GAlgebraM, AlgebraM, ElgotAlgebra, GAlgebra, Algebra)
-}

type Distribute f g = forall a. f (g a) -> g (f a)

--------------------------------------------------------------------------------
-- * Annotations

--infixr 5 :<
-- | Type of annotations.
--
-- Equivalent to 'Tag' or 'CofreeF'.
--
--data Tag f a b = a :< f b deriving (Eq, Ord, Show, Typeable, Generic, Generic1)

type Tag f a = CofreeF f a

-- | Extract the head of the base functor
tag :: Tag f a b -> a
tag (a :< _) = a

-- | Extract the tails of the base functor
node :: Tag f a b -> f b
node (_ :< as) = as

runTag :: Tag f a b -> (a, f b)
runTag (a :< as) = (a, as)

-- | Lifting natural transformations to annotations.
liftTag :: (forall x. f x -> g x) -> Tag f a b -> Tag g a b
liftTag fg (a :< fb) = a :< fg fb

distTag
  :: Functor f
  => (f a -> a)
  -> Distribute f w
  -> Distribute f (Tag w a)
distTag phi k = uncurry (:<) . (phi . fmap tag &&& k . fmap node)

{-
instance Functor f => Functor (Tag f a) where
  fmap f (a :< as)  = a :< fmap f as

instance (Monoid a, Applicative f) => Applicative (Tag f a) where
  pure = (mempty :<) . pure
  (af :< asf) <*> (aa :< asa) = (af `mappend` aa) :< (asf <*> asa)

instance Comonad f => Comonad (Tag f a) where
  duplicate (a :< as) = a :< (extend (a :<) as)
  extract (_ :< as) = extract as

instance Foldable f => Foldable (Tag f a) where
  foldMap f (_ :< as) = foldMap f as

instance Traversable f => Traversable (Tag f a) where
  traverse f (a :< as) = (a :<) <$> traverse f as

instance Functor f => Bifunctor (Tag f) where
  bimap f g (a :< as) = f a :< fmap g as

instance Foldable f => Bifoldable (Tag f) where
  bifoldMap f g (a :< as) = f a `mappend` foldMap g as

instance Traversable f => Bitraversable (Tag f) where
  bitraverse f g (a :< as) = (:<) <$> f a <*> traverse g as
-}

instance Comonad g => Projectable (CofreeT f g a) (CofreeF f a) where
  project = extract . runCofreeT

instance (Comonad g, Applicative g) => Steppable (CofreeT f g a) (CofreeF f a) where
  embed = CofreeT . pure


-- | Annotated fixed-point type. Equivalent to @Cofree f a@
--type Tagged f a = Mu (Tag f a)
type Tagged f a = Cofree f a

-- | The attribute of the root node.
toptag :: Functor f => Tagged f a -> a
toptag = tag . project

-- | A function forgetting all the toptags from an annotated tree.
forget :: Functor f => Tagged f a -> Mu f
forget = embed . fmap forget . node . project



--------------------------------------------------------------------------------
-- * Co-annotations

type Cotag f a = FreeF f a
type Cotagged f = Free f

instance Functor f => Recursive (Cofree f a) (CofreeF f a) where
  cata = flip Unsafe.hylo project

-- TODO: check this could be wrong
instance (Functor f, Applicative g, Comonad g) => Corecursive (CofreeT f g a) (CofreeF f a) where
  ana = Unsafe.hylo embed


{-
-- | Categorical dual of 'Tag'.
data Cotag f a b 
  = Pure a 
  | Cotag (f b)
  deriving (Eq, Ord, Show, Typeable, Generic, Generic1)

-- | Categorical dual of 'Tagged'. Equivalent to @Cotag f a@
type Cotagged f a = Nu (Cotag f a)

-- | Lifting natural transformations to annotations.
--
-- Use a natural transformation to transform data between two recursive structures.
-- 
liftCotag :: (forall x. f x -> g x) -> Cotag f a e -> Cotag g a e
liftCotag fg x = case x of
  Pure  x -> Pure  x
  Cotag t -> Cotag (fg t)

unCotag :: Steppable t f => Cotag f t t -> t
unCotag = \case
  Pure t  -> t
  Cotag ft -> embed ft

instance Show1 f => Show2 (Cotag f) where
  liftShowsPrec2 spa _sla _spb _slb d (Pure a) =
    showsUnaryWith spa "Pure" d a
  liftShowsPrec2 _spa _sla spb slb d (Cotag as) =
    showsUnaryWith (liftShowsPrec spb slb) "Cotag" d as

instance (Show1 f, Show a) => Show1 (Cotag f a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Eq1 f => Eq2 (Cotag f) where
  liftEq2 eq _ (Pure a) (Pure b) = eq a b
  liftEq2 _ eq (Cotag as) (Cotag bs) = liftEq eq as bs
  liftEq2 _ _ _ _ = False

instance (Eq1 f, Eq a) => Eq1 (Cotag f a) where
  liftEq = liftEq2 (==)

instance Ord1 f => Ord2 (Cotag f) where
  liftCompare2 cmp _ (Pure a) (Pure b) = cmp a b
  liftCompare2 _ _ (Pure _) (Cotag _) = LT
  liftCompare2 _ _ (Cotag _) (Pure _) = GT
  liftCompare2 _ cmp (Cotag fa) (Cotag fb) = liftCompare cmp fa fb

instance (Ord1 f, Ord a) => Ord1 (Cotag f a) where
  liftCompare = liftCompare2 compare

instance Functor f => Functor (Cotag f a) where
  fmap _ (Pure a)  = Pure a
  fmap f (Cotag as) = Cotag (fmap f as)
  {-# INLINE fmap #-}

instance Foldable f => Foldable (Cotag f a) where
  foldMap f (Cotag as) = foldMap f as
  foldMap _ _         = mempty
  {-# INLINE foldMap #-}

instance Traversable f => Traversable (Cotag f a) where
  traverse _ (Pure a)  = pure (Pure a)
  traverse f (Cotag as) = Cotag <$> traverse f as
  {-# INLINE traverse #-}

instance Functor f => Bifunctor (Cotag f) where
  bimap f _ (Pure a)  = Pure (f a)
  bimap _ g (Cotag as) = Cotag (fmap g as)
  {-# INLINE bimap #-}

instance Foldable f => Bifoldable (Cotag f) where
  bifoldMap f _ (Pure a)  = f a
  bifoldMap _ g (Cotag as) = foldMap g as
  {-# INLINE bifoldMap #-}

instance Traversable f => Bitraversable (Cotag f) where
  bitraverse f _ (Pure a)  = Pure <$> f a
  bitraverse _ g (Cotag as) = Cotag <$> traverse g as
  {-# INLINE bitraverse #-}

-}
--------------------------------------------------------------------------------
-- Shapes

-- | A type encoding the \"shape\" of the functor data:
-- We ignore all the fields whose type is the parameter type,
-- but remember the rest:
--
-- > newtype Shape f = Shape { unShape :: f () }
--
-- This can be used to decide whether two realizations are compatible.
newtype Shape f = Shape { unShape :: f () }

-- | Extracting the \"shape\" of the functor
shape :: Functor f => f a -> Shape f
shape = Shape . fmap (const ())

instance Eq1  f => Eq   (Shape f) where x == y        = liftEq      (==) (unShape x) (unShape y)
instance Ord1 f => Ord  (Shape f) where compare x y   = liftCompare compare (unShape x) (unShape y)

instance (Functor f, Show1 f) => Show (Shape f) where 
  showsPrec d x = showParen (d>app_prec) 
    $ showString "Shape "
    . showsPrec1 (app_prec+1) (fmap (const ()) $ unShape x)
    
--------------------------------------------------------------------------------

app_prec :: Int
app_prec = 10
