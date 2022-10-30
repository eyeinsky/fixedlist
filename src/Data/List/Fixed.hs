module Data.List.Fixed
  ( module Data.List.Fixed
  , module Data.Peano
  ) where

import Prelude
import Data.Kind
import Data.Proxy
import Data.Kind (Type)
import Data.Coerce
import GHC.TypeLits

import Data.Peano

-- * Fixed size list

data a :* as where (:*) :: a -> as -> a :* as

infixr 5 :*

type family PeanoListF p a = li where
  PeanoListF Zero a = a
  PeanoListF (Succ p) a = a :* PeanoListF p a

newtype PeanoList p a = PeanoList (PeanoListF p a)

-- ** Instances

deriving instance (Show a, Show as) => Show (a :* as)

-- *** Functor

instance Functor (PeanoList Zero) where
  fmap :: forall a b. (a -> b) -> PeanoList Zero a -> PeanoList Zero b
  fmap f (PeanoList (a :: a)) = let b = f a :: b in PeanoList b

instance (Functor (PeanoList p)) => Functor (PeanoList (Succ p)) where
  fmap
    :: forall a b. (Functor (PeanoList p))
    => (a -> b) -> PeanoList (Succ p) a -> PeanoList (Succ p) b
  fmap f (PeanoList li) = let
    (a :* as) = li
    in PeanoList (f a :* coerce (fmap f (coerce as :: PeanoList p a)))

-- *** Foldable

instance Foldable (PeanoList Zero) where
  foldMap :: forall a m. (Monoid m) => (a -> m) -> PeanoList Zero a -> m
  foldMap f (PeanoList a) = f a

instance Foldable (PeanoList p) =>  Foldable (PeanoList (Succ p)) where
  foldMap :: forall a m. (Monoid m) => (a -> m) -> PeanoList (Succ p) a -> m
  foldMap f (PeanoList (a :* as)) =
    f a <> foldMap f (PeanoList as :: PeanoList p a)

-- *** Traversable

instance Traversable (PeanoList Zero) where
  traverse f (PeanoList a) = PeanoList <$> f a

instance
   ( Functor (PeanoList p)
   , Foldable (PeanoList p)
   , Traversable (PeanoList p)
   ) => Traversable (PeanoList (Succ p)) where
  traverse
    :: forall f a b. Applicative f
    => (a -> f b) -> PeanoList (Succ p) a -> f (PeanoList (Succ p) b)
  traverse f (PeanoList (a :* as)) = let
    bs' = traverse f (PeanoList as) :: f (PeanoList p b)
    bs = coerce <$> bs' :: f (PeanoListF p b)
    in PeanoList <$> ((:*) <$> f a <*> bs)

-- * Generate

type family Element li where
  Element (a :* as) = a
  Element a = a

class Generate li where
  generate :: Monad m => m (Element li) -> m li

instance (Element a ~ a) => Generate a where
  generate m = m

instance {-# OVERLAPPING #-} (a ~ Element as, Generate as) => Generate (a :* as) where
  generate m = do
    a <- m
    as <- generate m
    return (a :* as)

-- | Generalte fixed-sized list from executing @m@ @(n :: Nat)@ times
type NatListC n a =
  ( Generate (PeanoListF (Prev (ToPeano n)) a)
  , Element (PeanoListF (Prev (ToPeano n)) a) ~ a)
type NatList n a = PeanoList (Prev (ToPeano n)) a
genList
  :: forall (n :: Nat) (a :: Type) (m :: Type -> Type).
   ( Monad m, NatListC n a )
  => Proxy n -> m a -> m (NatList n a)
genList _ m = PeanoList <$> generate m
