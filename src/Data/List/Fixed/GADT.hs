{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Data.List.Fixed.GADT
  ( module Data.List.Fixed.GADT
  , module Data.Peano
  ) where

import Prelude qualified as P
-- import Data.Proxy
import Data.Kind (Type)
-- import Data.Coerce
-- import GHC.TypeLits
import Data.Peano


data List (n :: Peano) (a :: Type) where
  Nil :: List Zero a
  (:|) :: a -> List n a -> List (Succ n) a

infixr 5 :|

deriving instance P.Show a => P.Show (List n a)

-- | TODO
(++) :: forall n n' x . List n x -> List n' x -> List (Add n n') x
a ++ b = case a of
  Nil -> b

  (a :: x) :| (as :: (Succ m ~ n) => List m x) -> let

    _tail = as ++ b :: List (Add m n') x
    _full = a :| _tail :: List (Succ (Add m n')) x
    -- • Could not deduce: Add m ('Succ n') ~ 'Succ (Add m n')
    --   from the context: n ~ 'Succ m

    res = P.undefined :: (Succ m ~ n) => List (Add n n') x
    in res -- Cons a (as ++ b)
-- ^ [ref1] Depends on the specific case order of arguments for Add in Data.Peano

head :: List (Succ n) x -> x
head = \case x :| _ -> x

last :: List (Succ n) x -> x
last = \case
  x :| xs -> case xs of
    Nil -> x
    _ :| _ -> last xs

init :: forall n x . List (Succ n) x -> List n x
init = \case
  x :| xs -> case xs of
    Nil -> Nil
    -- _ -> x :| init xs -- Doesn't work, need to match on :| to get the constraint.
    _ :| _ -> x :| init xs

-- uncons :: [a] -> Maybe (a, [a])

singleton :: x -> List (Succ Zero) x
singleton x = x :| Nil

-- null :: Foldable t => t a -> Bool

length :: List n x -> P.Int
length = \case
  Nil -> 0
  _ :| xs -> let !n = length xs in P.succ n

map :: (x -> y) -> List n x -> List n y
map f = \case
  Nil -> Nil
  x :| xs -> f x :| map f xs

reverse :: forall n x . List n x -> List n x
reverse xs =
  -- todo
  reverse1 xs Nil
  -- bla xs Nil
  -- • Couldn't match type ‘n’ with ‘Add n 'Zero’
  --   Expected: List n x
  --     Actual: List (Add n 'Zero) x

reverse1
  :: forall a b x
   . List a x -> List b x -> List (Add a b) x
reverse1 as' bs' = case as' of
  a :| as -> let
    _ = as :: (Succ a' ~ a) => List a' x
    _bs = a :| bs' :: List (Succ b) x
    _ = reverse1 as _bs
    in reverse1 as (a :| bs')
  Nil -> bs'

reverse2 :: forall a b x . List a x -> List b x -> List (Add a b) x
reverse2 as bs = case as of
  Nil -> bs
  _ :| _ -> let
    (as', bs') = move as bs :: (Succ a' ~ a) => (List a' x, List (Succ b) x)
    in case as' of
         Nil -> bs'
         _ :| _ -> reverse2 as' bs'

  where
    move :: forall a b x
       . List (Succ a) x -> List b x -> (List a x, List (Succ b) x)
    move (a :| from) to = (from, a :| to)



-- * Helpers

todo :: a
todo = P.error "todo"
