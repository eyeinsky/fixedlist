{-# LANGUAGE UndecidableInstances #-}
module Data.Peano where

import GHC.TypeLits

-- * Peano

data Peano = Zero | Succ Peano

type family ToPeano (n :: Nat) = (p :: Peano) where
  ToPeano 0 = Zero
  ToPeano n = Succ (ToPeano (n - 1))

-- | Convert GHC.TypeLits Nat to Peano
type family FromPeano (n :: Peano) = (p :: Nat) where
  FromPeano Zero = 0
  FromPeano (Succ p) = 1 + FromPeano p

-- | Get previous Peano number
type family Prev (n :: Peano) = (n' :: Peano) where
  Prev Zero = Zero
  Prev (Succ n) = n

-- | Add peano numbers [ref1]
type family Add (a :: Peano) (b :: Peano) = (c :: Peano) where
  Add Zero a = a
  Add (Succ a) b = Add a (Succ b)
-- type family Add (p0 :: Peano) (p1 :: Peano) = (p2 :: Peano) where
--   Add p Zero = p
--   Add p1 (Succ p0) = Add p0 (Succ p1)

-- type family AddC (a :: Peano) (b :: Peano) :: Constraint where
--   AddC Zero a = ()
--   AddC (Succ a) b = (AddC a b ~ Succ )

type family Subtract (a :: Peano) (b :: Peano) = (c :: Peano) where
  Subtract (Succ a) (Succ b) = Subtract a b
  Subtract a Zero = a
>>>>>>> 0e3a7e4 (Add FromPeano and Subtract type families)
