{-# LANGUAGE UndecidableInstances #-}
module Data.Peano where

import GHC.TypeLits

-- * Peano

data Peano = Zero | Succ Peano

type family ToPeano (n :: Nat) = (p :: Peano) where
  ToPeano 0 = Zero
  ToPeano n = Succ (ToPeano (n - 1))

type family Prev (n :: Peano) = (n' :: Peano) where
  Prev Zero = Zero
  Prev (Succ n) = n
