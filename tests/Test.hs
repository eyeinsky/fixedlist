module Main where

test1 = do
  ret@ (PeanoList (a :* b :* c)) <- genList1 @3 Proxy (return (1 :: Int))
--  let ret' = fmap (+1) (PeanoList ret :: PeanoList (ToPeano 2) Int)
  return ret

-- ^ :t test1 ==> test1 :: Monad m => m (Int :* (Int :* Int))
