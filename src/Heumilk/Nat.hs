module Heumilk.Nat where

data Nat = NZero | NSucc Nat deriving (Eq, Ord)
instance Show Nat where show = show . natToNum

natFromInt :: Int -> Nat
natFromInt 0 = NZero
natFromInt n
  | n > 0     = NSucc $ natFromInt (n - 1)
  | otherwise = undefined

natToNum :: Num a => Nat -> a
natToNum NZero = 0
natToNum (NSucc a) = 1 + natToNum a

natSub :: Nat -> Nat -> Nat
natSub a NZero = a
natSub NZero _ = NZero
natSub (NSucc a) (NSucc b) = natSub a b

natLength :: [a] -> Nat
natLength [] = NZero
natLength (_ : xs) = (NSucc . natLength) xs

natDiv :: (Ord a, Num a) => a -> a -> Nat
natDiv a b
  | a < b = NZero
  | a >= b = NSucc $ natDiv (a - b) b

natFit :: (Ord a, Num a) => a -> a -> Nat
natFit a b
  | a <= b = NSucc NZero
  | a > b = NSucc $ natFit (a - b) b

(!) :: [a] -> Nat -> a
(!) xs NZero = head xs
(!) (x : xs) (NSucc n) = xs ! n
