module Bitset where

import qualified Bit as B
  
type Bitset = [B.Bit]

one :: Bitset
one = [B.T]

zero :: Bitset
zero = [B.F]

strip :: Bitset -> Bitset
strip [] = []
strip full@(x : xs) = if B.bool (x B.<=> B.F) then strip xs else full

pad :: Int -> Bitset -> Bitset
pad n x = replicate (n - length x) B.F ++ x

padMax :: [Bitset] -> (Bitset -> Bitset)
padMax xs = pad (maximum [length x | x <- xs])

(-|-) :: Bitset -> Bitset -> Bitset
a -|- b = unwrap (wrap a `plus` wrap b)
  where
    wrap = reverse . strip
    unwrap = strip . reverse
    [] `plus` [] = []
    x `plus` [] = x
    [] `plus` x = x
    (x : xs) `plus` (y : ys) = (x B.<+> y) : (xs `plus` (ys `plus` [x B./\ y]))

inc :: Bitset -> Bitset
inc = (-|- one)

bits :: Int -> Bitset
bits 0 = zero
bits n = inc . bits $ n - 1

unbits :: Bitset -> Int
unbits xs = sum [(fromEnum . B.bool $ x) * 2 ^ i | (i, x) <- zip [0..] (reverse xs)]

bitsFixed :: Int -> Int -> Bitset
bitsFixed n = pad n . bits

table :: Int -> [Bitset]
table 0 = []
table n = [bitsFixed n x | x <- [0..2^n - 1]]

__ :: Bitset -> Bitset
__ = map B.__

bitwise :: (B.Bit -> B.Bit -> B.Bit) -> Bitset -> Bitset -> Bitset
bitwise f x y = strip (zipWith f (pax x) (pax y))
  where pax = padMax [x, y]

x /\ y  = bitwise (B./\)  x y
x \/ y  = bitwise (B.\/)  x y
x <+> y = bitwise (B.<+>) x y
x ==> y = bitwise (B.==>) x y
x <=> y = bitwise (B.<=>) x y
x /|\ y = bitwise (B./|\) x y
x \|/ y = bitwise (B.\|/) x y
