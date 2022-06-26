module Bit where

data Bit = T | F
  deriving Show

__ :: Bit -> Bit
__ F = T
__ T = F

(/\) :: Bit -> Bit -> Bit
T /\ T = T
_ /\ _ = F

(\/) :: Bit -> Bit -> Bit
F \/ F = F
_ \/ _ = T

(<+>) :: Bit -> Bit -> Bit
T <+> T = F
F <+> F = F
_ <+> _ = T

(==>) :: Bit -> Bit -> Bit
T ==> F = F
_ ==> _ = T

(<=>) :: Bit -> Bit -> Bit
x <=> y = __ (x <+> y)

(/|\) :: Bit -> Bit -> Bit
x /|\ y = __ (x /\ y)

(\|/) :: Bit -> Bit -> Bit
x \|/ y = __ (x \/ y)

bool :: Bit -> Bool
bool T = True
bool F = False

bit :: Bool -> Bit
bit True = T
bit False = F
  