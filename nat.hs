-- algebraic treatment of natural numbers
-- Konstantin Läufer
-- Comp 372/471: Programming Languages
-- Loyola University Chicago
-- http://laufer.cs.luc.edu/teaching/372
-- based on section two of Erik Meijer's banana paper

import Test.HUnit
import Data.List(unfoldr)

data Nat = Zero | Succ Nat

-- make it printable with parentheses where needed

instance Show Nat where
  showsPrec _ Zero     = showString "Zero"
  showsPrec d (Succ n) = showParen (d > 0) $ showString "Succ " . showsPrec (d + 1) n 

testsNatShow = 
  TestList 
  [ TestLabel "NatShow0" (TestCase ("Zero"             @=? show Zero))
  , TestLabel "NatShow1" (TestCase ("Succ Zero"        @=? show (Succ Zero)))
  , TestLabel "NatShow2" (TestCase ("Succ (Succ Zero)" @=? show (Succ (Succ Zero))))
  ]

-- unit values
  
zero :: Nat
zero = Zero

one :: Nat
one = Succ Zero

-- make it comparable

instance Eq Nat where
  Zero   == Zero   = True
  Zero   == Succ _ = False
  Succ _ == Zero   = False
  Succ n == Succ m = n == m

testsNatEq = 
  TestList 
  [ TestLabel "NatEq00" (TestCase (zero == zero @? "failed"))
  , TestLabel "NatEq01" (TestCase (zero /= one  @? "failed"))
  , TestLabel "NatEq10" (TestCase (one  /= zero @? "failed"))
  , TestLabel "NatEq11" (TestCase (one  == one  @? "failed"))
  , TestLabel "NatEq33" (TestCase (Succ (Succ (Succ Zero)) == Succ (Succ (Succ Zero)) @? "failed"))
  ]

instance Ord Nat where
  Zero   <= Zero   = True
  Zero   <= Succ _ = True
  Succ _ <= Zero   = False
  Succ n <= Succ m = n <= m

testsNatOrd = 
  TestList 
  [ TestLabel "NatOrd00" (TestCase (zero <= zero @? "failed"))
  , TestLabel "NatOrd01" (TestCase (zero <= one  @? "failed"))
  , TestLabel "NatOrd10" (TestCase (one  >= zero @? "failed"))
  , TestLabel "NatOrd11" (TestCase (one  >= one  @? "failed"))
  , TestLabel "NatOrd33" (TestCase (Succ (Succ (Succ Zero)) <= Succ (Succ (Succ Zero)) @? "failed"))
  , TestLabel "NatOrd32" (TestCase (Succ (Succ (Succ Zero))  > Succ (Succ Zero)        @? "failed"))
  , TestLabel "NatOrd23" (TestCase (Succ (Succ Zero)         < Succ (Succ (Succ Zero)) @? "failed"))
  ]

-- some recursive functions

addR          :: Nat -> Nat -> Nat
addR Zero     n = n
addR (Succ m) n = addR m (Succ n)

multR          :: Nat -> Nat -> Nat
multR Zero     n = Zero
multR (Succ m) n = addR n (multR m n)

toIntR          :: Nat -> Integer
toIntR Zero     = 0
toIntR (Succ n) = 1 + (toIntR n)

fromIntR           :: Integer -> Nat
fromIntR 0         = Zero
fromIntR n | n > 0 = Succ (fromIntR (n - 1))

two   = Succ one
three = Succ two
four  = Succ three
five  = Succ four
six   = Succ five

testsNatBasic toInt fromInt add mult = 
  TestList 
  [ TestLabel "toIntR0"   (TestCase (0    @=? toIntR zero))
  , TestLabel "toIntR5"   (TestCase (5    @=? toIntR five))
  , TestLabel "fromIntR0" (TestCase (zero @=? fromIntR 0))
  , TestLabel "fromIntR5" (TestCase (five @=? fromIntR 5))
  , TestLabel "addR01"    (TestCase (one  @=? zero `add` one ))
  , TestLabel "addR10"    (TestCase (one  @=? one  `add` zero))
  , TestLabel "addR11"    (TestCase (two  @=? one  `add` one ))
  , TestLabel "multR00"   (TestCase (zero @=? zero `mult` zero))
  , TestLabel "multR01"   (TestCase (zero @=? zero `mult` one ))
  , TestLabel "multR10"   (TestCase (zero @=? one  `mult` zero))
  , TestLabel "multR11"   (TestCase (one  @=? one  `mult` one ))
  , TestLabel "multR22"   (TestCase (four @=? two  `mult` two ))
  , TestLabel "multR32"   (TestCase (six  @=? two   `mult` three))
  , TestLabel "multR23"   (TestCase (six  @=? three `mult` two))
  ]

testsNatBasicR = testsNatBasic toIntR fromIntR addR multR

-- catamorphism: break down a Nat to some other value

fold              :: (a -> a) -> a -> Nat -> a
fold f z Zero     = z
fold f z (Succ n) = let z' = f z in seq z' $ fold f z' n

--foldl f z (Succ n) = fold f (f z) n
--foldr f z (Succ n) = f $ fold f z n

testsNatFold = 
  TestList 
  [ TestLabel "foldAdd0" (TestCase (0 @=? fold (+1) 0 zero))
  , TestLabel "foldAdd5" (TestCase (5 @=? fold (+1) 0 five))
  ]

-- functions from above as catamorphisms (no explicit recursion)

add :: Nat -> Nat -> Nat
add = fold Succ

mult   :: Nat -> Nat -> Nat
mult m = fold (add m) Zero

toInt :: Nat -> Integer
toInt = fold (+1) 0

toList            :: a -> Nat -> [a]
toList x Zero     = []
toList x (Succ n) = x : (toList x n)

factCata :: Nat -> Nat
factCata = snd . factCata'
  where factCata' = fold (\(n,r) -> (add n one, mult n r)) (one, one)

testsNatFact fact = 
  TestList
  [ TestLabel "factCata0" (TestCase (one @=? fact zero ))
  , TestLabel "factCata1" (TestCase (one @=? fact one  ))
  , TestLabel "factCata3" (TestCase (six @=? fact three))
  ]
  
testsNatFactCata = testsNatFact factCata

-- anamorphism: build up a Nat from some sequence of other values

unfold     :: (a -> Maybe a) -> a -> Nat
unfold g z = case g z of
  Nothing -> Zero
  Just z' -> Succ $ unfold g z'
  
testsNatUnfold = 
  TestList
  [ TestLabel "unfoldSucc5" (TestCase (five @=? unfold (\x -> if x >= 5 then Nothing else Just (x + 1)) 0))
  ]

-- function from above as anamorphism (no explicit recursion)

fromInt :: Integer -> Nat
fromInt = unfold (\n -> if n > 0 then Just (n - 1) else Nothing)

testsNatBasicCataAna = testsNatBasic toInt fromInt add mult

-- hylomorphism: build up the call stack as a list
  
hylor     :: (a -> b -> b) -> b -> (c -> Maybe (a, c)) -> c -> b
hylor f z g = foldr f z . unfoldr g

gInc          :: Nat -> Maybe (Nat, Nat)
gInc Zero     = Nothing
gInc (Succ m) = Just (Succ m, m)

factHylo :: Nat -> Nat
factHylo = hylor mult one gInc

testsNatFactHylo = testsNatFact factHylo

-- paramorphism

para              :: (Nat -> a -> a) -> a -> Nat -> a
para f z Zero     = z
para f z (Succ n) = f n (para f z n)

incAndMult :: Nat -> Nat -> Nat
incAndMult = mult . Succ

fact :: Nat -> Nat
fact = para incAndMult one

testsNatFactPara = testsNatFact fact

testsNatAll = 
  TestList
  [ testsNatShow
  , testsNatEq
  , testsNatOrd
  , testsNatBasicR
  , testsNatFold
  , testsNatFactCata
  , testsNatUnfold
  , testsNatBasicCataAna
  , testsNatFactHylo
  , testsNatFactPara
  ]