{-# LANGUAGE TypeApplications #-}

module Semigroup
  ( Semigroup (..)
  , SumInt (..)
  , ProductInt (..)
  , And (..)
  , Or (..)
  ) where

import Prelude hiding (Semigroup(..), concat)
import Core
import Test.SmallCheck (smallCheck)

-- #@@range_begin(class)
class Semigroup a where
  (<>) :: a -> a -> a
-- #@@range_end(class)

-- #@@range_begin(basic_instances)
-- #@@range_begin(int_instance)
instance Semigroup Int where
  (<>) = (+)
-- #@@range_end(int_instance)

instance Semigroup Float where
  (<>) = (+)

instance Semigroup [a] where
  (<>) = (++)

instance Semigroup Bool where
  (<>) = (&&)

instance Semigroup () where
  () <> () = ()
-- #@@range_end(basic_instances)

-- #@@range_begin(newtype_instances)
instance Semigroup SumInt where
  (<>) = (+)

instance Semigroup ProductInt where
  (<>) = (*)

instance Semigroup SumFloat where
  (<>) = (+)

instance Semigroup ProductFloat where
  (<>) = (*)

instance Semigroup And where
  And x <> And y = And $ x && y

instance Semigroup Or where
  Or x <> Or y = Or $ x || y
-- #@@range_end(newtype_instances)

-- #@@range_begin(int_instances_examples)
aSumInt :: SumInt
aSumInt = SumInt 10 <> SumInt 20

aProductInt :: ProductInt
aProductInt = ProductInt 10 <> ProductInt 20
-- #@@range_end(int_instances_examples)

-- #@@range_begin(law)
assocLaw :: (Semigroup a, Eq a) => a -> a -> a -> Bool
assocLaw x y z =
  (x <> y) <> z == x <> (y <> z)
-- #@@range_end(law)

{-# ANN concat "HLint: ignore Eta reduce" #-}

-- #@@range_begin(practice)
concat :: Semigroup a => a -> [a] -> a
concat initial xs = foldl (<>) initial xs

result :: SumInt
result = sum [1..100]
  where
    sum :: [SumInt] -> SumInt
    sum [] = 0
    sum xs = concat 0 xs
-- #@@range_end(practice)

-- #@@range_begin(tests_for_law)
main :: IO ()
main = do
  smallCheck 2 $ assocLaw @ SumInt
  smallCheck 2 $ assocLaw @ ProductInt
  smallCheck 2 $ assocLaw @ SumFloat
  smallCheck 2 $ assocLaw @ ProductFloat
  smallCheck 2 $ assocLaw @ [Double]
  smallCheck 2 $ assocLaw @ And
  smallCheck 2 $ assocLaw @ Or
  smallCheck 2 $ assocLaw @ ()
-- #@@range_end(tests_for_law)
