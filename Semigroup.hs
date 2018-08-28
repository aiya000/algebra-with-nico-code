{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Semigroup
  ( Semigroup (..)
  , Sum (..)
  , Product (..)
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

instance Semigroup Rational where
  (<>) = (+)

instance Semigroup [a] where
  (<>) = (++)

instance Semigroup Bool where
  (<>) = (&&)

instance Semigroup () where
  () <> () = ()
-- #@@range_end(basic_instances)

-- #@@range_begin(newtype_instances)
instance Semigroup Sum where
  (<>) = (+)

instance Semigroup Product where
  (<>) = (*)

instance Semigroup RSum where
  (<>) = (+)

instance Semigroup RProduct where
  (<>) = (*)

instance Semigroup And where
  And x <> And y = And $ x && y

instance Semigroup Or where
  Or x <> Or y = Or $ x || y
-- #@@range_end(newtype_instances)

-- #@@range_begin(int_instances_examples)
aSumInt :: Sum
aSumInt = Sum 10 <> Sum 20

aProductInt :: Product
aProductInt = Product 10 <> Product 20
-- #@@range_end(int_instances_examples)

-- #@@range_begin(law)
associativeLaw :: (Semigroup a, Eq a) => a -> a -> a -> Bool
associativeLaw x y z =
  (x <> y) <> z == x <> (y <> z)
-- #@@range_end(law)

{-# ANN concat "HLint: ignore Eta reduce" #-}

-- #@@range_begin(practice)
concat :: Semigroup a => a -> [a] -> a
concat initial xs = foldl (<>) initial xs

result :: Sum
result = sum [1..100]
  where
    sum :: [Sum] -> Sum
    sum [] = 0
    sum xs = concat 0 xs
-- #@@range_end(practice)

-- #@@range_begin(tests_for_law)
main :: IO ()
main = do
  smallCheck 2 $ associativeLaw @Sum
  smallCheck 2 $ associativeLaw @Product
  smallCheck 2 $ associativeLaw @RSum
  smallCheck 2 $ associativeLaw @RProduct
  smallCheck 2 $ associativeLaw @[Double]
  smallCheck 2 $ associativeLaw @And
  smallCheck 2 $ associativeLaw @Or
  smallCheck 2 $ associativeLaw @()
-- #@@range_end(tests_for_law)
