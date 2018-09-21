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

-- #@@range_begin(int_instance)
instance Semigroup Integer where
  (<>) = (+)
-- #@@range_end(int_instance)

-- #@@range_begin(bool_instance)
instance Semigroup Bool where
  (<>) = (&&)
-- #@@range_end(bool_instance)

-- #@@range_begin(newtype_instances)
instance Semigroup Sum where
  (<>) = (+)

instance Semigroup Product where
  (<>) = (*)

instance Semigroup And where
  And x <> And y = And $ x && y

instance Semigroup Or where
  Or x <> Or y = Or $ x || y
-- #@@range_end(newtype_instances)

-- #@@range_begin(another_instances)
instance Semigroup RSum where
  (<>) = (+)

instance Semigroup RProduct where
  (<>) = (*)

instance Semigroup Rational where
  (<>) = (+)

instance Semigroup [a] where
  (<>) = (++)

instance Semigroup () where
  () <> () = ()
-- #@@range_end(another_instances)

-- #@@range_begin(int_instances_examples)
aSumInt :: Sum
aSumInt = Sum 10 <> Sum 20

aProductInt :: Product
aProductInt = Product 10 <> Product 20
-- #@@range_end(int_instances_examples)

-- #@@range_begin(law)
-- #@@range_begin(law_type)
associativeLaw :: (Semigroup a, Eq a) => a -> a -> a -> Bool
-- #@@range_end(law_type)
associativeLaw x y z =
  (x <> y) <> z == x <> (y <> z)
-- #@@range_end(law)

-- #@@range_begin(practice)
concat :: Semigroup a => a -> [a] -> a
concat = foldl (<>)

resultSum :: Integer
resultSum = concat 0 [1..100]

resultAll :: Bool
resultAll = concat True [True, True, True]
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
