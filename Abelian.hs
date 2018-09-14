{-# LANGUAGE TypeApplications #-}

module Abelian where

import Core
import Prelude hiding (Semigroup(..))
import Semigroup
import Test.SmallCheck (smallCheck)

-- #@@range_begin(class)
class Semigroup a => Abelian a
-- #@@range_end(class)

-- #@@range_begin(instances)
instance Abelian Sum
instance Abelian Product
instance Abelian RSum
instance Abelian RProduct
instance Abelian And
instance Abelian Or
instance Abelian ()
-- #@@range_end(instances)

-- #@@range_begin(law)
commutativeLaw :: (Abelian a, Eq a) => a -> a -> Bool
commutativeLaw x y =
  x <> y == y <> x
-- #@@range_end(law)

-- #@@range_begin(tests_for_law)
main :: IO ()
main = do
  smallCheck 2 $ commutativeLaw @Sum
  smallCheck 2 $ commutativeLaw @Product
  smallCheck 2 $ commutativeLaw @RSum
  smallCheck 2 $ commutativeLaw @RProduct
  smallCheck 2 $ commutativeLaw @And
  smallCheck 2 $ commutativeLaw @Or
  smallCheck 2 $ commutativeLaw @()
-- #@@range_end(tests_for_law)
