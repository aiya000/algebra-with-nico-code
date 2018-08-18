{-# LANGUAGE TypeApplications #-}

module Monoid
  ( Monoid (..)
  ) where

import Core
import Data.Ratio ((%))
import Prelude hiding (Semigroup(..), Monoid(..))
import Semigroup
import Test.SmallCheck (smallCheck)

-- #@@range_begin(class)
class Semigroup a => Monoid a where
  empty :: a
-- #@@range_end(class)

-- #@@range_begin(instances)
instance Monoid Sum where
  empty = Sum 0

instance Monoid Product where
  empty = Product 1

instance Monoid RSum where
  empty = RSum $ 0 % 1

instance Monoid RProduct where
  empty = RProduct $ 1 % 1

instance Monoid [a] where
  empty = []

instance Monoid And where
  empty = And True

instance Monoid Or where
  empty = Or False

instance Monoid () where
  empty = ()
-- #@@range_end(instances)

-- #@@range_begin(value_example)
aSumInt :: Sum
aSumInt = Sum 10 <> empty

aProductInt :: Product
aProductInt = empty <> Product 20
-- #@@range_end(value_example)

-- #@@range_begin(law)
emptyLaw :: (Monoid a, Eq a) => a -> Bool
emptyLaw x =
  empty <> x == x && x == x <> empty
-- #@@range_end(law)

{-# ANN mconcat "HLint: ignore Eta reduce" #-}

mconcat :: Monoid a => [a] -> a
mconcat xs = foldl (<>) empty xs

result :: Sum
result = mconcat [1..100]

-- #@@range_begin(tests_for_law)
main :: IO ()
main = do
  smallCheck 2 $ emptyLaw @ Sum
  smallCheck 2 $ emptyLaw @ Product
  smallCheck 2 $ emptyLaw @ RSum
  smallCheck 2 $ emptyLaw @ RProduct
  smallCheck 2 $ emptyLaw @ [Double]
  smallCheck 2 $ emptyLaw @ And
  smallCheck 2 $ emptyLaw @ Or
  smallCheck 2 $ emptyLaw @ ()
-- #@@range_end(tests_for_law)
