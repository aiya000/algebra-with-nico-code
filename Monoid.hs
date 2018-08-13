{-# LANGUAGE TypeApplications #-}

module Monoid
  ( Monoid (..)
  ) where

import Prelude hiding (Semigroup(..), Monoid(..))
import Semigroup
import Test.SmallCheck (smallCheck)

class Semigroup a => Monoid a where
  empty :: a

instance Monoid SumInt where
  empty = 0

instance Monoid ProductInt where
  empty = 1

instance Monoid [a] where
  empty = []

instance Monoid And where
  empty = And True

instance Monoid Or where
  empty = Or False

instance Monoid () where
  empty = ()

aSumInt :: SumInt
aSumInt = SumInt 10 <> empty

aProductInt :: ProductInt
aProductInt = empty <> ProductInt 20

emptyLaw :: (Monoid a, Eq a) => a -> Bool
emptyLaw x =
  empty <> x == x && x == x <> empty

{-# ANN mconcat "HLint: ignore Eta reduce" #-}

mconcat :: Monoid a => [a] -> a
mconcat xs = foldl (<>) empty xs

result :: SumInt
result = mconcat [1..100]

main :: IO ()
main = do
  smallCheck 2 $ emptyLaw @ SumInt
  smallCheck 2 $ emptyLaw @ ProductInt
  smallCheck 2 $ emptyLaw @ [Double]
  smallCheck 2 $ emptyLaw @ And
  smallCheck 2 $ emptyLaw @ Or
  smallCheck 2 $ emptyLaw @ ()
