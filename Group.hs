{-# LANGUAGE TypeApplications #-}

module Group
  ( Group (..)
  ) where

import Monoid
import Prelude hiding (Semigroup(..), Monoid(..))
import Semigroup
import Test.SmallCheck (smallCheck)

class Monoid a => Group a where
  inverse :: a -> a

instance Group SumInt where
  inverse = negate

instance Group () where
  inverse _ = ()

aSumInt :: SumInt
aSumInt = inverse empty

aUnit :: ()
aUnit = inverse empty

inverseLaw :: (Group a, Eq a) => a -> Bool
inverseLaw x =
  (x <> inverse x == empty) && (empty == inverse x <> x)

main :: IO ()
main = do
  smallCheck 2 $ inverseLaw @ SumInt
  smallCheck 2 $ inverseLaw @ ()
