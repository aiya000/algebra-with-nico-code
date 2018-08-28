{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Group
  ( Group (..)
  ) where

import Core
import Data.Ratio ((%), numerator, denominator)
import Monoid
import Prelude hiding (Semigroup(..), Monoid(..))
import Semigroup
import Test.SmallCheck (smallCheck)

-- #@@range_begin(class)
class Monoid a => Group a where
  inverse :: a -> a
-- #@@range_end(class)

-- #@@range_begin(instances)
instance Group Sum where
  inverse = negate

instance Group RSum where
  inverse = negate

instance Group () where
  inverse () = ()
-- #@@range_end(instances)

aSumInt :: Sum
aSumInt = inverse empty

aUnit :: ()
aUnit = inverse empty

-- #@@range_begin(law)
inverseLaw :: (Group a, Eq a) => a -> Bool
inverseLaw x =
  (x <> inverse x == empty) && (empty == inverse x <> x)
-- #@@range_end(law)

main :: IO ()
main = do
  smallCheck 2 $ inverseLaw @Sum
  smallCheck 2 $ inverseLaw @RSum
  smallCheck 2 $ inverseLaw @()

-- #@@range_begin(extra)
-- | An illegal instance
instance Group RProduct where
  inverse (RProduct x) = RProduct $ denominator x % numerator x

checkRProductGroup :: IO ()
checkRProductGroup =
  smallCheck 2 $ inverseLaw @ RProduct
-- #@@range_end(extra)
