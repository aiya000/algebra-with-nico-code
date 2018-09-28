{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Field
  ( Field (..)
  ) where

import Data.Ratio ((%), numerator, denominator)
import Prelude hiding ((<>))
import Ring
import Test.SmallCheck (smallCheck)

-- #@@range_begin(class)
class Ring a => Field a where
  inverseM :: a -> a
-- #@@range_end(class)

-- #@@range_begin(instances)
instance Field Rational where
  inverseM x = denominator x % numerator x
-- #@@range_end(instances)

-- #@@range_begin(laws)
inverseLawForMulti :: forall a. (Field a, Eq a) => a -> Bool
inverseLawForMulti x
  | x == emptyA = True
  | otherwise =
    (x >< inverseM x == emptyM) && (emptyM == inverseM x >< x)

emptyDifferenceLaw :: forall a. (Field a, Eq a) => Bool
emptyDifferenceLaw = (emptyM :: a) /= (emptyA :: a)
-- #@@range_end(laws)

main :: IO ()
main = do
  smallCheck 2 $ inverseLawForMulti @Rational
  smallCheck 2 $ emptyDifferenceLaw @Rational
