{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Field
  ( Field (..)
  ) where

import Data.Maybe (fromJust)
import Data.Ratio ((%), numerator, denominator)
import Prelude hiding ((<>))
import Ring
import Test.SmallCheck (smallCheck)

-- #@@range_begin(class)
class Ring a => Field a where
  emptyM :: a
  inverseM :: a -> a
-- #@@range_end(class)

-- #@@range_begin(instances)
instance Field Rational where
  emptyM = 1 % 1
  inverseM x = denominator x % numerator x

instance Field () where
  emptyM = ()
  inverseM () = ()
-- #@@range_end(instances)

-- #@@range_begin(laws)
emptyLawForMulti :: (Field a, Eq a) => a -> Bool
emptyLawForMulti x =
  (x >< emptyM == x) && (x == emptyM >< x)

inverseLawForMulti :: (Field a, Eq a) => a -> Bool
inverseLawForMulti x
  | x == emptyA = True
  | otherwise =
    (x >< inverseM x == emptyM) && (emptyM == inverseM x >< x)
-- #@@range_end(laws)

checkEmptyLawForMulti :: IO ()
checkEmptyLawForMulti = do
  smallCheck 2 $ emptyLawForMulti @Rational
  smallCheck 2 $ emptyLawForMulti @()

checkInverseLawForMulti :: IO ()
checkInverseLawForMulti = do
  smallCheck 2 $ inverseLawForMulti @Rational
  smallCheck 2 $ inverseLawForMulti @()

main :: IO ()
main = do
  checkEmptyLawForMulti
  checkInverseLawForMulti
