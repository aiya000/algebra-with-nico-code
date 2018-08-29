{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Field
  ( Field (..)
  ) where

import Data.Ratio ((%), numerator, denominator)
import Prelude hiding ((<>))
import Ring
import Test.SmallCheck (smallCheck)

class Ring a => Field a where
  emptyM :: a
  inverseM :: a -> a

instance Field Rational where
  emptyM = 1 % 1
  inverseM x = denominator x % numerator x

instance Field () where
  emptyM = ()
  inverseM () = ()

emptyLawForMulti :: (Field a, Eq a) => a -> Bool
emptyLawForMulti x =
  (x >< emptyM == x) && (x == emptyM >< x)

inverseLawForMulti :: (Field a, Eq a) => a -> Bool
inverseLawForMulti x
  | x == emptyA = True
  | otherwise =
    (x >< inverseM x == emptyM) && (emptyM == inverseM x >< x)

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