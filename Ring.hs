{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Ring
  ( Ring (..)
  ) where

import Data.Ratio ((%))
import Prelude hiding ((<>))
import Test.SmallCheck (smallCheck)

-- #@@range_begin(class)
class Ring a where
  (<>)     :: a -> a -> a
  emptyA   :: a
  inverseA :: a -> a
  (><)     :: a -> a -> a

infixl 6 <>
infixl 7 ><
-- #@@range_end(class)

-- #@@range_begin(instances)
instance Ring Int where
  (<>)     = (+)
  emptyA   = 0
  inverseA = negate
  (><)     = (*)

instance Ring Rational where
  (<>)     = (+)
  emptyA   = 0 % 1
  inverseA = negate
  (><)     = (*)

instance Ring () where
  () <> ()    = ()
  emptyA      = ()
  inverseA () = ()
  () >< ()    = ()
-- #@@range_end(instances)

-- #@@range_begin(usual_laws)
associativeLawForAdd :: (Ring a, Eq a) => a -> a -> a -> Bool
associativeLawForAdd x y z =
  (x <> y) <> z == x <> (y <> z)

emptyLawForAdd :: (Ring a, Eq a) => a -> Bool
emptyLawForAdd x =
  (x <> emptyA == x) && (x == emptyA <> x)

inverseLawForAdd :: (Ring a, Eq a) => a -> Bool
inverseLawForAdd x =
  (x <> inverseA x == emptyA) && (emptyA == inverseA x <> x)

associativeLawForMulti :: (Ring a, Eq a) => a -> a -> a -> Bool
associativeLawForMulti x y z =
  (x >< y) >< z == x >< (y >< z)
-- #@@range_end(usual_laws)

-- #@@range_begin(newer_law)
distributiveLaw :: (Ring a, Eq a) => a -> a -> a -> Bool
distributiveLaw x y z =
  x >< (y <> z) == x >< y <> x >< z
    &&
  (y <> z) >< x == y >< x <> z >< x
-- #@@range_end(newer_law)

checkAssociativeLawForAdd :: IO ()
checkAssociativeLawForAdd = do
  smallCheck 2 $ associativeLawForAdd @Int
  smallCheck 2 $ associativeLawForAdd @Rational
  smallCheck 2 $ associativeLawForAdd @()

checkEmptyLawForAdd :: IO ()
checkEmptyLawForAdd = do
  smallCheck 2 $ emptyLawForAdd @Int
  smallCheck 2 $ emptyLawForAdd @Rational
  smallCheck 2 $ emptyLawForAdd @()

checkInverseLawForAdd :: IO ()
checkInverseLawForAdd = do
  smallCheck 2 $ inverseLawForAdd @Int
  smallCheck 2 $ inverseLawForAdd @Rational
  smallCheck 2 $ inverseLawForAdd @()

checkAssociativeLawForMulti :: IO ()
checkAssociativeLawForMulti = do
  smallCheck 2 $ associativeLawForMulti @Int
  smallCheck 2 $ associativeLawForMulti @Rational
  smallCheck 2 $ associativeLawForMulti @()

checkDistributiveLaw :: IO ()
checkDistributiveLaw = do
  smallCheck 2 $ distributiveLaw @Int
  smallCheck 2 $ distributiveLaw @Rational
  smallCheck 2 $ distributiveLaw @()

main :: IO ()
main = do
  checkAssociativeLawForAdd
  checkEmptyLawForAdd
  checkInverseLawForAdd
  checkAssociativeLawForMulti
