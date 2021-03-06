{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Ring
  ( Ring (..)
  ) where

import Core (xor)
import Data.Ratio ((%))
import Prelude hiding ((<>))
import Test.SmallCheck (smallCheck)

-- #@@range_begin(class)
class Ring a where
  (<>)     :: a -> a -> a
  emptyA   :: a
  inverseA :: a -> a
  (><)     :: a -> a -> a
  emptyM   :: a

infixl 6 <>
infixl 7 ><
-- #@@range_end(class)

-- #@@range_begin(instances)
instance Ring Integer where
  (<>)     = (+)
  emptyA   = 0
  inverseA = negate
  (><)     = (*)
  emptyM   = 1

instance Ring Rational where
  (<>)     = (+)
  emptyA   = 0 % 1
  inverseA = negate
  (><)     = (*)
  emptyM   = 1 % 1

instance Ring Bool where
  (<>)     = xor
  emptyA   = False
  inverseA = id
  (><)     = (&&)
  emptyM   = True

instance Ring () where
  () <> ()    = ()
  emptyA      = ()
  inverseA () = ()
  () >< ()    = ()
  emptyM      = ()
-- #@@range_end(instances)

-- #@@range_begin(laws)
-- #@@range_begin(additive_laws)
associativeLawForAdd :: (Ring a, Eq a) => a -> a -> a -> Bool
associativeLawForAdd x y z =
  (x <> y) <> z == x <> (y <> z)

commutativeLawForAdd :: (Ring a, Eq a) => a -> a -> Bool
commutativeLawForAdd x y =
  x <> y == y <> x

emptyLawForAdd :: (Ring a, Eq a) => a -> Bool
emptyLawForAdd x =
  (x <> emptyA == x) && (x == emptyA <> x)

inverseLawForAdd :: (Ring a, Eq a) => a -> Bool
inverseLawForAdd x =
  (x <> inverseA x == emptyA) && (emptyA == inverseA x <> x)
-- #@@range_end(additive_laws)

-- #@@range_begin(multiplicative_laws)
associativeLawForMulti :: (Ring a, Eq a) => a -> a -> a -> Bool
associativeLawForMulti x y z =
  (x >< y) >< z == x >< (y >< z)

commutativeLawForMulti :: (Ring a, Eq a) => a -> a -> Bool
commutativeLawForMulti x y =
  x <> y == y <> x

emptyLawForMulti :: (Ring a, Eq a) => a -> Bool
emptyLawForMulti x =
  (x >< emptyM == x) && (x == emptyM >< x)
-- #@@range_end(multiplicative_laws)

-- #@@range_begin(distributive_law)
distributiveLaw :: (Ring a, Eq a) => a -> a -> a -> Bool
distributiveLaw x y z =
  x >< (y <> z) == x >< y <> x >< z
    &&
  (y <> z) >< x == y >< x <> z >< x
-- #@@range_end(distributive_law)
-- #@@range_end(laws)

checkAdditiveLaws :: IO ()
checkAdditiveLaws = do
  smallCheck 2 $ associativeLawForAdd @Integer
  smallCheck 2 $ associativeLawForAdd @Rational
  smallCheck 2 $ associativeLawForAdd @Bool
  smallCheck 2 $ associativeLawForAdd @()
  smallCheck 2 $ commutativeLawForAdd @Integer
  smallCheck 2 $ commutativeLawForAdd @Rational
  smallCheck 2 $ commutativeLawForAdd @Bool
  smallCheck 2 $ commutativeLawForAdd @()
  smallCheck 2 $ emptyLawForAdd @Integer
  smallCheck 2 $ emptyLawForAdd @Rational
  smallCheck 2 $ emptyLawForAdd @Bool
  smallCheck 2 $ emptyLawForAdd @()
  smallCheck 2 $ inverseLawForAdd @Integer
  smallCheck 2 $ inverseLawForAdd @Rational
  smallCheck 2 $ inverseLawForAdd @Bool
  smallCheck 2 $ inverseLawForAdd @()

checkMultiplicativeLaws :: IO ()
checkMultiplicativeLaws = do
  smallCheck 2 $ associativeLawForMulti @Integer
  smallCheck 2 $ associativeLawForMulti @Rational
  smallCheck 2 $ associativeLawForMulti @Bool
  smallCheck 2 $ associativeLawForMulti @()
  smallCheck 2 $ commutativeLawForMulti @Integer
  smallCheck 2 $ commutativeLawForMulti @Rational
  smallCheck 2 $ commutativeLawForMulti @Bool
  smallCheck 2 $ commutativeLawForMulti @()
  smallCheck 2 $ emptyLawForMulti @Integer
  smallCheck 2 $ emptyLawForMulti @Rational
  smallCheck 2 $ emptyLawForMulti @Bool
  smallCheck 2 $ emptyLawForMulti @()

checkDistributiveLaw :: IO ()
checkDistributiveLaw = do
  smallCheck 2 $ distributiveLaw @Integer
  smallCheck 2 $ distributiveLaw @Rational
  smallCheck 2 $ distributiveLaw @Bool
  smallCheck 2 $ distributiveLaw @()

main :: IO ()
main = do
  checkAdditiveLaws
  checkMultiplicativeLaws
  checkDistributiveLaw
