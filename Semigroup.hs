{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Semigroup
  ( Semigroup (..)
  , SumInt (..)
  , ProductInt (..)
  , And (..)
  , Or (..)
  ) where

import Prelude hiding (Semigroup(..), concat)
import Test.SmallCheck (smallCheck)
import Test.SmallCheck.Series (Serial(..), Series)

-- #@@range_begin(class)
class Semigroup a where
  (<>) :: a -> a -> a
-- #@@range_end(class)

-- #@@range_begin(basic_instances)
-- #@@range_begin(int_instance)
instance Semigroup Int where
  (<>) = (+)
-- #@@range_end(int_instance)

instance Semigroup Float where
  (<>) = (+)

instance Semigroup [a] where
  (<>) = (++)

instance Semigroup Bool where
  (<>) = (&&)

instance Semigroup () where
  () <> () = ()
-- #@@range_end(basic_instances)

-- #@@range_begin(wrapper_types)
newtype SumInt = SumInt
  { unSumInt :: Int
  } deriving (Show, Eq, Num, Enum)

newtype ProductInt = ProductInt
  { unProductInt :: Int
  } deriving (Show, Eq, Num, Enum)

newtype SumFloat = SumFloat
  { unSumFloat :: Float
  } deriving (Show, Eq, Num)

newtype ProductFloat = ProductFloat
  { unProductFloat :: Float
  } deriving (Show, Eq, Num)

newtype And = And
  { unAnd :: Bool
  } deriving (Show, Eq)

newtype Or = Or
  { unOr :: Bool
  } deriving (Show, Eq)
-- #@@range_end(wrapper_types)

-- #@@range_begin(newtype_instances)
instance Semigroup SumInt where
  (<>) = (+)

instance Semigroup ProductInt where
  (<>) = (*)

instance Semigroup SumFloat where
  (<>) = (+)

instance Semigroup ProductFloat where
  (<>) = (*)

instance Semigroup And where
  And x <> And y = And $ x && y

instance Semigroup Or where
  Or x <> Or y = Or $ x || y
-- #@@range_end(newtype_instances)

instance Monad m => Serial m SumInt where
  series :: Series m SumInt
  series = SumInt <$> series

instance Monad m => Serial m ProductInt where
  series :: Series m ProductInt
  series = ProductInt <$> series

instance Monad m => Serial m SumFloat where
  series :: Series m SumFloat
  series = SumFloat <$> series

instance Monad m => Serial m ProductFloat where
  series :: Series m ProductFloat
  series = ProductFloat <$> series

instance Monad m => Serial m And where
  series :: Series m And
  series = And <$> series

instance Monad m => Serial m Or where
  series :: Series m Or
  series = Or <$> series

-- #@@range_begin(law)
assocLaw :: (Semigroup a, Eq a) => a -> a -> a -> Bool
assocLaw x y z =
  (x <> y) <> z == x <> (y <> z)
-- #@@range_end(law)

{-# ANN concat "HLint: ignore Eta reduce" #-}

-- #@@range_begin(practice)
concat :: Semigroup a => a -> [a] -> a
concat initial xs = foldl (<>) initial xs

result :: SumInt
result = sum [1..100]
  where
    sum :: [SumInt] -> SumInt
    sum [] = 0
    sum xs = concat 0 xs
-- #@@range_end(practice)

-- #@@range_begin(tests_for_law)
main :: IO ()
main = do
  smallCheck 2 $ assocLaw @ SumInt
  smallCheck 2 $ assocLaw @ ProductInt
  smallCheck 2 $ assocLaw @ SumFloat
  smallCheck 2 $ assocLaw @ ProductFloat
  smallCheck 2 $ assocLaw @ [Double]
  smallCheck 2 $ assocLaw @ And
  smallCheck 2 $ assocLaw @ Or
  smallCheck 2 $ assocLaw @ ()
-- #@@range_end(tests_for_law)
