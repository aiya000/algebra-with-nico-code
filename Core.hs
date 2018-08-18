{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Expose pricipal data with the instances
module Core where

import Data.Ratio (Rational, (%))
import Test.SmallCheck.Series (Serial(..), Series)

-- #@@range_begin(wrapper_types)
newtype Sum = Sum
  { unSum :: Int
  } deriving (Show, Eq, Num, Enum)

newtype Product = Product
  { unProduct :: Int
  } deriving (Show, Eq, Num, Enum)

newtype RSum = RSum
  { unRSum :: Rational
  } deriving (Show, Eq, Num, Enum)

newtype RProduct = RProduct
  { unRProduct :: Rational
  } deriving (Show, Eq, Num, Enum)

newtype And = And
  { unAnd :: Bool
  } deriving (Show, Eq)

newtype Or = Or
  { unOr :: Bool
  } deriving (Show, Eq)
-- #@@range_end(wrapper_types)

instance Monad m => Serial m Sum where
  series :: Series m Sum
  series = Sum <$> series

instance Monad m => Serial m Product where
  series :: Series m Product
  series = Product <$> series

instance Monad m => Serial m RSum where
  series :: Series m RSum
  series = RSum <$> series

instance Monad m => Serial m RProduct where
  series :: Series m RProduct
  series = RProduct <$> series

instance Monad m => Serial m And where
  series :: Series m And
  series = And <$> series

instance Monad m => Serial m Or where
  series :: Series m Or
  series = Or <$> series
