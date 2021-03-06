{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Expose pricipal data with the instances
module Core where

import Data.Ratio ((%))
import Test.SmallCheck.Series (Serial(..), Series)

-- #@@range_begin(wrapper_types)
newtype Sum = Sum
  { unSum :: Integer
  } deriving (Show, Eq, Num, Enum)

newtype Product = Product
  { unProduct :: Integer
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

xor :: Bool -> Bool -> Bool
xor True  True  = False
xor True  False = True
xor False True  = True
xor False False = False

newtype Xor = Xor
  { unXor :: Bool
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

instance Monad m => Serial m Xor where
  series :: Series m Xor
  series = Xor <$> series
