{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Expose pricipal data with the instances
module Core where

import Test.SmallCheck.Series (Serial(..), Series)

-- #@@range_begin(wrapper_types)
newtype Sum = Sum
  { unSum :: Int
  } deriving (Show, Eq, Num, Enum)

newtype Product = Product
  { unProduct :: Int
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

instance Monad m => Serial m And where
  series :: Series m And
  series = And <$> series

instance Monad m => Serial m Or where
  series :: Series m Or
  series = Or <$> series
