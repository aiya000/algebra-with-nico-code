{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Expose pricipal data with the instances
module Core where

import Test.SmallCheck.Series (Serial(..), Series)

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
