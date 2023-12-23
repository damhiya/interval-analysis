{-# LANGUAGE FunctionalDependencies #-}

module Apply where

class Apply f a b | f a -> b where
  apply :: f -> a -> b
