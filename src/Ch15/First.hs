module Ch15.First where

import Ch15.Optional

newtype First' a = First' {getFirst' :: Optional a} deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' {getFirst' = Nada}

instance Semigroup (First' a) where
  First' {getFirst' = Nada} <> First' {getFirst' = Nada} = First' {getFirst' = Nada}
  First' {getFirst' = Nada} <> First' {getFirst' = Only x} = First' {getFirst' = Only x}
  First' {getFirst' = Only x} <> First' _ = First' {getFirst' = Only x}
