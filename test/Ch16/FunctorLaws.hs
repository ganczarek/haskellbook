module Ch16.FunctorLaws where

import Test.QuickCheck.Function

functorIdentityLaw :: (Eq (f a), Functor f) => f a -> Bool
functorIdentityLaw f = fmap id f == f

functorCompositionLaw :: (Eq (f c), Functor f) => Fun a b -> Fun b c -> f a -> Bool
functorCompositionLaw (Fun _ f) (Fun _ g) x = fmap (g . f) x == (fmap g . fmap f) x
