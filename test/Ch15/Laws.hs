module Ch15.Laws where

leftIdentityLaw :: (Eq m, Monoid m) => m -> Bool
leftIdentityLaw a = (mempty <> a) == a

rightIdentityLaw :: (Eq m, Monoid m) => m -> Bool
rightIdentityLaw a = (a <> mempty) == a

associativityLaw :: (Eq m, Semigroup m) => m -> m -> m -> Bool
associativityLaw a b c = (a <> (b <> c)) == ((a <> b) <> c)