module Ch15.Monoid where

import Ch15.Semigroup

-- Exercise 1 A
instance Monoid Trivial where
  mempty = Trivial

-- Exercise 2
instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

-- Exercise 3
instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

-- Exercise 4
instance Monoid BoolConj where
  mempty = BoolConj True

-- Exercise 5
instance Monoid BoolDisj where
  mempty = BoolDisj False

-- Exercise 6
instance Monoid b => Monoid (Combine a b) where
  mempty = Combine (\n -> mempty)

-- Exercise 7
instance Monoid a => Monoid (Comp a) where
  mempty = Comp mempty

---- Exercise 8
newtype Mem s a = Mem {runMem :: s -> (a, s)}

instance Semigroup a => Semigroup (Mem s a) where
  Mem {runMem = f} <> Mem {runMem = g} =
    Mem
      { runMem = \s ->
          let (a1, s1) = f s
              (a2, s2) = g s1
           in (a1 <> a2, s2)
      }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem {runMem = \s -> (mempty, s)}
