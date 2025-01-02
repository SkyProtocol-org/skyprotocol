module Data.Internal.RecursionSchemes where

import Control.Arrow ((&&&), (<<<), (>>>))

-- | Y-combinator or fixed point combinator for types
newtype Term f = In {out :: f (Term f)}

-- | Type of functions from a container `f a` to the collapsed value `a`.
-- | Basically it's a type of functions for cata-morphisms a.k.a. folds.
type Algebra f a = f a -> a

-- | Type of functions from a value `a` to the unfolded container `f a`
-- | Basically it's a type of functions for ana-morphisms a.k.a. unfolds.
type CoAlgebra f a = a -> f a

-- | Type of functions for folds with extra context of `Term f` we're currently working on,
-- | and value `a` to the collapsed value `a` a.k.a. para-morphisms.
type RAlgebra f a = f (Term f, a) -> a

-- | Type of functions for unfolds that can be either terminated on `Term f` or continued on value `a`,
-- | a.k.a. apo-morphism
type RCoAlgebra f a = a -> f (Either (Term f) a)

-- | Attributeted version of `Term f`. Used to hold the "history" of the fold that is currently going on.
-- | Helper data type for the histo-morphism
data Attr f a = Attr
  { attribute :: a,
    hole :: f (Attr f a)
  }

-- | Type of functions for folds that remembers their "history" as they go through the recursion,
-- | a.k.a. histo-morphism
type CVAlgebra f a = f (Attr f a) -> a

-- | The dual of Attr for futu-morphisms, a.k.a. unfolds with control flow
data CoAttr f a
  = Automatic a
  | Manual (f (CoAttr f a))

-- | Type of functions for unfold that has control over the flow of the unfold, a.k.a. futu-morphism
type CVCoAlgebra f a = a -> f (CoAttr f a)

-- | Histomorphism
histo :: forall f a. (Functor f) => CVAlgebra f a -> Term f -> a
histo h = attribute . go
  where
    go :: Term f -> Attr f a
    go = uncurry Attr . (h &&& id) . fmap go . out

-- | Catamorphism
cata :: (Functor f) => Algebra f a -> Term f -> a
cata f = histo (f . fmap attribute)

-- | Paramorphism
para :: (Functor f) => RAlgebra f a -> Term f -> a
para f = histo (f . fmap go)
  where
    go (Attr a h) = (In (fmap (fst . go) h), a)

-- | Futumorphism
futu :: forall f a. (Functor f) => CVCoAlgebra f a -> a -> Term f
futu f = In . fmap go . f
  where
    go :: CoAttr f a -> Term f
    go (Automatic a) = futu f a -- continue through this level
    go (Manual g) = In (fmap go g) -- omit folding this level, delegating to the worker

-- | Anamorphism
ana :: (Functor f) => CoAlgebra f a -> a -> Term f
ana f = futu (fmap Automatic . f)

-- | Apomorphism
apo :: (Functor f) => RCoAlgebra f a -> a -> Term f
apo f = futu (fmap (either termToCoAttr Automatic) . f)
  where
    termToCoAttr = Manual . fmap termToCoAttr . out
