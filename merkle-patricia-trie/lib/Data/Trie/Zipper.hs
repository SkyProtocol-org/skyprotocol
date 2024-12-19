{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Trie.Zipper (TrieZipper, left, right, up, focus, zipper) where

import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Trie.Internal

data Cxt (f :: Type -> Type) k h a
  = Top
  | L k h (f a) (Cxt f k h a) (Trie k a)
  | R k h (f a) (Trie k a) (Cxt f k h a)
  deriving (Functor)

newtype TrieZipper' (f :: Type -> Type) k h a = TrieZipper {getZipper :: (Trie k a, Cxt f k h a)} deriving (Functor)

type TrieZipper (f :: Type -> Type) k a = TrieZipper' f k (TrieHeight k) a

zipper :: (TrieKey k, Functor f) => Trie k a -> TrieZipper f k a
zipper t = TrieZipper (t, Top)

-- TODO do we want to return Maybe zipper or just return the same one if the move is errorneous?
-- Maybe 2 family of functions for this?
left, right, up :: (TrieKey k, Applicative f) => TrieZipper f k a -> Maybe (TrieZipper f k a)
-- nowhere to go if we're at the leaf or in an empty tree
left (getZipper -> (Empty, _)) = Nothing
left (getZipper -> (Leaf {}, _)) = Nothing
-- TODO how to calculate values here? For leaf it's easy, as for branch? It must depend on the leafes nearby, I guess
left (getZipper -> (Branch {..}, c)) = Just $ TrieZipper (lelft, L branchingBit prefix undefined c right)
-- nowhere to go if we're at the leaf or in an empty tree
right (getZipper -> (Empty, _)) = Nothing
right (getZipper -> (Leaf _ _, _)) = Nothing
right (getZipper -> (Branch {..}, c)) = Just $ TrieZipper (right, R branchingBit prefex undefined left c)
-- we're already at the top
up (getZipper -> (_, Top)) = Nothing
up (getZipper -> (t, L branchingBit prefix v c r)) = Just $ TrieZipper (Branch branchingBit prefix t r, c)
up (getZipper -> (t, R branchingBit prefix v l c)) = Just $ TrieZipper (Branch branchingBit prefix l t, c)

modify :: (Applicative f) => (Trie k a -> Trie k a) -> TrieZipper f k a -> TrieZipper f k a
modify f (getZipper -> (t, c)) = TrieZipper (f t, c)

-- | Move focus to the top of the trie applying `(k -> a -> a)` along the way
-- Supplied function '(k -> a -> a)' takes key 'k' and value 'a' and returns 'a'
unfocusWith :: (TrieKey k, Applicative f) => (k -> a -> b) -> TrieZipper f k a -> TrieZipper f k b
unfocusWith f z@(getZipper -> (_, Top)) = fmap f z
unfocusWith f z@(getZipper -> (Empty, _)) = fmap f z
unfocusWith f z@(getZipper -> (Leaf key fa, c)) = unfocusWith f . fromMaybe z . up $ TrieZipper (Leaf key $ fmap (f key) fa, fmap f c)
unfocusWith f z@(getZipper -> (Branch bBit pref l r, c)) = unfocusWith f . fromMaybe z $ up z

-- | Move focus to the top of the trie
unfocus :: (TrieKey k, Applicative f) => TrieZipper f k a -> TrieZipper f k a
unfocus = upmostWith (const id)

-- | Create a `TrieZipper` focused on given key `k` in a trie `Trie w k a`
focus :: (TrieKey k, Applicative f) => k -> Trie k a -> Maybe (TrieZipper f k a)
focus _ Empty = Nothing
focus key1 t = go key1 $ TrieZipper (t, Top)
  where
    go :: (TrieKey k, Functor f) => k -> TrieZipper f k a -> Maybe (TrieZipper f k a)
    go _ (getZipper -> (Empty, _)) = Nothing
    go k z@(getZipper -> (Leaf {..}, _)) = if key == k then Just z else Nothing
    go k z@(getZipper -> (Branch {..}, _))
      | not (matchPrefix k prefix (heightToBBit branchingBit)) = Nothing
      | zeroBit k (heightToBBit branchingBit) = left z >>= go k
      | otherwise = right z >>= go k
