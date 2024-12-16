{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Trie.Zipper (TrieZipper, left, right, up, focus, zipper) where

import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Trie.Internal
import Data.Utils

data Cxt (w :: Type -> Type) k a
  = Top
  | L k k (Cxt w k a) (Trie w k a)
  | R k k (Trie w k a) (Cxt w k a)
  deriving (Functor)

newtype TrieZipper (w :: Type -> Type) k a = TrieZipper {getZipper :: (Trie w k a, Cxt w k a)} deriving (Functor)

zipper :: (TrieKey k, Wrapper w) => Trie w k a -> TrieZipper w k a
zipper t = TrieZipper (t, Top)

left, right, up :: (TrieKey k) => TrieZipper w k a -> Maybe (TrieZipper w k a)
-- nowhere to go if we're at the leaf or in an empty tree
left (getZipper -> (Empty, _)) = Nothing
left (getZipper -> (Leaf _ _, _)) = Nothing
left (getZipper -> (Branch bBit pref l r, c)) = Just $ TrieZipper (l, L bBit pref c r)
-- nowhere to go if we're at the leaf or in an empty tree
right (getZipper -> (Empty, _)) = Nothing
right (getZipper -> (Leaf _ _, _)) = Nothing
right (getZipper -> (Branch bBit pref l r, c)) = Just $ TrieZipper (r, R bBit pref l c)
-- we're already at the top
up (getZipper -> (_, Top)) = Nothing
up (getZipper -> (t, L bBit pref c r)) = Just $ TrieZipper (Branch bBit pref t r, c)
up (getZipper -> (t, R bBit pref l c)) = Just $ TrieZipper (Branch bBit pref l t, c)

modify :: (Wrapper w) => (Trie w k a -> Trie w k a) -> TrieZipper w k a -> TrieZipper w k a
modify f (getZipper -> (t, c)) = TrieZipper (f t, c)

-- | Move focus to the top of the trie applying `(k -> a -> a)` along the way
-- Supplied function '(k -> a -> a)' takes key 'k' and value 'a' and returns 'a'
unfocusWith :: (TrieKey k, Wrapper w) => (k -> a -> b) -> TrieZipper w k a -> TrieZipper w k b
unfocusWith f z@(getZipper -> (_, Top)) = fmap f z
unfocusWith f z@(getZipper -> (Empty, _)) = fmap f z
unfocusWith f z@(getZipper -> (Leaf key fa, c)) = unfocusWith f . fromMaybe z . up $ TrieZipper (Leaf key $ fmap (f key) fa, fmap f c)
unfocusWith f z@(getZipper -> (Branch bBit pref l r, c)) = unfocusWith f . fromMaybe z $ up z

-- | Move focus to the top of the trie
unfocus :: (TrieKey k, Wrapper w) => TrieZipper w k a -> TrieZipper w k a
unfocus = upmostWith (const id)

-- | Create a `TrieZipper` focused on given key `k` in a trie `Trie w k a`
focus :: (TrieKey k, Wrapper w) => k -> Trie w k a -> Maybe (TrieZipper w k a)
focus _ Empty = Nothing
focus key1 t = go key1 $ TrieZipper (t, Top)
  where
    go :: (TrieKey k, Wrapper w) => k -> TrieZipper w k a -> Maybe (TrieZipper w k a)
    go _ (getZipper -> (Empty, _)) = Nothing
    go key z@(getZipper -> (Leaf k _, _)) = if key == k then Just z else Nothing
    go key z@(getZipper -> (Branch bBit pref _ _, _))
      | not (matchPrefix key pref bBit) = Nothing
      | zeroBit key bBit = left z >>= go key
      | otherwise = right z >>= go key
