{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Trie.Zipper (TrieZipper, left, right, up, lookupZipper, zipper) where

-- import Control.Monad.Identity (Identity (..))
import Data.Kind (Type)
import Data.Trie.Internal

data Cxt (f :: Type -> Type) k a
  = Top
  | L k k (Cxt f k a) (Trie f k a)
  | R k k (Trie f k a) (Cxt f k a)

newtype TrieZipper (f :: Type -> Type) k a = TrieZipper {getZipper :: (Trie f k a, Cxt f k a)}

zipper :: (TrieKey k, Functor f) => Trie f k a -> TrieZipper f k a
zipper t = TrieZipper (t, Top)

left, right, up :: (TrieKey k) => TrieZipper f k a -> Maybe (TrieZipper f k a)
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

-- TODO not sure if this is what we want, need more research

-- | Supplied function '(k -> a -> b)' takes key 'k' and value 'a' and returns 'b'
-- upmostWith :: (TrieKey k, Functor f, Applicative f) => (k -> a -> b) -> TrieZipper f k a -> f (TrieZipper f k a)
-- upmostWith _ z@(getZipper -> (_, Top)) = pure z
-- upmostWith _ z@(getZipper -> (Empty, _)) = pure z
-- upmostWith f z@(getZipper -> (Leaf key fa, c)) = undefined
-- upmostWith f z@(getZipper -> (Branch bBit pref l r, c)) = undefined

-- upmost :: (TrieKey k) => TrieZipper Identity k a -> TrieZipper Identity k a
-- upmost = runIdentity . upmostWith const

lookupZipper :: (TrieKey k, Functor f) => k -> Trie f k a -> Maybe (TrieZipper f k a)
lookupZipper _ Empty = Nothing
lookupZipper key1 t = go key1 $ TrieZipper (t, Top)
  where
    go :: (TrieKey k, Functor f) => k -> TrieZipper f k a -> Maybe (TrieZipper f k a)
    go _ (getZipper -> (Empty, _)) = Nothing
    go key z@(getZipper -> (Leaf k _, _)) = if key == k then Just z else Nothing
    go key z@(getZipper -> (Branch bBit pref _ _, _))
      | not (matchPrefix key pref bBit) = Nothing
      | zeroBit key bBit = left z >>= go key
      | otherwise = right z >>= go key
