{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Trie.Zipper
  ( TrieZipper,
    Where (..),
    goLeft,
    goRight,
    goUp,
    focus,
    upmostWith,
    upmost,
    modify,
    zipper,
    -- some accessors
    getValue,
    getKey,
    getKeyValue,
    getNode,
    direction,
  )
where

import Data.Trie.Internal

data Cxt k h a
  = Top
  | L h k (Cxt k h a) (Trie k a)
  | R h k (Trie k a) (Cxt k h a)
  deriving (Functor)

newtype TrieZipper' k h a = TrieZipper {getZipper :: (Trie k a, Cxt k h a)} deriving (Functor)

type TrieZipper k a = TrieZipper' k (TrieHeight k) a

zipper :: (TrieKey k) => Trie k a -> TrieZipper k a
zipper t = TrieZipper (t, Top)

data Where = LeftBranch | RightBranch deriving (Show, Eq)

direction :: (TrieKey k) => TrieZipper k a -> Maybe Where
direction (getZipper -> (_, Top)) = Nothing
direction (getZipper -> (_, L {})) = Just LeftBranch
direction (getZipper -> (_, R {})) = Just RightBranch

-- TODO do we want to return Maybe zipper or just return the same one if the move is errorneous?
-- Maybe 2 family of functions for this?
goLeft, goRight :: (TrieKey k) => TrieZipper k a -> Maybe (TrieZipper k a)
-- nowhere to go if we're at the leaf or in an empty tree
goLeft (getZipper -> (Empty, _)) = Nothing
goLeft (getZipper -> (Leaf {}, _)) = Nothing
goLeft (getZipper -> (Branch {..}, c)) = Just $ TrieZipper (left, L height prefix c right)
-- nowhere to go if we're at the leaf or in an empty tree
goRight (getZipper -> (Empty, _)) = Nothing
goRight (getZipper -> (Leaf _ _, _)) = Nothing
goRight (getZipper -> (Branch {..}, c)) = Just $ TrieZipper (right, R height prefix left c)

-- we're already at the top
goUp :: (TrieKey k) => TrieZipper k a -> TrieZipper k a
goUp z@(getZipper -> (_, Top)) = z
goUp (getZipper -> (t, L height prefix c r)) = TrieZipper (Branch height prefix t r, c)
goUp (getZipper -> (t, R height prefix l c)) = TrieZipper (Branch height prefix l t, c)

-- | Basically a fold from the current node up to the top of the trie.
-- | If current node is not a leaf - returns Nothing
-- | Accepts accumulator 'b' and combine function '(b -> Trie k a -> b)', where
-- | 'b' - starting value, 'Trie k a' - current node
-- TODO look into how to do this through recustion schemes, since this is
-- basically recursion scheme style recursion, we're "visiting" each node and doing something with it
-- then accumulating the result.
upmostWith :: (TrieKey k) => (b -> Trie k a -> b) -> b -> TrieZipper k a -> (b, TrieZipper k a)
upmostWith _ acc z@(getZipper -> (_, Top)) = (acc, z)
upmostWith f acc z = upmostWith f (f acc (getNode z)) $ goUp z

upmost :: (TrieKey k) => TrieZipper k a -> TrieZipper k a
-- undefined should not cause any troubles due to laziness
-- and if it does - we've made a mistake somewhere :P
upmost = snd . upmostWith const undefined

modify :: (Trie k a -> Trie k a) -> TrieZipper k a -> TrieZipper k a
modify f (getZipper -> (t, c)) = TrieZipper (f t, c)

getNode :: (TrieKey k) => TrieZipper k a -> Trie k a
getNode (getZipper -> (t, _)) = t

getKeyValue :: (TrieKey k) => TrieZipper k a -> Maybe (k, a)
getKeyValue t = (,) <$> getKey t <*> getValue t

getValue :: (TrieKey k) => TrieZipper k a -> Maybe a
getValue (getZipper -> (Empty, _)) = Nothing
getValue (getZipper -> (Branch {}, _)) = Nothing
getValue (getZipper -> (Leaf {..}, _)) = Just value

getKey :: (TrieKey k) => TrieZipper k a -> Maybe k
getKey (getZipper -> (Empty, _)) = Nothing
getKey (getZipper -> (Branch {}, _)) = Nothing
getKey (getZipper -> (Leaf {..}, _)) = Just key

-- | Create a `TrieZipper` focused on given key `k` in a trie `Trie w k a`
focus :: (TrieKey k) => k -> Trie k a -> Maybe (TrieZipper k a)
focus _ Empty = Nothing
focus key1 t = go key1 $ zipper t
  where
    go :: (TrieKey k) => k -> TrieZipper k a -> Maybe (TrieZipper k a)
    go _ (getZipper -> (Empty, _)) = Nothing
    go k z@(getZipper -> (Leaf {..}, _)) = if key == k then Just z else Nothing
    go k z@(getZipper -> (Branch {..}, _))
      | not (matchPrefix k prefix (heightToBBit height)) = Nothing
      | zeroBit k (heightToBBit height) = goLeft z >>= go k
      | otherwise = goRight z >>= go k
