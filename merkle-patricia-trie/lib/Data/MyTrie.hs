-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.MyTrie (TrieNode (..), Trie, TrieKey, TriePath, TrieZipper, refocus, get, update, insert, remove, singleton, lookup, empty, ofZipper, zipperOf) where

import Prelude hiding (lookup)

import Data.Binary
import Data.Bits
import Data.Function ((&))
-- import GHC.Generics (Generic)

-- import Data.Internal.RecursionSchemes
import Data.Utils

-- TODO make it work with Bits k as well as FiniteBits k?
-- h ::: a type for the height of keys, e.g. UInt8 for k==UInt256
-- k ::: a type for the keys of the Trie, e.g. UInt256
class (Num h, Integral h, {- Binary h, -}
       Num k, Integral k, {- Binary k, -} FiniteBits k) => TrieKey h k where

-- TODO: add reference functor? synthesized attribute type on non-leaves?
-- e.g. ... Wrapable r, Binary a... => Trie r h k a c
-- c ::: a type for the content of the leaves of the Trie
data (TrieKey h k {-, Binary c-}) =>
  Trie h k c = TrieTop {
    trieHeight :: Int
      {- integerLength of the largest key in the Trie; -1 if empty -}
  , trieTopNode :: TrieNode h k c
      {- top node in the Trie -}
  }
  deriving (Show, Eq {-, Binary-})

data (TrieKey h k {-, Binary c-}) =>
  TrieNode h k c =
    Empty
  | Leaf c
  | Branch {left :: TrieNode h k c, right :: TrieNode h k c}
  | Skip {heightMinus1 :: h, bits :: k, child :: TrieNode h k c}
  deriving (Show, Eq, {-Binary,-} Functor)

data (TrieKey h k {-, Binary d-}) =>
  TriePath h k d = TriePath {
    triePathHeight :: Int
      {- the height of lowest key bit for the node *above* the focused node,
         which is 0 for a Leaf, 256 if the key is 256 bits with high bit set.
         Note that may thus not fit the type h of the height,
         e.g. because h==UInt8, k==UInt256 and 256 > 255 the max value in h. -}
  , triePathKey :: k
      {- bits of key starting from triePathHeight (:: Int) of the pointed node. -}
  , triePathSkipMask :: k
      {- bits of a mask indicating which of the bits of k1 are skipped. -}
  , triePathOthers :: [d]
      {- the list s1 of data items from the *other* branches
         (no data for skip nodes), such that the first item corresponds
         to the first (low-order) bits of k1. -}
  }
  deriving (Show, Eq, {-Binary,-} Functor)

-- An abstract Trie zipper
data (TrieKey h k {-, Binary t-}) =>
  TrieZip h k t = TrieZip {
    trieZipFocus :: t
      {- a pointed node within a Trie, or abstraction thereof -}
  , trieZipPath :: TriePath h k t
      {- path from the focus node back to the top,
         with abstractions of other branches -}
  }
  deriving (Show, Eq, {-Binary,-} Functor)

type TrieZipper h k c = TrieZip h k (TrieNode h k c)

data TrieStep h k t
  = LeftStep t
  | RightStep t
  | SkipStep h k
  deriving (Show, Eq, {-Binary,-} Functor)

{- Probably not needed
-- the maybeHeight is the height of the largest bit, the int is one more than that.
intOfHeight :: IntegralSized h => h -> Int
intOfHeight h = 1 + fromIntegral h

intOfMaybeHeight :: Integral h => Maybe h -> Int
intOfMaybeHeight Nothing = 0
intOfMaybeHeight (Just h) = intOfHeight h

maybeHeightOfInt :: IntegralSized h => Int -> Maybe h
maybeHeightOfInt 0 = Nothing
maybeHeightOfInt i = Just (fromIntegral (i - 1))
-}

zipperOf :: (TrieKey h k {-, Binary c-}) => Trie h k c -> TrieZipper h k c
zipperOf (TrieTop h t) = TrieZip t (TriePath h 0 0 [])

pathStep :: TrieKey h k => TriePath h k a -> Maybe (TrieStep h k a, TriePath h k a)
pathStep (TriePath h k m s) =
  if testBit m 0 then
    let h1 = lowestBitClear m
        k1 = k .&. lowBitsMask h1
        hr = h + h1
        kr = k `shiftR` h1
        mr = m `shiftR` h1 in
    Just (SkipStep (fromIntegral $ h1-1) k1, TriePath hr kr mr s)
  else case s of
    t : sr -> let hr = h - 1
                  kr = k `shiftR` 1
                  mr = m `shiftR` 1
                  branch = if testBit m 0 then LeftStep else RightStep in
              Just (branch t, TriePath hr kr mr sr)
    [] -> Nothing

{-
pathSteps :: TrieKey h k => TriePath h k a -> [TrieStep h k a]
pathSteps p = case pathStep p of
  Nothing -> []
  Just (s, p') -> s : pathSteps p'
-}

-- stepUp from a TrieNode
stepUp :: TrieKey h k =>
  TrieStep h k (TrieNode h k c) -> TrieNode h k c -> TrieNode h k c
stepUp (LeftStep r) Empty = stepUp (SkipStep 0 1) r
stepUp (LeftStep Empty) l = stepUp (SkipStep 0 0) l
stepUp (LeftStep r) l = Branch l r
stepUp (RightStep l) Empty = stepUp (SkipStep 0 0) l
stepUp (RightStep Empty) r = stepUp (SkipStep 0 1) r
stepUp (RightStep l) r = Branch l r
stepUp (SkipStep _ _) Empty = Empty
stepUp (SkipStep h k) (Skip h0 k0 c0) =
  Skip (h + h0 + 1) ((k `shiftL` (1 + fromIntegral h0)) .|. k0) c0
stepUp (SkipStep h k) c = Skip h k c

{- Probably not needed
stepHeight :: TrieStep h k d -> Int
stepHeight (LeftStep _) = 1
stepHeight (RightStep _) = 1
stepHeight (SkipStep h _) = 1 + fromIntegral h

stepBits :: TrieStep h k d -> k -> k
stepBits (LeftStep _) k = k `shiftL` 1
stepBits (RightStep _) k = (k `shiftL` 1) .|. 1
stepBits (SkipStep h ks) k = (k `shiftL` (1 + fromIntegral h)) .|. ks
-}

ofZipper :: (TrieKey h k {-, Binary c-}) => TrieZipper h k v -> Trie h k v
ofZipper (TrieZip t p) =
  case pathStep p of
    Just (s, p') -> ofZipper (TrieZip (stepUp s t) p')
    Nothing -> TrieTop (triePathHeight p) t

-- stepUp from a TriePath
stepDown :: TrieKey h k => TrieStep h k a -> TriePath h k a -> TriePath h k a
stepDown step (TriePath h k m s) =
  case step of
    LeftStep t -> TriePath (h + 1) (k `shiftL` 1) (m `shiftL` 1) (t : s)
    RightStep t -> TriePath (h + 1) ((k `shiftL` 1) .|. 1) (m `shiftL` 1) (t : s)
    SkipStep hd kd ->
      let ld = 1 + fromIntegral hd
          h1 = h - ld
          (k1, m1) = if null s && k == 0 then
                       -- topiary: prune unnecessary zero key bits at the Trie top
                       ( kd
                       , lowBitsMask $ integerLength kd )
                     else
                       ( (k `shiftL` ld) .|. kd
                       , (m `shiftL` ld) .|. lowBitsMask ld ) in
      TriePath h1 k1 m1 s

-- TODO check each definition in this function for offby1 errors!
{- refocus the zipper for the set of keys from k' `shiftL` h' included to
   (k' + 1) `shiftL` h' excluded -}
refocus :: TrieKey h k => Int -> k -> TrieZipper h k c -> TrieZipper h k c
refocus h' k' (TrieZip node path@(TriePath h0 k0 _ _)) =
  ascend node path where
    {- hcommon: height up to which to ascend: no less than the desired height
       but also no less than necessary for there being a branch to our desired key
       and no less than necessary for there being a branch to the current key
       and yet no more than necessary. -}
    hcommon = h' `max`
              integerLength ((k0 `shiftL` h0) .^. (k' `shiftL` h'))
    {- Note that for very long keys, this bitwise-xor is already a log N operation,
       in which case maintaining an O(1) amortized cost would require us to
       take as parameter an incremental change on a zipper for the height
       and return an accordingly modified zipper for the Trie.
       In practice we use 256-bit keys for Cardano, which is borderline. -}

    {- ascend: go up the tree from old focus until h >= hcommon
       (then descend to new focus) -}
    ascend t p@(TriePath h _ _ _) =
      if h >= hcommon then
        descend t p
      else
        case pathStep p of
          -- Can go up the original tree? Do.
          Just (s, p') -> ascend (stepUp s t) p'
          -- Can't go up the original tree? Extend it!
          {- At this point we're still below the desired level,
             but there are no more steps to zip up in the original trie
             so k is 0, h is the original trie height, and
             hcommon is h' + integerLength k',
             which means we have to create additional trie nodes
             to accommodate space for the new key (k' `shiftL` h')
             and take a RightStep from it. -}
          Nothing ->
            let l1 = hcommon - h - 2
                h1 = fromIntegral (l1 - 1)
                t1 = stepUp (SkipStep h1 0) t in
            descend Empty (TriePath (hcommon - 1) 1 0 [t1])

    -- descend: descend toward the sought focus from a pointed node above
    descend t p@(TriePath h _ _ _) =
      if h == h' then
        TrieZip t p
      else
        case t of
          -- base case: done
          Empty ->
            let l = h - h'
                h1 = fromIntegral (l - 1)
                k1 = k' .^. lowBitsMask l in
            TrieZip Empty (stepDown (SkipStep h1 k1) p)
          -- This case should never happen, being caught by h == h'
          Leaf _ -> TrieZip t p
          -- recursive case
          Branch l r ->
            let (step, t') = if testBit k' (h - h' - 1) then
                              (RightStep l, r)
                            else
                              (LeftStep r, l) in
            descend t' (stepDown step p)
          -- hard case: descending common then uncommon parts of a Skip
          Skip bitsHeightMinus1 bits child ->
            let childHeight = h - (fromIntegral bitsHeightMinus1) - 1
                floorHeight = h' `max` childHeight
                comparableLength = h - floorHeight
                keyBits = extractBitField comparableLength
                            (floorHeight - h') k'
                nodeBits = extractBitField comparableLength
                            (floorHeight - childHeight) bits
                diffLength = integerLength (keyBits .^. nodeBits) in
            if diffLength == 0 then
              -- Not so hard: if it was the same key all the way that matters
              let llo = floorHeight - childHeight
                  hlo = fromIntegral (llo - 1)
                  blo = bits .^. lowBitsMask llo
                  hhi = fromIntegral (comparableLength - 1)
                  bhi = bits `shiftR` llo in
              descend (stepUp (SkipStep hlo blo) child)
                      (stepDown (SkipStep hhi bhi) p)
            else -- harder: keys differ in that bit range
              let sameLength = comparableLength - diffLength
                  branchNodeHeight = h - sameLength -- height right below which the keys differ
                  branchHeight = branchNodeHeight - 1 -- height of the two new branches
                  oldBranchLength = branchHeight - childHeight
                  oldBranch = if oldBranchLength > 0 then
                                let hh = fromIntegral (oldBranchLength - 1)
                                    bb = bits .^. lowBitsMask oldBranchLength in
                                stepUp (SkipStep hh bb) child
                              else
                                child
                  branch = oldBranch &
                           if testBit k' (branchNodeHeight - h' - 1) then
                             RightStep
                           else
                             LeftStep
                  skipSame = SkipStep (fromIntegral sameLength - 1)
                                      (bits `shiftR` (branchNodeHeight - childHeight)) in
                  descend Empty (p & stepDown skipSame & stepDown branch)

update :: TrieKey h k => (Maybe v -> Maybe v) -> k -> Trie h k v -> Trie h k v
update updateLeaf key trie =
  case refocus 0 key (zipperOf trie) of
    TrieZip t path ->
      TrieZip (t & maybeOfLeaf & updateLeaf & leafOfMaybe) path & ofZipper
  where
    leafOfMaybe Nothing = Empty
    leafOfMaybe (Just v) = Leaf v

insert :: TrieKey h k => k -> v -> Trie h k v -> Trie h k v
insert k v t = update (const (Just v)) k t

remove :: TrieKey h k => k -> Trie h k v -> Trie h k v
remove k t = update (const Nothing) k t

lookup :: TrieKey h k => Trie h k a -> k -> Maybe a
lookup t k =
  case refocus 0 k (zipperOf t) of
    TrieZip Empty _ -> Nothing
    TrieZip (Leaf x) _ -> Just x
    TrieZip _ _ -> Nothing -- should never happen!

maybeOfLeaf :: TrieNode h k a -> Maybe a
maybeOfLeaf (Leaf v) = Just v
maybeOfLeaf _ = Nothing -- only the Empty case should be used

singleton :: TrieKey h k => k -> a -> Trie h k a
singleton k v = insert k v empty

empty :: TrieKey h k => Trie h k a
empty = TrieTop (-1) Empty
