-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}

module Data.MyTrie (TrieNode (..), Trie, TrieKey, TriePath, TrieZipper, refocus, get, update, insert, remove, singleton, lookup, empty, ofZipper, zipperOf) where

import Prelude hiding (lookup)

import Data.Binary
import Data.Bits
import Data.Function ((&))
-- import GHC.Generics (Generic)

-- import Data.Internal.RecursionSchemes
import Data.Utils

-- Abstract zippers
-- should we further require that pathF a = (focusKey, shape, [a]) ?
-- or that focusKey = (height, key) ?
data Zip pathF point otherdata = Zip {
    zipFocus :: point
      {- a pointed node within a data structure, or abstraction thereof -}
  , zipPath :: pathF otherdata
      {- path from the focus node back to the top,
         with abstractions of other branches -}
  }
  deriving (Show, Eq, {-Binary,-} Functor)

class PreZipper pathF stepF | pathF -> stepF where
  pathStep :: pathF a -> Maybe (stepF a, pathF a)
  stepDown :: stepF a -> pathF a -> pathF a

zipUp :: PreZipper pathF stepF =>
  (stepF b -> a -> a) -> Zip pathF a b -> Zip pathF a b
zipUp synth z@(Zip t p) =
  case pathStep p of
    Just (s, p') -> zipUp synth (Zip (synth s t) p')
    Nothing -> z

class PreZipper pathF stepF =>
  Zipper t node pathF stepF blur key |
      t -> node,
      node -> t pathF,
      pathF -> stepF blur key where
  zipperOf :: t -> Zip pathF node node
  ofTopZipper :: Zip pathF node node -> t
  stepUp :: stepF node -> node -> node
  refocus :: blur -> key -> Zip pathF node node -> Zip pathF node node

ofZipper :: Zipper top node pathF stepF blur key => Zip pathF node node -> top
ofZipper = ofTopZipper . zipUp stepUp


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

data TrieStep h k t
  = LeftStep t
  | RightStep t
  | SkipStep h k
  deriving (Show, Eq, {-Binary,-} Functor)

type TrieZip h k = Zip (TriePath h k)

type TrieZipper h k c = TrieZip (TrieNode h k c) (TrieNode h k c)

instance TrieKey h k => PreZipper (TriePath h k) (TrieStep h k) where
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
  -- stepDown from a TriePath
  stepDown step (TriePath h k m s) =
    case step of
      LeftStep t -> TriePath (h + 1) (k `shiftL` 1) (m `shiftL` 1) (t : s)
      RightStep t -> TriePath (h + 1) ((k `shiftL` 1) .|. 1) (m `shiftL` 1) (t : s)
      SkipStep hd kd ->
        let ld = 1 + fromIntegral hd
            h1 = h - ld
            k1 = (k `shiftL` ld) .|. kd
            m1 = (m `shiftL` ld) .|. lowBitsMask ld in
        TriePath h1 k1 m1 s

instance TrieKey h k => Zipper (Trie h k c) (TrieNode h k c) (TriePath h k) (TrieStep h k) Int k where
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

  -- TODO check each definition in this function for offby1 errors!
  {- refocus the zipper for the set of keys from k' `shiftL` h' included to
     (k' + 1) `shiftL` h' excluded -}
  refocus h' k' (Zip node path@(TriePath h0 k0 _ _)) =
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
          Zip t p
        else
          case t of
            -- base case: done
            Empty ->
              let l = h - h'
                  h1 = fromIntegral (l - 1)
                  k1 = k' .^. lowBitsMask l in
              Zip Empty (stepDown (SkipStep h1 k1) p)
            -- This case should never happen, being caught by h == h'
            Leaf _ -> Zip t p
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

  zipperOf (TrieTop h t) = Zip t (TriePath h 0 0 [])

  ofTopZipper (Zip t p) = trieTop (triePathHeight p) t


trieTop :: TrieKey h k => Int -> TrieNode h k c -> Trie h k c
trieTop h x =
  case x of
    Skip hh mm c ->
      let hhi = fromIntegral hh in
      if testBit mm hhi then
        TrieTop h x
      else
        {- topiary: prune unnecessary zero key bits at the Trie top.
           Note that we don't do it in stepUp because that would break
           the zipper height invariant, especially so when aligning the top
           of the focus to match two tries. -}
        let l = integerLength mm
            hh' = fromIntegral (l - 1)
            h' = h + hhi - l in
        TrieTop h' (Skip hh' mm c)
    _ -> TrieTop h x


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

pathSteps :: PreZipper pathF stepF => pathF a -> [stepF a]
pathSteps p = case pathStep p of
  Nothing -> []
  Just (s, p') -> s : pathSteps p'

stepHeight :: TrieStep h k d -> Int
stepHeight (LeftStep _) = 1
stepHeight (RightStep _) = 1
stepHeight (SkipStep h _) = 1 + fromIntegral h

stepBits :: TrieStep h k d -> k -> k
stepBits (LeftStep _) k = k `shiftL` 1
stepBits (RightStep _) k = (k `shiftL` 1) .|. 1
stepBits (SkipStep h ks) k = (k `shiftL` (1 + fromIntegral h)) .|. ks
-}

{-
getMerkleProof :: (TrieKey h k, Digesting d) => Trie h k c -> k -> TriePath h k c
getMerkleProof trie key =
  zipperOf trie & refocus 0 k & zipPath & fmap getTrieDigest

getMerkleProof :: (TrieKey h k) => Trie h k c -> k -> TriePath h k c
isMerkleProof trieDigest key value proof =
  trieDigest == zipUp digestSynth (Zip (digestSynth $ Leaf value) proof) &&
  key == triePathKey . zipPath $ proof
-}

update :: TrieKey h k => (Maybe v -> Maybe v) -> k -> Trie h k v -> Trie h k v
update updateLeaf key trie =
  case refocus 0 key (zipperOf trie) of
    Zip t path ->
      Zip (t & maybeOfLeaf & updateLeaf & leafOfMaybe) path & ofZipper
  where
    leafOfMaybe Nothing = Empty
    leafOfMaybe (Just v) = Leaf v

-- The definition suggests we should swap v and k, so insert = update . const . Just
insert :: TrieKey h k => k -> v -> Trie h k v -> Trie h k v
insert k v = update (const (Just v)) k

remove :: TrieKey h k => k -> Trie h k v -> Trie h k v
remove = update (const Nothing)

lookup :: TrieKey h k => Trie h k a -> k -> Maybe a
lookup t k = refocus 0 k (zipperOf t) & zipFocus & maybeOfLeaf

maybeOfLeaf :: TrieNode h k a -> Maybe a
maybeOfLeaf (Leaf v) = Just v
maybeOfLeaf _ = Nothing -- only the Empty case should be used

singleton :: TrieKey h k => k -> a -> Trie h k a
singleton k v = insert k v empty

empty :: TrieKey h k => Trie h k a
empty = TrieTop (-1) Empty
