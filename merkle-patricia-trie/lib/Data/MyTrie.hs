-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FunctionalDependencies, FlexibleInstances, InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Data.MyTrie (TrieNode (..), Trie, TrieKey, TriePath, TrieZipper, refocus, get, update, insert, remove, singleton, lookup, empty, ofZipper, zipperOf) where

import Prelude hiding (lookup)

import Data.Binary
import Data.Bits
-- import Data.Function ((&))
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

class Monad e => PreZipper e pathF stepF | pathF -> stepF where
  pathStep :: pathF a -> e (Maybe (stepF a, pathF a))
  stepDown :: stepF a -> pathF a -> e (pathF a)

zipUp :: (Monad e, PreZipper e pathF stepF) =>
  (stepF other -> focus -> e focus) -> Zip pathF focus other -> e (Zip pathF focus other)
zipUp synth z@(Zip t p) =
  pathStep p >>= \case
    Just (s, p') -> synth s t >>= \a -> zipUp synth (Zip a p')
    Nothing -> return z

-- merge blur and key into focusKey, and
-- later have a (sharpFocus :: key -> focusKey) to extract content ?
class (Monad e, PreZipper e pathF stepF) =>
  Zipper e t node pathF stepF blur key |
      t -> node,
      node -> t pathF,
      pathF -> stepF blur key where
  zipperOf :: t -> e (Zip pathF node node)
  ofTopZipper :: Zip pathF node node -> e t
  stepUp :: stepF node -> node -> e node
  refocus :: blur -> key -> Zip pathF node node -> e (Zip pathF node node)

ofZipper :: Zipper e top node pathF stepF blur key => Zip pathF node node -> e top
ofZipper z = zipUp stepUp z >>= ofTopZipper



-- TODO make it work with Bits k as well as FiniteBits k?
-- h ::: a type for the height of keys, e.g. UInt8 for k==UInt256
-- k ::: a type for the keys of the Trie, e.g. UInt256
class (Num h, Integral h, {- Binary h, -}
       Num k, Integral k, {- Binary k, -} FiniteBits k) =>
  TrieKey h k where

-- TODO: add reference functor? synthesized attribute type on non-leaves?
-- e.g. ... Wrapable r, Binary a... => Trie r h k a c
-- c ::: a type for the content of the leaves of the Trie
data TrieKey h k =>
  Trie r h k c = TrieTop {
    trieHeight :: Int
      {- integerLength of the largest key in the Trie; -1 if empty -}
  , trieTopNode :: r (TrieNode r h k c)
      {- top node in the Trie -}
  }
  -- deriving (Show, Eq {-, Binary-})

data TrieKey h k =>
  TrieNode r h k c =
    Empty
  | Leaf c
  | Branch {left :: r (TrieNode r h k c), right :: r (TrieNode r h k c)}
  | Skip {heightMinus1 :: h, bits :: k, child :: r (TrieNode r h k c)}
  -- deriving (Show, Eq, Binary, Functor)

data TrieKey h k =>
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
  -- deriving (Show, Eq, Functor)

data TrieStep h k t
  = LeftStep t
  | RightStep t
  | SkipStep h k
  deriving (Show, Eq, Functor)

type TrieZip h k = Zip (TriePath h k)

type TrieZipper r h k c = TrieZip (r (TrieNode r h k c)) (r (TrieNode r h k c))

instance (Monad e, TrieKey h k) => PreZipper e (TriePath h k) (TrieStep h k) where
  pathStep (TriePath h k m s) = pure $
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
  stepDown step (TriePath h k m s) = pure $
    case step of
      LeftStep t -> TriePath (h + 1) (k `shiftL` 1) (m `shiftL` 1) (t : s)
      RightStep t -> TriePath (h + 1) ((k `shiftL` 1) .|. 1) (m `shiftL` 1) (t : s)
      SkipStep hd kd ->
        let ld = 1 + fromIntegral hd
            h1 = h - ld
            k1 = (k `shiftL` ld) .|. kd
            m1 = (m `shiftL` ld) .|. lowBitsMask ld in
        TriePath h1 k1 m1 s

instance (TrieKey h k, Wrapping r e) => Zipper e (Trie r h k c) (r (TrieNode r h k c)) (TriePath h k) (TrieStep h k) Int k where
  stepUp :: TrieStep h k (r (TrieNode r h k c)) -> r (TrieNode r h k c) -> e (r (TrieNode r h k c))
  stepUp step x =
    unwrap x >>=
    \case
      Empty -> case step of
        LeftStep r -> stepUp (SkipStep 0 1) r
        RightStep l -> stepUp (SkipStep 0 0) l
        SkipStep _ _ -> wrap Empty
      Skip hn kn cn ->
        case step of
          SkipStep hs ks ->
            wrap (Skip (hn + hs + 1) ((ks `shiftL` (1 + fromIntegral hn)) .|. kn) cn)
          _ -> up
      _ -> up
    where
      up = case step of
             LeftStep r -> wrap (Branch x r)
             RightStep l -> wrap (Branch l x)
             SkipStep h k -> wrap (Skip h k x)

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
          pathStep p >>=
          \case
            -- Can go up the original tree? Do.
            Just (s, p') -> stepUp s t >>= \ t' -> ascend t' p'
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
                  h1 = fromIntegral (l1 - 1) in
              do { t1 <- stepUp (SkipStep h1 0) t
                 ; e0 <- wrap Empty
                 ; descend e0 (TriePath (hcommon - 1) 1 0 [t1]) }

      -- descend: descend toward the sought focus from a pointed node above
      descend t p@(TriePath h _ _ _) =
        if h == h' then
          return (Zip t p)
        else
          unwrap t >>=
          \case
            -- base case: done
            Empty ->
              let l = h - h'
                  h1 = fromIntegral (l - 1)
                  k1 = k' .^. lowBitsMask l in
              do { e0 <- wrap Empty
                 ; p1 <- stepDown (SkipStep h1 k1) p
                 ; return (Zip e0 p1) }
            -- This case should never happen, being caught by h == h'
            Leaf _ -> return (Zip t p)
            -- recursive case
            Branch l r ->
              if testBit k' $ h - h' - 1 then
                continue r $ RightStep l
              else
                continue l $ LeftStep r
              where
                continue t' step = stepDown step p >>= descend t'
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
                do { t2 <- stepUp (SkipStep hlo blo) child
                   ; p2 <- stepDown (SkipStep hhi bhi) p
                   ; descend t2 p2 }
              else -- harder: keys differ in that bit range
                let sameLength = comparableLength - diffLength
                    branchNodeHeight = h - sameLength -- height right below which the keys differ
                    branchHeight = branchNodeHeight - 1 -- height of the two new branches
                    oldBranchLength = branchHeight - childHeight in
                do { oldBranch <- if oldBranchLength > 0 then
                                    let hh = fromIntegral (oldBranchLength - 1)
                                        bb = bits .^. lowBitsMask oldBranchLength in
                                    stepUp (SkipStep hh bb) child
                                  else
                                    return child
                   ; branchStep <- if testBit k' (branchNodeHeight - h' - 1) then
                                     return (RightStep oldBranch)
                                   else
                                     return (LeftStep oldBranch)
                   ; let skipSame = SkipStep (fromIntegral sameLength - 1)
                                      (bits `shiftR` (branchNodeHeight - childHeight))
                   ; e0 <- wrap Empty
                   ; stepDown skipSame p >>= stepDown branchStep >>= descend e0 }

  zipperOf (TrieTop h t) = return $ Zip t (TriePath h 0 0 [])

  ofTopZipper (Zip t p) = trieTop (triePathHeight p) t

trieTop :: (TrieKey h k, Wrapping r e) => Int -> r (TrieNode r h k c) -> e (Trie r h k c)
trieTop h x =
  unwrap x >>= \case
    Skip hh mm c ->
      let hhi = fromIntegral hh in
      if testBit mm hhi then
        return (TrieTop h x)
      else
        {- topiary: prune unnecessary zero key bits at the Trie top.
           Note that we don't do it in stepUp because that would break
           the zipper height invariant, especially so when aligning the top
           of the focus to match two tries. -}
        let l = integerLength mm
            hh' = fromIntegral (l - 1)
            h' = h + hhi - l in
        wrap (Skip hh' mm c) >>= pure . TrieTop h'
    _ -> pure $ TrieTop h x


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
getMerkleProof :: (TrieKey h k, Digesting d) => Trie r h k c -> k -> TriePath h k c
getMerkleProof trie key =
  zipperOf trie & refocus 0 k & zipPath & fmap getTrieDigest

getMerkleProof :: (TrieKey h k) => Trie r h k c -> k -> TriePath h k c
isMerkleProof trieDigest key value proof =
  trieDigest == zipUp digestSynth (Zip (digestSynth $ Leaf value) proof) &&
  key == triePathKey . zipPath $ proof
-}

update :: (TrieKey h k, Wrapping r e) => (Maybe v -> e (Maybe v)) -> k -> Trie r h k v -> e (Trie r h k v)
update updateLeaf key trie =
  zipperOf trie >>= refocus 0 key >>= \case
    Zip t path ->
      maybeOfLeaf t >>= updateLeaf >>= leafOfMaybe >>= \ t' -> ofZipper (Zip t' path)

-- The definition suggests we should swap v and k, so insert = update . const . Just
insert :: (TrieKey h k, Wrapping r e) => k -> v -> Trie r h k v -> e (Trie r h k v)
insert k v = update (const (return (Just v))) k

remove :: (TrieKey h k, Wrapping r e) => k -> Trie r h k v -> e (Trie r h k v)
remove = update (pure . const Nothing)

lookup :: (TrieKey h k, Wrapping r e) => Trie r h k a -> k -> e (Maybe a)
lookup t k = zipperOf t >>= refocus 0 k >>= \ (Zip t' _) -> maybeOfLeaf t'

leafOfMaybe :: PreWrapping r e => Maybe a -> e (r (TrieNode r h k a))
leafOfMaybe Nothing = wrap Empty
leafOfMaybe (Just v) = wrap (Leaf v)

maybeOfLeaf :: Wrapping r e => r (TrieNode r h k a) -> e (Maybe a)
maybeOfLeaf x = unwrap x >>= pure . \case
  Leaf v -> Just v
  _ -> Nothing -- only the Empty case should be used

singleton :: (TrieKey h k, Wrapping r e) => k -> a -> e (Trie r h k a)
singleton k v = empty >>= insert k v

empty :: (TrieKey h k, Wrapping r e) => e (Trie r h k a)
empty = wrap Empty >>= pure . TrieTop (-1)
