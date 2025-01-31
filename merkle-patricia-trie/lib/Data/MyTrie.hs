-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.MyTrie (TrieNodeRef, TrieNode, TrieNodeF (..), Trie, TrieKey, TriePath, TrieZipper, TrieProof, refocus, get, update, insert, remove, singleton, lookup, empty, ofZipper, zipperOf, zipInsert, zipRemove, zipInsertList, appendListOfZipper, ofList, listOf, {-getMerkleProof,-} {-isMerkleProof-}) where

import Prelude hiding (lookup)

import Data.Internal.RecursionSchemes
import Data.Utils

--import Control.Arrow
import Control.Monad
import Crypto.Hash
import Data.Binary (Binary, Get, put, get) -- Put, encode, decode
import Data.Bits
-- import Data.Function ((&))
import Data.Function.Contravariant.Syntax
import Data.Functor
import Data.Kind (Type)
import Data.WideWord (Word256)
import Data.Word (Word8)
-- import GHC.Generics (Generic)

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
  deriving (Show, Eq, Functor)

class Monad e =>
  PreZipper e pathF stepF | pathF -> stepF where
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


-- TODO make it work with Bits k as well as Bits k?
-- h ::: a type for the height of keys, e.g. UInt8 for k==UInt256
-- k ::: a type for the keys of the Trie, e.g. UInt256
class (Num k, Integral k, Bits k, Eq k, Show k, Binary k) =>
  TrieKey k where
    lowestKeyBitClear :: k -> Int
    lowestKeyBitClear m = lowestBitClear m

class (Num h, Integral h, Eq h, Show h, Binary h) =>
  TrieHeight h where

class (TrieHeight h, TrieKey k) =>
  TrieHeightKey h k where

-- TODO: add reference functor? synthesized attribute type on non-leaves?
-- e.g. ... Wrapable r, Binary a... => Trie r h k a c
-- c ::: a type for the content of the leaves of the Trie
data TrieTop t = TrieTop {
    _trieHeight :: Int
      {- integerLength of the largest key in the Trie; -1 if empty -}
  , _trieTopNode :: t
      {- top node in the Trie -}
  } deriving (Show, Eq)

instance Binary t =>
  Binary (TrieTop t) where
  put (TrieTop h t) = (put h) >> (put t)
  get = do h <- get ; t <- get ; return (TrieTop h t)

type Trie r h k c = TrieTop (TrieNodeRef r h k c)

data TrieNodeF h k c t =
    Empty
  | Leaf c
  | Branch {left :: t, right :: t}
  | Skip {heightMinus1 :: h, bits :: k, child :: t}
  deriving (Eq, Show, Functor)

instance (TrieHeightKey h k, Binary h, Binary k, Binary c, Binary t) =>
  Binary (TrieNodeF h k c t) where
  put Empty = put (0 :: Word8)
  put (Leaf c) = put (1 :: Word8) >> put c
  put (Branch l r) = put (2 :: Word8) >> put l >> put r
  put (Skip h k c) = put (3 :: Word8) >> put h >> put k >> put c
  get = do
          v <- (get :: Get Word8)
          case v of
            0 -> return $ Empty
            1 -> Leaf <$> get
            2 -> do l <- get
                    r <- get
                    return $ Branch l r
            3 -> do h <- get
                    k <- get
                    d <- get
                    return $ Skip h k d
            _ -> error "bad input"

newtype TrieNodeFL r h k c t = TrieNodeFL { trienodefl :: TrieNodeF h k c (Lift r t) }
  deriving (Eq, Show)

instance (Show h, Show k, Show c, LiftShow r) =>
  LiftShow (TrieNodeFL r h k c) where
  liftShow = show

instance (Eq h, Eq k, Eq c, LiftEq r) =>
  LiftEq (TrieNodeFL r h k c) where
  liftEq = (==)

instance (TrieHeightKey h k, Binary h, Binary k, Binary c, LiftBinary r) =>
  LiftBinary (TrieNodeFL r h k c) where
  liftGet = (get :: Binary t => Get (TrieNodeF h k c (Lift r t))) >>= pure . TrieNodeFL
  liftPut = put . trienodefl

type TrieNode r h k c = Fix (TrieNodeFL r h k c)

-- Get a reference from a TrieNodeF
rf :: PreWrapping (TrieNode r h k c) (Lift r) e => TrieNodeF h k c (TrieNodeRef r h k c) -> e (TrieNodeRef r h k c)
rf = wrap . In . TrieNodeFL

-- Get a TrieNodeF from a reference
fr :: Wrapping (TrieNode r h k c) (Lift r) e => TrieNodeRef r h k c -> e (TrieNodeF h k c (TrieNodeRef r h k c))
fr w = do u <- unwrap w ; case out u of TrieNodeFL x -> return x

--instance (TrieHeightKey h k, LiftShow r, Show h, Show k, Show c) =>
--  Show (TrieNode r h k c) where
--  show Empty = "Empty"
--  show (Leaf c) = "(Leaf " ++ (show c) ++ ")"
--  show (Branch l r) = "(Branch " ++ (liftShow l) ++ " " ++ (liftShow r) ++ ")"
--  show (Skip h k c) = "(Branch " ++ (show h) ++ " " ++ (show k) ++ (liftShow c) ++ ")"

type TrieNodeRef r h k c = Lift r (TrieNode r h k c)

-- instance (LiftShow r, Show h, Show k, Show c) =>
--   Show (TrieNodeRef r h k c) where
--   show = liftShow

data TrieHeightKey h k =>
  TriePath h k d = TriePath {
    triePathHeight :: Int
      {- the height of lowest key bit for the node *above* the focused node,
         which is 0 for a Leaf, 256 if the key is 256 bits with high bit set.
         Note that may thus not fit the type h of the height,
         e.g. because h==UInt8, k==UInt256 and 256 > 255 the max value in h. -}
  , triePathKey :: k
      {- bits of key starting from triePathHeight (:: Int) of the pointed node. -}
  , _triePathSkipMask :: k
      {- bits of a mask indicating which of the bits of k1 are skipped. -}
  , _triePathOthers :: [d]
      {- the list s1 of data items from the *other* branches
         (no data for skip nodes), such that the first item corresponds
         to the first (low-order) bits of k1. -}
  }
  deriving (Eq, Show, Functor)

--instance (Show h, Show k, Show d) =>
--  Show (TriePath h k d) where
--  show (TriePath h k m l) = "(TriePath " ++ (show h) ++ " " ++ (show k) ++ " " ++ (show m) ++
--                            " " ++ (show l) ++ ")"

data TrieStep h k t
  = LeftStep t
  | RightStep t
  | SkipStep h k
  deriving (Show, Eq, Functor)

type TrieZip h k = Zip (TriePath h k) :: Type -> Type -> Type

type TrieZipper r h k c = TrieZip h k (TrieNodeRef r h k c) (TrieNodeRef r h k c) :: Type

instance (Monad e, TrieHeightKey h k) =>
  PreZipper e (TriePath h k) (TrieStep h k) where
  pathStep (TriePath h k m s) = pure $
    if testBit m 0 then
      let h1 = lowestKeyBitClear m
          k1 = k .&. lowBitsMask h1
          hr = h + h1
          kr = k `shiftR` h1
          mr = m `shiftR` h1 in
        Just (SkipStep (fromIntegral $ h1-1) k1, TriePath hr kr mr s)
    else case s of
      t : sr -> let hr = h - 1
                    kr = k `shiftR` 1
                    mr = m `shiftR` 1
                    branch = if testBit m 0 then RightStep else LeftStep in
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

instance (TrieHeightKey h k, Wrapping (TrieNode r h k c) (Lift r) e) => Zipper e (Trie r h k c) (TrieNodeRef r h k c) (TriePath h k) (TrieStep h k) Int k where
  stepUp :: TrieStep h k (TrieNodeRef r h k c) -> TrieNodeRef r h k c -> e (TrieNodeRef r h k c)
  stepUp step x =
    fr x >>=
    \case
      Empty -> case step of
        LeftStep r -> stepUp (SkipStep 0 1) r
        RightStep l -> stepUp (SkipStep 0 0) l
        SkipStep _ _ -> rf $ Empty
      Skip hn kn cn ->
        case step of
          SkipStep hs ks ->
            rf $ Skip (hn + hs + 1) ((ks `shiftL` (1 + fromIntegral hn)) .|. kn) cn
          _ -> up
      _ -> up
    where
      up = case step of
             LeftStep r -> rf $ Branch x r
             RightStep l -> rf $ Branch l x
             SkipStep h k -> rf $ Skip h k x

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
                 ; e0 <- rf Empty
                 ; descend e0 (TriePath (hcommon - 1) 1 0 [t1]) }

      -- descend: descend toward the sought focus from a pointed node above
      descend t p@(TriePath h _ _ _) =
        if h == h' then
          return (Zip t p)
        else
          fr t >>=
          \case
            -- base case: done
            Empty ->
              let l = h - h'
                  h1 = fromIntegral (l - 1)
                  k1 = k' .^. lowBitsMask l in
              do { e0 <- rf Empty
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
                   ; e0 <- rf Empty
                   ; stepDown skipSame p >>= stepDown branchStep >>= descend e0 }

  zipperOf (TrieTop h t) = return $ Zip t (TriePath h 0 0 [])

  ofTopZipper (Zip t p) = trieTop (triePathHeight p) t

trieTop :: (TrieHeightKey h k, Wrapping (TrieNode r h k c) (Lift r) e) => Int -> TrieNodeRef r h k c -> e (Trie r h k c)
trieTop h x =
  fr x >>= \case
    Empty -> return (TrieTop (-1) x)
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
        rf (Skip hh' mm c) >>= return . TrieTop h'
    _ -> return (TrieTop h x)


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

type TrieProof h k ha = TriePath h k (Digest ha)

zipMapFocus :: Monad e => (node -> e node') -> Zip pathF node otherdata -> e (Zip pathF node' otherdata)
zipMapFocus f (Zip node path) = f node >>= pure . flip Zip path

zipUpdate :: (TrieHeightKey h k, Wrapping (TrieNode r h k c) (Lift r) e) => (Maybe c -> e (Maybe c)) -> k -> TrieZipper r h k c -> e (TrieZipper r h k c)
zipUpdate updateLeaf k z =
  refocus 0 k z >>= zipMapFocus (maybeOfLeaf >=> updateLeaf >=> leafOfMaybe)

-- NOTE: zipInsert v k, not zipInsert k v
zipInsert :: (TrieHeightKey h k, Wrapping (TrieNode r h k c) (Lift r) e) => c -> k -> TrieZipper r h k c -> e (TrieZipper r h k c)
zipInsert = zipUpdate . const . return . Just

zipRemove :: (TrieHeightKey h k, Wrapping (TrieNode r h k c) (Lift r) e) => k -> TrieZipper r h k c -> e (TrieZipper r h k c)
zipRemove = zipUpdate (pure . const Nothing)

zipLookup :: (TrieHeightKey h k, Wrapping (TrieNode r h k c) (Lift r) e, Monad e) => TrieZipper r h k c -> k -> e (Maybe c)
zipLookup z k = refocus 0 k z >>= pure . zipFocus >>= maybeOfLeaf

zipInsertList :: (Show c, LiftShow r, TrieHeightKey h k, Wrapping (TrieNode r h k c) (Lift r) e) => [(k,c)] -> TrieZipper r h k c -> e (TrieZipper r h k c)
zipInsertList ((k, v):l) = zipInsertList l >=> zipInsert v k
zipInsertList [] = \ t -> return t

appendListOfZipper :: (TrieHeightKey h k, Wrapping (TrieNode r h k c) (Lift r) e) => TrieZipper r h k c -> [(k,c)] -> e [(k, c)]
appendListOfZipper (Zip (t :: TrieNodeRef r h k c) p@(TriePath _ k _ _)) a =
  fr t >>= \case
  Empty -> return a
  Leaf v -> return ((k, v): a)
  Branch l r -> do
                  rp <- stepDown (RightStep l) p
                  ra <- appendListOfZipper (Zip r rp) a
                  lp <- stepDown (LeftStep r) p
                  appendListOfZipper (Zip l lp) ra
  Skip h k' c -> do
                  sp <- stepDown (SkipStep h k') p
                  appendListOfZipper (Zip c sp) a

update :: (TrieHeightKey h k, Wrapping (TrieNode r h k c) (Lift r) e) => (Maybe c -> e (Maybe c)) -> k -> Trie r h k c -> e (Trie r h k c)
update updateLeaf key = zipperOf >=> zipUpdate updateLeaf key >=> ofZipper

-- NOTE: zipInsert v k, not zipInsert k v
-- The definition suggests we should swap v and k, so insert = update . const . Just
insert :: (TrieHeightKey h k, Wrapping (TrieNode r h k c) (Lift r) e) => c -> k -> Trie r h k c -> e (Trie r h k c)
insert v k = update (const (return (Just v))) k

remove :: (TrieHeightKey h k, Wrapping (TrieNode r h k c) (Lift r) e) => k -> Trie r h k c -> e (Trie r h k c)
remove = update (pure . const Nothing)

lookup :: (TrieHeightKey h k, Wrapping (TrieNode r h k c) (Lift r) e) => Trie r h k c -> k -> e (Maybe c)
lookup t k = zipperOf t >>= flip zipLookup k

leafOfMaybe :: PreWrapping (TrieNode r h k c) (Lift r) e => Maybe c -> e (TrieNodeRef r h k c)
leafOfMaybe Nothing = rf Empty
leafOfMaybe (Just v) = rf (Leaf v)

maybeOfLeaf :: Wrapping (TrieNode r h k c) (Lift r) e => TrieNodeRef r h k c -> e (Maybe c)
maybeOfLeaf x = fr x >>= pure . \case
  Leaf v -> Just v
  _ -> Nothing -- only the Empty case should be used

singleton :: (TrieHeightKey h k, Wrapping (TrieNode r h k c) (Lift r) e) => k -> c -> e (Trie r h k c)
singleton k v = empty >>= insert v k

empty :: (TrieHeightKey h k, Wrapping (TrieNode r h k c) (Lift r) e) => e (Trie r h k c)
empty = rf Empty >>= pure . TrieTop (-1)

ofList :: (LiftShow r, Show h, Show k, Show c, TrieHeightKey h k, Wrapping (TrieNode r h k c) (Lift r) e) => [(k, c)] -> e (Trie r h k c)
ofList bindings = empty >>= zipperOf >>= \z -> zipInsertList bindings z >>= ofZipper

listOf :: (TrieHeightKey h k, Wrapping (TrieNode r h k c) (Lift r) e) => Trie r h k c -> e [(k, c)]
listOf = zipperOf >=> flip appendListOfZipper []

instance TrieKey Word256 where
  lowestKeyBitClear = fbLowestBitClear
instance TrieHeight Word8 where

instance TrieHeightKey Word8 Word256 where

getMerkleProof :: (TrieHeightKey h k, Wrapping (TrieNode r h k c) (Lift r) e, Digestible r) =>
  Trie r h k c -> k -> e (TrieProof h k (HashAlgorithmOf r))
getMerkleProof t k =
  zipperOf t >>= refocus 0 k <&> zipPath -. fmap (getDigest . lifted)

applyTrieStep :: TrieHeightKey h k => TrieStep h k t -> t -> TrieNodeF h k () t
applyTrieStep s t = case s of
  LeftStep r -> Branch t r
  RightStep l -> Branch l t
  SkipStep h k -> Skip h k t

digestTrieStep :: (TrieHeightKey h k, HashAlgorithm ha, Binary (Digest ha)) =>
  TrieStep h k (Digest ha) -> Digest ha -> Digest ha
digestTrieStep s h = computeDigest $ applyTrieStep s h

isMerkleProof :: (TrieHeightKey h k, HashAlgorithm ha, Binary (Digest ha), Monad e) => k -> Digest ha -> Digest ha -> TrieProof h k ha -> e Bool
isMerkleProof key leafDigest topDigest proof =
  do
    synthTop <- zipUp (\ s h -> return $ digestTrieStep s h) (Zip leafDigest proof) <&>
      \case Zip d (TriePath hh _ _ _) -> computeDigest $ TrieTop hh d
    return (synthTop == topDigest && key == triePathKey proof)
