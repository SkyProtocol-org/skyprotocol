{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fobject-code #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:preserve-logging #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module Trie where

import SkyBase
import SkyCrypto

import PlutusTx.Prelude
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Builtins
import PlutusTx.Functor
import PlutusTx.Show
import PlutusTx.Utils

import Control.Composition ((-.))
import Control.Monad (Monad, (>=>))
import Data.Function ((&))
import Data.Functor.Identity (Identity (..))
import GHC.Generics (Generic)

-- * Types

-- | 'Trie' is the user-visible type of tries
-- 'r' is a 'Type->Type' wrapper, typically 'HashRef'
-- 'h' is the 'Type' of height-minus-1, typically 'Byte'
-- 'k' is the 'Type' of keys, e.g. 'Bytes8' or 'UInt256'
-- 'c' is the 'Type' of content, e.g. some 'MessageEntry'
type Trie r h k c = TrieTop (TrieNodeRef r h k c)

-- | TrieTop can be seen as a simplified TrieZip where k=0, m=0
-- Or should we just be using an actual TrieZip???
newtype TrieTop t = TrieTop_ (Integer, t)
  deriving Eq via (Integer, t)
pattern TrieTop :: Integer -> t -> TrieTop t
pattern TrieTop
  { trieTopHeight,
    {- integerLength of the largest key in the Trie; -1 if empty -}
    trieTopNode
    {- top node in the Trie -}
  } = TrieTop_ (trieTopHeight, trieTopNode)

-- | TrieNodeRef is the t used recursively in a Trie
-- Its content is a series of wrappings for type-directed code-generation purposes
type TrieNodeRef r h k c = LiftRef r (TrieNode r h k c)

type TrieNode r h k c = Fix (TrieNodeFL r h k c)

newtype TrieNodeFL r h k c t = TrieNodeFL {tfl :: TrieNodeF h k c (LiftRef r t)}
  deriving (Eq, FromByteString, ToByteString)
    via TrieNodeF h k c (LiftRef r t)

data TrieNodeF h k c t =
    Empty
  | Leaf c
  | Branch {branchLeft :: t, branchRight :: t}
  | Skip {skipHeightMinus1 :: h, skipBits :: k, skipChild :: t}

-- ** Zippers

-- | Abstract zippers
-- should we further require that pathF a = (focusKey, shape, [a]) ?
-- where focusKey = (height, key) for Trie's?
data Zip pathF focus background = Zip
  { zipFocus :: focus,
    -- ^ a pointed node within a data structure, or abstraction thereof
    zipPath :: pathF background
    -- ^ path from the focus node back to the top,
    --   with abstractions of other branches
  }

type TrieZipper r h k c = TrieZip h k (TrieNodeRef r h k c) (TrieNodeRef r h k c)

type TrieZip h k f b = Zip (TriePath h k) f b

data TrieStep h k o
  = LeftStep o -- ^ take a left step with that other thing on the right
  | RightStep o -- ^ take a right step with that other thing on the left
  | SkipStep h k -- ^ take a step over some index bits to skip

data
  (TrieHeightKey h k) =>
  TriePath h k d = TriePath
  { triePathHeight :: Integer,
    {- ^ the height of lowest key bit for the node *above* the focused node,
       which is 0 for a Leaf, 256 if the key is 256 bits with high bit set.
       Note that may thus not fit the type h of the height,
       e.g. because h==UInt8, k==UInt256 and 256 > 255 the max value in h. -}
    triePathKey :: k,
    {- ^ bits of key starting from triePathHeight of the pointed node.
         This node and descendants cover keys from k*2**h to k*2**h+2**h-1 included. -}
    _triePathSkipMask :: k,
    {- ^ bits of a mask indicating which of the bits of k1 are skipped. -}
    _triePathOthers :: [d]
    {- ^ the list s1 of data items from the *other* branches
       (no data for skip nodes), such that the first item corresponds
       to the first (low-order) bits of k1. -}
  }

type TrieProof h k hf = TriePath h k (DataDigest hf)

-- * Typeclasses

class
  (Monad e) =>
  PreZipper e pathF stepF
    | pathF -> stepF
  where
  pathStep :: pathF a -> e (Maybe (stepF a, pathF a))
  stepDown :: stepF a -> pathF a -> e (pathF a)

-- merge blur and key into focusKey, and
-- later have a (sharpFocus :: key -> focusKey) to extract content ?
class
  (Monad e, PreZipper e pathF stepF, Dato node, Dato (pathF node)) =>
  Zipper e t node pathF stepF
    | t -> node,
      node -> t pathF,
      pathF -> stepF
  where
  zipperOf :: t -> e (Zip pathF node node)
  ofTopZipper :: Zip pathF node node -> e t
  stepUp :: stepF node -> node -> e node

class
  FocusableZipper e node pathF focusKey where
  refocus :: focusKey -> Zip pathF node node -> e (Zip pathF node node)

class
  (Dato k, ToInt k, FromInt k, BitLogic k) =>
  TrieKey k where

class
  (Dato h, ToInt h, FromInt h) =>
  TrieHeight h where

class
  (TrieHeight h, TrieKey k) =>
  TrieHeightKey h k where

-- * Instances

-- ** TrieTop
-- for blockchain serialization purposes, we assume height will fit in a UInt16
--instance
--  (Eq t) =>
--  Eq (TrieTop t) where
--  (TrieTop h t) == (TrieTop h' t') = h == h' && t == t'
instance
  (ToByteString t) =>
  ToByteString (TrieTop t) where
  byteStringOut (TrieTop h t) = byteStringOut (toUInt16 $ h+1, t)
instance
  (FromByteString t) =>
  FromByteString (TrieTop t) where
  byteStringIn isTerminal = byteStringIn isTerminal <&> \ (hp1 :: UInt16, t) -> TrieTop (toInt hp1 - 1) t
instance
  (Dato t) =>
  Dato (TrieTop t) where
instance
  Functor TrieTop where
  fmap f (TrieTop h t) = TrieTop h (f t)
instance (Show t) => Show (TrieTop t) where
  showsPrec prec (TrieTop h t) = showApp prec "TrieTop" [showArg h, showArg t]

-- ** TrieNodeF
instance (TrieHeightKey h k, Eq c, Eq t) => Eq (TrieNodeF h k c t) where
  Empty == Empty = True
  Leaf c == Leaf c' = c == c'
  Branch l r == Branch l' r' = l == l' && r == r'
  Skip h k c == Skip h' k' c' = h == h' && k == k' && c == c'
  _ == _ = False
instance (TrieHeightKey h k, Show c, Show t) => Show (TrieNodeF h k c t) where
  show Empty = "Empty"
  show (Leaf c) = "Leaf " <> show c
  show (Branch lt rt) = "Branch " <> show lt <> " " <> show rt
  show (Skip h k ct) = "Skip " <> show h <> " " <> show k <> " " <> show ct
instance
  (TrieHeightKey h k, ToByteString c, ToByteString t) =>
  ToByteString (TrieNodeF h k c t) where
  byteStringOut Empty = byteStringOut (Byte 0)
  byteStringOut (Leaf c) = byteStringOut (Byte 1, c)
  byteStringOut (Branch l r) = byteStringOut (Byte 2, l, r)
  byteStringOut (Skip h k c) = byteStringOut (Byte 3, h, k, c)
instance
  (TrieHeightKey h k, Dato c, Dato t) =>
  Dato (TrieNodeF h k c t) where
instance
  (TrieHeightKey h k, FromByteString h, FromByteString k, FromByteString c, FromByteString t) =>
  FromByteString (TrieNodeF h k c t) where
  byteStringIn isTerminal = do
    (Byte tag) <- byteStringIn NonTerminal
    if tag == 0 then
      return Empty
    else if tag == 1 then
      byteStringIn isTerminal <&> Leaf
    else if tag == 2 then
      byteStringIn isTerminal <&> uncurry Branch
    else if tag == 3 then
      byteStringIn isTerminal <&> uncurry3 Skip
    else byteStringReaderFail
instance
  Functor (TrieNodeF h k c) where
  fmap f Empty = Empty
  fmap f (Leaf c) = Leaf c
  fmap f (Branch l r) = Branch (f l) (f r)
  fmap f (Skip h k t) = Skip h k (f t)

-- ** TrieNodeFL
--instance
--  (TrieHeightKey h k, Show c, LiftShow r, Show t) =>
--  Eq (TrieNodeFL r h k c t) where
--  TrieNodeFL x == TrieNodeFL y = x = y
instance
  (TrieHeightKey h k, Show c, LiftShow r, Show t) =>
  Show (TrieNodeFL r h k c t) where
  showsPrec prec (TrieNodeFL x) = showApp prec "TrieNodeFL" [showArg x]
instance
  (TrieHeightKey h k, Dato c, LiftDato r, Dato t) =>
  Dato (TrieNodeFL r h k c t) where
instance
  (TrieHeightKey h k, Eq c, LiftEq r) =>
  LiftEq (TrieNodeFL r h k c) where
  liftEq = (==)
instance
  (TrieHeightKey h k, Show c, LiftShow r) =>
  LiftShow (TrieNodeFL r h k c) where
  liftShowsPrec = showsPrec
instance
  (LiftToByteString r, TrieHeightKey h k, ToByteString c) =>
  LiftToByteString (TrieNodeFL r h k c) where
  liftToByteString = toByteString
  liftByteStringOut = byteStringOut
instance
  (TrieHeightKey h k, FromByteString h, FromByteString k, FromByteString c, LiftFromByteString r) =>
  LiftFromByteString (TrieNodeFL r h k c) where
  liftFromByteString = fromByteString
  liftByteStringIn = byteStringIn
instance
  (TrieHeightKey h k, Dato c, LiftDato r) =>
  LiftDato (TrieNodeFL r h k c) where
--instance
--  (TrieHeightKey h k, FromByteString h, FromByteString k, FromByteString c, LiftFromByteString r, FromByteString t) =>
--  FromByteString (TrieNodeFL r h k c t) where
--  byteStringIn = liftFromByteString
{- This would require refining Bifunctor into DatoBifunctor that requires Dato its two arguments:
instance
  (LiftWrapping Identity r, TrieHeightKey h k) =>
  DatoBifunctor (TrieNodeFL r h k) where
  bimap f g (TrieNodeFL t) =
    TrieNodeFL (let gg = LiftRef . runIdentity . liftWrap . g . runIdentity . liftUnwrap . liftref in
                  case t of
                    Empty -> Empty
                    Leaf c -> Leaf (f c)
                    Branch l r -> Branch (gg l) (gg r)
                    Skip h k t -> Skip h k (gg t))
-}

-- ** Zip
instance (Eq focus, Eq (pathF background)) => Eq (Zip pathF focus background) where
  Zip f1 p1 == Zip f2 p2 = f1 == f2 && p1 == p2
instance (Show focus, Show (pathF background)) => Show (Zip pathF focus background) where
  showsPrec prec (Zip focus path) = showApp prec "Zip" [showArg focus, showArg path]
instance (Functor pathF) => Functor (Zip pathF focus) where
  fmap f (Zip focus path) = Zip focus (fmap f path)

-- ** TriePath
instance
  (TrieHeightKey h k, Eq d) =>
  Eq (TriePath h k d) where
  TriePath h k m d == TriePath h' k' m' d' = h == h' && k == k' && m == m' && d == d'
instance
  (TrieHeightKey h k, Show d) =>
  Show (TriePath h k d) where
  showsPrec prec (TriePath h k m d) =
    showApp prec "TriePath" [showArg h, showArg k, showArg m, showArg d]
instance
  (TrieHeightKey h k, FromByteString h, FromByteString k, FromByteString d) =>
  FromByteString (TriePath h k d) where
  byteStringIn isTerminal = byteStringIn isTerminal <&> uncurry4 TriePath
instance
  (TrieHeightKey h k, ToByteString d) =>
  ToByteString (TriePath h k d) where
  byteStringOut (TriePath h k m ds) = byteStringOut (h, k, m, ds)
instance
  (TrieHeightKey h k, Dato d) =>
  Dato (TriePath h k d) where
instance
  (TrieHeightKey h k) =>
  Functor (TriePath h k) where
  fmap f (TriePath h k m ds) = TriePath h k m (fmap f ds)

-- ** TrieStep
instance
  (TrieHeightKey h k, Eq t) =>
  Eq (TrieStep h k t) where
  LeftStep o == LeftStep o' = o == o'
  RightStep o == RightStep o' = o == o'
  SkipStep h k == SkipStep h' k' = h == h && k == k'
  _ == _ = False
instance
  (TrieHeightKey h k, Show t) =>
  Show (TrieStep h k t) where
  showsPrec prec (LeftStep o) = showApp prec "LeftStep" [showArg o]
  showsPrec prec (RightStep o) = showApp prec "RightStep" [showArg o]
  showsPrec prec (SkipStep h k) = showApp prec "SkipStep" [showArg h, showArg k]
instance
  (TrieHeightKey h k, ToByteString t) =>
  ToByteString (TrieStep h k t) where
  byteStringOut (LeftStep t) = byteStringOut (Byte 0, t)
  byteStringOut (RightStep t) = byteStringOut (Byte 1, t)
  byteStringOut (SkipStep h k) = byteStringOut (Byte 2, h, k)
instance
  (FromByteString h, FromByteString k, FromByteString t) =>
  FromByteString (TrieStep h k t) where
  byteStringIn isTerminal = do
    (Byte tag) <- byteStringIn NonTerminal
    if tag == 0 then
      byteStringIn isTerminal <&> LeftStep
    else if tag == 1 then
      byteStringIn isTerminal <&> RightStep
    else if tag == 2 then
      byteStringIn isTerminal <&> uncurry SkipStep
    else byteStringReaderFail
instance
  (TrieHeightKey h k, Dato t) =>
  Dato (TrieStep h k t) where

-- ** Zippers
instance
  (Monad e, TrieHeightKey h k) =>
  PreZipper e (TriePath h k) (TrieStep h k)
  where
  {-# INLINEABLE pathStep #-}
  pathStep (TriePath h k m s) =
    return $
      if isBitSet 0 m
        then
          let h1 = lowestBitClear m
              k1 = k `logicalAnd` lowBitsMask h1
              hr = h + h1
              kr = k `shiftRight` h1
              mr = m `shiftRight` h1
           in Just (SkipStep (fromInt $ h1 - 1) k1, TriePath hr kr mr s)
        else case s of
          t : sr ->
            let hr = h + 1
                kr = k `shiftRight` 1
                mr = m `shiftRight` 1
                branch = if isBitSet 0 k then RightStep else LeftStep
             in Just (branch t, TriePath hr kr mr sr)
          [] -> Nothing

  -- stepDown from a TriePath
  {-# INLINEABLE stepDown #-}
  stepDown step p@(TriePath h k m s) = return $
    let branch t k' = TriePath (h - 1) k' (m `shiftLeft` 1) (t : s) in
    case step of
      LeftStep t -> branch t $ k `shiftLeft` 1
      RightStep t -> branch t $ shiftLeftWithBits k 1 $ lowBitsMask 1
      SkipStep hd kd -> triePathSkipDown (1 + toInt hd) kd p

instance
  (TrieHeightKey h k, LiftWrapping e r, LiftDato r, Dato c) =>
  Zipper e (Trie r h k c) (TrieNodeRef r h k c) (TriePath h k) (TrieStep h k) where
  zipperOf (TrieTop h t) = return $ Zip t (TriePath h (lowBitsMask 0) (lowBitsMask 0) [])
  ofTopZipper = ofTrieZipperFocus
  stepUp step x =
      fr x >>= \case
            Empty -> case step of
              LeftStep r -> stepUp (SkipStep (fromInt 0) $ lowBitsMask 1) r
              RightStep l -> stepUp (SkipStep (fromInt 0) $ lowBitsMask 0) l
              SkipStep _ _ -> return x
            Skip hn kn cn ->
              case step of
                SkipStep hs ks ->
                  rf $ Skip (fromInt $ toInt hn + toInt hs + 1)
                            (shiftLeftWithBits ks (1 + toInt hn) kn)
                            cn
                _ -> up
            _ -> up
      where
      up = case step of
        LeftStep r -> rf $ Branch x r
        RightStep l -> rf $ Branch l x
        SkipStep h k -> rf $ Skip h k x

instance
  (TrieHeightKey h k, LiftWrapping e r, LiftDato r, Dato c) =>
  FocusableZipper e (TrieNodeRef r h k c) (TriePath h k) (Integer, k) where
{- refocus the zipper toward the set of keys from k' `shiftLeft` h' included to (k' + 1) `shiftLeft` h' excluded -}
  refocus (h', k') z@(Zip node path@(TriePath h0 k0 _ _)) =
    --trace (show ("refocus", h', k', z)) $
    if h' == (-1) then -- focus from infinity
     --trace "foo"
     zipUp stepUp z >>= trieZipperFocusOnly
    else if h0 == (-1) then -- refocus an empty zipper
      let l = bitLength k'
          m = lowBitsMask l
          p = TriePath h' k' m [] :: TriePath h k (TrieNodeRef r h k c) in
      return $ Zip node p
    else
      ascend node path
      where
        {- hcommon: height up to which to ascend: no less than the desired height
           but also no less than necessary for there being a branch to our desired key
           and no less than necessary for there being a branch to the current key
           and yet no more than necessary. -}
        hcommon = h' `max` bitLength ((k0 `shiftLeft` h0) `logicalXor` (k' `shiftLeft` h'))
        {- Note that for very long keys, this bitwise-xor is already a log N operation,
           in which case maintaining an O(1) amortized cost would require us to
           take as parameter an incremental change on a zipper for the height
           and return an accordingly modified zipper for the Trie.
           In practice we use 256-bit keys for Cardano, which is borderline. -}

        {- ascend: go up the tree from old focus until h >= hcommon
           (then descend to new focus) -}
        ascend t p@(TriePath h _ _ _) =
          if h >= hcommon
            then descend t p
            else
              pathStep p
                >>= \case
                  -- Can go up the original tree? Do.
                  Just (s, p') -> stepUp s t >>= \t' -> ascend t' p'
                  -- Can't go up the original tree? Extend it!
                  {- At this point we're still below the desired level,
                     but there are no more steps to zip up in the original trie
                     so k is 0, h is the original trie height, and
                     hcommon is h' + bitLength k',
                     which means we have to create additional trie nodes
                     to accommodate space for the new key (k' `shiftLeft` h')
                     and take a RightStep from it. -}
                  Nothing ->
                    let l1 = hcommon - h - 1
                    in do
                         t1 <- trieNodeSkipUp l1 (lowBitsMask 0) t
                         e0 <- rf Empty
                         descend e0 (TriePath (hcommon - 1) (lowBitsMask 1) (lowBitsMask 0) [t1])
        -- descend: descend toward the sought focus from a pointed node above
        descend t p@(TriePath h _ _ _) =
          if h == h'
            then return (Zip t p)
            else
              fr t
                >>= \case
                  -- base case: done
                  Empty ->
                    let l = h - h'
                        k1 = k' `logicalAnd` lowBitsMask l
                        p1 = triePathSkipDown l k1 p
                     in do
                          e0 <- rf Empty
                          return (Zip e0 p1)
                  -- This case should never happen, being caught by h == h'
                  Leaf _ -> return (Zip t p)
                  -- recursive case
                  Branch l r ->
                    if isBitSet (h - h' - 1) k'
                      then continue r $ RightStep l
                      else continue l $ LeftStep r
                    where
                      continue t' step = stepDown step p >>= descend t'
                  -- hard case: descending common then uncommon parts of a Skip
                  Skip bitsHeightMinus1 bits child ->
                    let childHeight = h - (toInt bitsHeightMinus1) - 1
                        floorHeight = h' `max` childHeight
                        comparableLength = h - floorHeight
                        keyBits =
                          extractBitField
                            comparableLength
                            (floorHeight - h')
                            k'
                        nodeBits =
                          extractBitField
                            comparableLength
                            (floorHeight - childHeight)
                            bits
                        diffLength = bitLength (keyBits `logicalXor` nodeBits) in
                     if diffLength == 0
                     then -- Not so hard: if it was the same key all the way that matters
                       let llo = floorHeight - childHeight
                           blo = bits `logicalXor` lowBitsMask llo
                           bhi = bits `shiftRight` llo
                           p2 = triePathSkipDown comparableLength bhi p
                       in trieNodeSkipUp llo blo child >>= flip descend p2
                     else -- harder: keys differ in that bit range
                       let sameLength = comparableLength - diffLength
                           branchNodeHeight = h - sameLength -- height right below which the keys differ
                           sameBits = bits `shiftRight` (branchNodeHeight - childHeight)
                           branchHeight = branchNodeHeight - 1 -- height of the two new branches
                           oldBranchLength = branchHeight - childHeight
                           branchStep = if isBitSet (branchNodeHeight - h' - 1) k'
                             then RightStep else LeftStep
                       in do
                           oldBranch <-
                             if oldBranchLength > 0
                             then
                               let hh = fromInt (oldBranchLength - 1)
                                   bb = bits `logicalAnd` lowBitsMask oldBranchLength
                               in stepUp (SkipStep hh bb) child
                             else return child
                           e0 <- rf Empty
                           triePathSkipDown sameLength sameBits p &
                             stepDown (branchStep oldBranch) >>=
                             descend e0
instance
  (TrieHeightKey h k, LiftWrapping e r, LiftDato r, Dato c) =>
  FocusableZipper e (TrieNodeRef r h k c) (TriePath h k) k where
  refocus k z = refocus (0, k) z


-- * Helpers
{-# INLINEABLE zipUp #-}
zipUp ::
  (Monad e, PreZipper e pathF stepF) =>
  (stepF background -> focus -> e focus) ->
  Zip pathF focus background ->
  e (Zip pathF focus background)
zipUp synth z@(Zip f p) =
  pathStep p >>= \case
    Just (s, p') -> synth s f >>= \a -> zipUp synth (Zip a p')
    Nothing -> return z

ofZipper :: (Zipper e top node pathF stepF, Dato top, Dato (pathF node), Dato (stepF node)) => Zip pathF node node -> e top
ofZipper = zipUp stepUp >=> ofTopZipper

-- | Get a TrieNodeRef from a TrieNodeF
rf :: (LiftPreWrapping e r, TrieHeightKey h k, LiftDato r, Dato c) => TrieNodeF h k c (TrieNodeRef r h k c) -> e (TrieNodeRef r h k c)
rf = wrap . Fix . TrieNodeFL

-- | Get a TrieNodeF from a TrieNodeRef
fr :: (LiftWrapping e r, TrieHeightKey h k, LiftDato r, Dato c) => TrieNodeRef r h k c -> e (TrieNodeF h k c (TrieNodeRef r h k c))
fr w = do u <- unwrap w; case getFix u of TrieNodeFL x -> return x

triePathSkipDown :: (TrieHeightKey h k) => Integer -> k -> TriePath h k d -> TriePath h k d
triePathSkipDown sl sk p@(TriePath h k m d) =
  if sl == 0 then p else -- is this check an optimization or pessimization?
    let h' = h - sl
        k' = shiftLeftWithBits k sl sk
        m' = shiftLeftWithBits m sl $ lowBitsMask sl
    in TriePath h' k' m' d

trieNodeSkipUp :: (TrieHeightKey h k, LiftWrapping e r, LiftDato r, Dato c) => Integer -> k -> TrieNodeRef r h k c -> e (TrieNodeRef r h k c)
trieNodeSkipUp l k x =
  if l == 0 then return x else
    fr x >>= \case
      Empty -> return x
      Skip hn kn cn -> rf $ Skip (fromInt (l + toInt hn)) (shiftLeftWithBits k (1 + toInt hn) kn) cn
      _ -> rf $ Skip (fromInt $ l - 1) k x

-- Get a Trie out of the focus of a TrieZipper
ofTrieZipperFocus :: (TrieHeightKey h k, LiftWrapping e r, LiftDato r, Dato c) => TrieZipper r h k c -> e (Trie r h k c)
ofTrieZipperFocus = trieZipperFocusOnly >=> \case Zip f (TriePath h _ _ _) -> return $ TrieTop h f --)

-- Keep the focus, prune all branches above, zip up to the top
trieZipperFocusOnly :: (TrieHeightKey h k, LiftWrapping e r, LiftDato r, Dato c) => TrieZipper r h k c -> e (TrieZipper r h k c)
trieZipperFocusOnly (Zip f p@(TriePath h k _ _)) =
  let done h' f' = return $ Zip f' (TriePath h' (lowBitsMask 0) (lowBitsMask 0) []) in
    fr f >>= \case
      Empty -> done (-1) f
      Skip hh mm c -> stepDown (SkipStep hh mm) p >>= \ p' -> trieZipperFocusOnly (Zip c p')
      _ -> let l = bitLength k in trieNodeSkipUp l k f >>= done (h + l)

zipMapFocus :: (Monad e) => (node -> e node') -> Zip pathF node otherdata -> e (Zip pathF node' otherdata)
zipMapFocus f (Zip node path) = f node >>= return . flip Zip path

zipUpdate :: (TrieHeightKey h k, LiftWrapping e r, LiftDato r, Dato c) => (Maybe c -> e (Maybe c)) -> k -> TrieZipper r h k c -> e (TrieZipper r h k c)
zipUpdate updateLeaf k = refocus k >=> zipMapFocus (maybeOfLeaf >=> updateLeaf >=> leafOfMaybe)

-- NOTE: zipInsert v k, not zipInsert k v
zipInsert :: (TrieHeightKey h k, LiftWrapping e r, LiftDato r, Dato c) => c -> k -> TrieZipper r h k c -> e (TrieZipper r h k c)
zipInsert = zipUpdate . const . return . Just

zipRemove :: (TrieHeightKey h k, LiftWrapping e r, LiftDato r, Dato c) => k -> TrieZipper r h k c -> e (TrieZipper r h k c)
zipRemove = zipUpdate (return . const Nothing)

zipLookup :: (TrieHeightKey h k, LiftWrapping e r, Monad e, LiftDato r, Dato c) => k -> TrieZipper r h k c -> e (Maybe c)
zipLookup k = refocus k >=> return . zipFocus >=> maybeOfLeaf

zipInsertList :: (LiftDato r, TrieHeightKey h k, LiftWrapping e r, Dato c) => [(k, c)] -> TrieZipper r h k c -> e (TrieZipper r h k c)
zipInsertList ((k, v) : l) = zipInsertList l >=> zipInsert v k
zipInsertList [] = return

zipFocusLastLeaf :: (Monad e, TrieHeightKey h k, LiftWrapping e r, LiftDato r, Dato c) => TrieZipper r h k c -> e (TrieZipper r h k c)
zipFocusLastLeaf z = zipUp stepUp z >>= descendRight where
  descendRight z@(Zip node path) = fr node >>= \case
    Empty -> return z -- it was empty all along
    Leaf _ -> return z -- found it
    Branch left right -> stepDown (RightStep left) path >>= descendRight . Zip right
    Skip hn kn cn -> stepDown (SkipStep hn kn) path >>= descendRight . Zip cn

zipLastLeaf :: (TrieHeightKey h k, LiftWrapping e r, LiftDato r, Dato c) => TrieZipper r h k c -> e (Maybe (k, c))
zipLastLeaf = zipFocusLastLeaf >=> \ (Zip node (TriePath _ k _ _)) -> fr node >>= \case
  Leaf c -> return $ Just (k, c) -- found it
  _ -> return Nothing -- not found: should be Empty, since Branch and Skip are invalid.

-- TODO: bounds check on k and return a Maybe instead of erroring out
zipNextIndex :: (TrieHeightKey h k, LiftWrapping e r, LiftDato r, Dato c) => TrieZipper r h k c -> e k
zipNextIndex z = zipLastLeaf z >>= \case
  Nothing -> return . fromInt $ 0
  Just (k, _) -> return . fromInt $ 1 + toInt k

appendListOfZipper :: (TrieHeightKey h k, LiftWrapping e r, LiftDato r, Dato c) => TrieZipper r h k c -> [(k, c)] -> e [(k, c)]
appendListOfZipper (Zip (t :: TrieNodeRef r h k c) p@(TriePath _ k _ _)) a =
 --trace "aloz" $
  fr t >>= \case
    Empty -> return a
    Leaf v -> return ((k, v) : a)
    Branch l r -> do
      rp <- stepDown (RightStep l) p
      ra <- appendListOfZipper (Zip r rp) a
      lp <- stepDown (LeftStep r) p
      appendListOfZipper (Zip l lp) ra
    Skip h k' c -> do
      sp <- stepDown (SkipStep h k') p
      appendListOfZipper (Zip c sp) a

update :: (TrieHeightKey h k, LiftWrapping e r, LiftDato r, Dato c) => (Maybe c -> e (Maybe c)) -> k -> Trie r h k c -> e (Trie r h k c)
update updateLeaf key = zipperOf >=> zipUpdate updateLeaf key >=> ofZipper

-- NOTE: zipInsert v k, not zipInsert k v
-- The definition suggests we should swap v and k, so insert = update . const . Just
insert :: (TrieHeightKey h k, LiftWrapping e r, LiftDato r, Dato c) => c -> k -> Trie r h k c -> e (Trie r h k c)
insert v k = update (const (return (Just v))) k

remove :: (TrieHeightKey h k, LiftWrapping e r, LiftDato r, Dato c) => k -> Trie r h k c -> e (Trie r h k c)
remove = update (return . const Nothing)

lookup :: (TrieHeightKey h k, LiftWrapping e r, LiftDato r, Dato c) => k -> Trie r h k c -> e (Maybe c)
lookup k = zipperOf >=> zipLookup k

-- TODO: bounds check on k and return a Maybe instead of erroring out
nextIndex :: (TrieHeightKey h k, LiftWrapping e r, LiftDato r, Dato c) => Trie r h k c -> e k
nextIndex = zipperOf >=> zipNextIndex

leafOfMaybe :: (LiftPreWrapping e r, TrieHeightKey h k, LiftDato r, Dato c) => Maybe c -> e (TrieNodeRef r h k c)
leafOfMaybe Nothing = rf Empty
leafOfMaybe (Just v) = rf (Leaf v)

maybeOfLeaf :: (LiftWrapping e r, TrieHeightKey h k, LiftDato r, Dato c) => TrieNodeRef r h k c -> e (Maybe c)
maybeOfLeaf x =
  fr x
    >>= return . \case
      Leaf v -> Just v
      _ -> Nothing -- only the Empty case should be used

singleton :: (TrieHeightKey h k, LiftWrapping e r, LiftDato r, Dato c) => k -> c -> e (Trie r h k c)
singleton k v = empty >>= insert v k

empty :: (TrieHeightKey h k, LiftWrapping e r, LiftDato r, Dato c) => e (Trie r h k c)
empty = rf Empty >>= return . TrieTop (-1)

ofList :: (LiftDato r, Dato h, Dato k, Dato c, TrieHeightKey h k, LiftWrapping e r) => [(k, c)] -> e (Trie r h k c)
ofList bindings = empty >>= zipperOf >>= zipInsertList bindings >>= ofZipper

listOf :: (TrieHeightKey h k, LiftWrapping e r, LiftDato r, Dato c) => Trie r h k c -> e [(k, c)]
listOf = zipperOf >=> flip appendListOfZipper []

-- TODO: have merkle proofs of Non-Inclusion, by showing the last not before Empty.
getMerkleProof ::
  (TrieHeightKey h k, LiftWrapping e r, DigestibleRef hf r, LiftDato r, Dato c) =>
  k ->
  Trie r h k c ->
  e (TrieProof h k hf)
getMerkleProof k t = zipperOf t >>= refocus k >>= zipPath -. fmap (liftref -. getDigest -. castDigest) -. return

applyTrieStep :: (TrieHeightKey h k) => TrieStep h k t -> t -> TrieNodeF h k () t
applyTrieStep s t = case s of
  LeftStep r -> Branch t r
  RightStep l -> Branch l t
  SkipStep h k -> Skip h k t

{-# INLINEABLE digestTrieStep #-}
digestTrieStep ::
  (TrieHeightKey h k, HashFunction hf) =>
  TrieStep h k (DataDigest hf) ->
  DataDigest hf ->
  DataDigest hf
digestTrieStep s h = computeDigest . toByteString $ applyTrieStep s h

applyMerkleProof :: (TrieHeightKey h k, HashFunction hf, Monad e) => DataDigest hf -> TrieProof h k hf -> e (DataDigest hf)
applyMerkleProof leafDigest proof =
  zipUp (\s h -> return $ digestTrieStep s h) (Zip leafDigest proof) >>=
  \ (Zip d (TriePath hh _ _ _)) -> return . computeDigest . toByteString $ TrieTop hh d
