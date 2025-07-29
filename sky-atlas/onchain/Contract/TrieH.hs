module Contract.TrieH where

import Common.Crypto
import Common.Trie
-- import PlutusTx.Blueprint

import Common.Types
import Control.Monad (Monad)
import PlutusTx.Prelude

-- * Types

-- | TrieTopH has height then H(TrieNodeH)
type TrieTopH = TrieTop Blake2b_256

type TrieNodeH h k c = TrieNodeF h k c Blake2b_256

-- ** Zippers

type TrieZipperH h k = TrieZip h k Blake2b_256 Blake2b_256

-- * Typeclasses

class
  (Monad e, TrieHeightKey h k) =>
  TriePathing e h k
  where
  triePathStep :: TriePath h k a -> e (Maybe (TrieStep h k a, TriePath h k a))
  trieStepDown :: TrieStep h k a -> TriePath h k a -> e (TriePath h k a)

{-
class
  (Monad e, TriePathing e h k, Dato node, Dato (TriePath h k node)) =>
  TrieTopping e h k t node
  where
  trieZipperOf :: t -> e (TrieZip h k node node)
  ofTopTrieZipper :: TrieZip h k node node -> e t

class
  (Monad e, Dato node) =>
  TrieSteppingUp e h k node
  where
  trieStepUp :: TrieStep h k node -> node -> e node

class FocusableTrieZipping e h k node focusKey where
  refocusTrie :: focusKey -> TrieZip h k node node -> e (TrieZip h k node node)
-}

-- * Instances

-- ** Zippings

instance
  (Monad e, TrieHeightKey h k) =>
  TriePathing e h k
  where
  {-# INLINEABLE triePathStep #-}
  triePathStep (TriePath h k m s) =
    if isBitSet 0 m
      then
        let h1 = lowestBitClear m
            k1 = k `logicalAnd` lowBitsMask h1
            hr = h + h1
            kr = k `shiftRight` h1
            mr = m `shiftRight` h1
         in return . Just $ (SkipStep (fromInt $ h1 - 1) k1, TriePath hr kr mr s)
      else case s of
        t : sr ->
          let hr = h + 1
              kr = k `shiftRight` 1
              mr = m `shiftRight` 1
              branch = if isBitSet 0 k then RightStep else LeftStep
           in return . Just $ (branch t, TriePath hr kr mr sr)
        [] -> return Nothing

  -- stepDown from a TriePath
  {-# INLINEABLE trieStepDown #-}
  trieStepDown step p@(TriePath h k m s) =
    return
      $ let branch t k' = TriePath (h - 1) k' (m `shiftLeft` 1) (t : s)
         in case step of
              LeftStep t -> branch t $ k `shiftLeft` 1
              RightStep t -> branch t $ shiftLeftWithBits k 1 $ lowBitsMask 1
              SkipStep hd kd -> triePathSkipDown (1 + toInt hd) kd p

{-
instance
  (TrieHeightKey h k, Dato c) =>
  TrieTopping e h k (TrieNodeF h k c Hash) Hash
  where
  trieZipperOf (TrieTop h t) = return $ Zip t (TriePath h (lowBitsMask 0) (lowBitsMask 0) [])
  ofTopTrieZipper = ofTrieZipperFocus

instance
  (TrieHeightKey h k) =>
  TrieSteppingUp e h k Hash
  where
  trieStepUp step x =
    x >>= \case
      Empty -> case step of
        LeftStep r -> trieStepUp (SkipStep (fromInt 0) $ lowBitsMask 1) r
        RightStep l -> trieStepUp (SkipStep (fromInt 0) $ lowBitsMask 0) l
        SkipStep _ _ -> return x
      Skip hn kn cn ->
        case step of
          SkipStep hs ks ->
            Skip
              (fromInt $ toInt hn + toInt hs + 1)
              (shiftLeftWithBits ks (1 + toInt hn) kn)
              cn
          _ -> up
      _ -> up
    where
      up = case step of
        LeftStep r -> Branch x r
        RightStep l -> Branch l x
        SkipStep h k -> Skip h k x
-}

-- * Helpers

{-# INLINEABLE trieZipUp #-}
trieZipUp ::
  (Monad e, TriePathing e h k) =>
  (TrieStep h k background -> focus -> e focus) ->
  TrieZip h k focus background ->
  e (TrieZip h k focus background)
trieZipUp synth z@(Zip f p) =
  triePathStep p >>= \case
    Just (s, p') -> synth s f >>= \a -> trieZipUp synth (Zip a p')
    Nothing -> return z

applyTrieMerkleProof :: (TrieHeightKey h k, Monad e, IsHash d) => d -> TriePath h k d -> e d
applyTrieMerkleProof leafDigest proof =
  trieZipUp (\s h -> return $ digestTrieStep s h) (Zip leafDigest proof)
    >>= \(Zip d (TriePath hh _ _ _)) -> return . computeDigest . toByteString $ TrieTop hh d
