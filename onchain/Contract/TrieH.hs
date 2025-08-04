module Contract.TrieH where

import Common.Crypto
import Common.Trie
-- import PlutusTx.Blueprint

import Common.Types
import Control.Monad (Monad)
import PlutusTx.Prelude
import GHC.ByteOrder (ByteOrder (..))

-- * Types

-- | TrieTopH has height then H(TrieNodeH)
type TrieTopH = TrieTop Hash

type TrieNodeH h k c = TrieNodeF h k c Hash

-- ** Zippers

type TrieZipperH h k = TrieZip h k Hash Hash

-- * Typeclasses

class
  (Monad e, TrieHeightKey h k) =>
  TriePathing e h k
  where
  triePathStep :: TriePath h k a -> e (Maybe (TrieStep h k a, TriePath h k a))
  trieStepDown :: TrieStep h k a -> TriePath h k a -> e (TriePath h k a)

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

applyTrieMerkleProof :: (TrieHeightKey h k, Monad e) => Hash -> TriePath h k Hash -> e Hash
applyTrieMerkleProof leafDigest proof =
  trieZipUp (\s h -> return $ digestTrieStep s h) (Zip leafDigest proof)
    >>= \(Zip d (TriePath hh _ _ _)) -> return . hashFunction . trieTopToByteString hashToByteString $ TrieTop hh d

{-# INLINEABLE pairToByteString #-}
pairToByteString :: (a -> BuiltinByteString) -> (b -> BuiltinByteString) -> (a, b) -> BuiltinByteString
pairToByteString fstTBS sndTBS (a, b) = appendByteString (fstTBS a) (sndTBS b)

{-# INLINEABLE trieTopToByteString #-}
trieTopToByteString :: (t -> BuiltinByteString) -> TrieTop t -> BuiltinByteString
trieTopToByteString tbs (TrieTop h t) =
  pairToByteString uint16ToByteString tbs (toUInt16 $ h + 1, t)

{-# INLINEABLE uint16ToByteString #-}
uint16ToByteString :: UInt16 -> BuiltinByteString
uint16ToByteString (UInt16 n) = integerToByteString BigEndian 2 n

{-# INLINEABLE hashToByteString #-}
hashToByteString :: Blake2b_256 -> BuiltinByteString
hashToByteString (Blake2b_256 (Bytes32 b)) = b
