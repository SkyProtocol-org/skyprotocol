{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.MerkleTrie where

-- import Crypto.Hash (Digest, SHA256, hash)
-- import Data.ByteString.Char8 (pack)
-- import Data.Trie

-- computeHash :: (Show a) => a -> Digest SHA256
-- computeHash = hash . pack . show

-- -- | Compute the hash of a Patricia Trie
-- -- | For internal use only, since it's partially matched against the trie!
-- -- TODO don't like the fact, that this is an incomplete pattern, although I can wrap this in Maybe
-- -- need to think about this
-- computeNodeHash :: (Show k, Show (TrieHeight k), Show a) => Trie k a -> Digest SHA256
-- computeNodeHash Leaf {..} = computeHash (key, value)
-- computeNodeHash Branch {..} = computeHash (computeNodeHash left, computeNodeHash right)

-- type MerkleProof = (Digest SHA256, [Digest SHA256])

-- prove :: (Show a, Show k, Show (TrieHeight k), TrieKey k) => k -> Trie k a -> Maybe MerkleProof
-- prove k t = do
--   z <- focus k t
--   startingHash <- computeHash <$> getKeyValue z -- compute hash for the leaf
--   let accum = (startingHash, []) -- initial accumulator
--       zUp = goUp z -- now we're at the bottom-most branch
--   pure . fst $ upmostWith go accum zUp
--   where
--     -- TODO I don't like the fact, that there are 2 cases with undefined here, although logically I know, that they will never show up
--     -- compiler doesn't know that, and I'm not sure how to ensure everything is okay without some ugliness
--     -- although I can just wrap this whole acc in Maybe and deal with it. Anyway, need more thoughts
--     go :: (Show a, Show k, Show (TrieHeight k), TrieKey k) => MerkleProof -> Trie k a -> MerkleProof
--     go _ Empty = undefined -- Can't reach here, since 'focus' ensures that we're working with something(or it'll short cirtcuit and return Nothing)
--     go _ Leaf {} = undefined -- Can't reach here, since we start from the bottom-most branch
--     go (topHash, proof) b@Branch {} = let h = computeNodeHash b in (computeHash (topHash, h), h : proof)

-- -- | TODO do this
-- validate :: (TrieKey k) => Trie k a -> MerkleProof -> Bool
-- validate = undefined
