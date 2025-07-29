{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Contract.DaH where

import Common.Crypto
import Common.DA
import Common.Trie
import Common.Types
import Control.Monad (Monad)
import Data.Functor.Identity (Identity (..))
import PlutusTx
import PlutusTx.Functor as PlutusTx
import PlutusTx.Prelude as PlutusTx
import PlutusTx.Show as PlutusTx

-- import Prelude qualified as HP

------------------------------------------------------------------------------
-- Core Data Types
------------------------------------------------------------------------------

-- * Types

newtype SkyDaH = SkyDaOfTupleH {tupleOfSkyDaH :: (Blake2b_256, Blake2b_256)} -- H(DaMetaData), H(TopicTrie)
  deriving newtype (PlutusTx.Eq, PlutusTx.Show, ToByteString, FromByteString, ToData, FromData, UnsafeFromData)

-- deriving (HP.Eq, HP.Show)

pattern SkyDaH :: Blake2b_256 -> Blake2b_256 -> SkyDaH
pattern SkyDaH {skyMetaDataH, skyTopicTrieH} = SkyDaOfTupleH (skyMetaDataH, skyTopicTrieH)

{-# COMPLETE SkyDaH #-}

newtype DaMetaDataH = DaMetaDataOfTupleH {tupleOfDaMetaDataH :: (Blake2b_256, Blake2b_256)}
  deriving newtype (PlutusTx.Eq, PlutusTx.Show, ToByteString, FromByteString, ToData, FromData, UnsafeFromData)

-- deriving (HP.Eq, HP.Show)

pattern DaMetaDataH :: Blake2b_256 -> Blake2b_256 -> DaMetaDataH -- schema, committee
pattern DaMetaDataH {daSchemaH, daCommitteeH} = DaMetaDataOfTupleH (daSchemaH, daCommitteeH)

{-# COMPLETE DaMetaDataH #-}

type TopicEntryH = (Blake2b_256, Blake2b_256) -- H(TopicMetaDataH), H(MessageTrieH)

newtype TopicMetaDataH
  = TopicMetaDataOfTupleH {tupleOfTopicMetaDataH :: (Blake2b_256, Blake2b_256)}
  deriving newtype (PlutusTx.Eq, PlutusTx.Show, ToByteString, FromByteString, ToData, FromData, UnsafeFromData)

pattern TopicMetaDataH :: Blake2b_256 -> Blake2b_256 -> TopicMetaDataH
pattern TopicMetaDataH {topicSchemaH, topicCommitteeH} =
  TopicMetaDataOfTupleH (topicSchemaH, topicCommitteeH)

{-# COMPLETE TopicMetaDataH #-}

type TrieTopicPathH t = TriePath Byte TopicId t

type TrieMessagePathH t = TriePath Byte MessageId t

newtype SkyDataProofH = SkyDataProofOfTupleH
  {tupleOfSkyDataProofH :: (Blake2b_256, TrieTopicPathH Blake2b_256, Blake2b_256, TrieMessagePathH Blake2b_256, Blake2b_256)}
  deriving newtype (PlutusTx.Eq, PlutusTx.Show, ToByteString, FromByteString, ToData, FromData, UnsafeFromData)

-- deriving (HP.Eq, HP.Show)

pattern SkyDataProofH :: Blake2b_256 -> TrieTopicPathH Blake2b_256 -> Blake2b_256 -> TrieMessagePathH Blake2b_256 -> Blake2b_256 -> SkyDataProofH
pattern SkyDataProofH {proofDaMetaDataH, proofTopicTriePathH, proofTopicMetaDataH, proofMessageTriePathH, proofMessageMetaDataH} = SkyDataProofOfTupleH (proofDaMetaDataH, proofTopicTriePathH, proofTopicMetaDataH, proofMessageTriePathH, proofMessageMetaDataH)

{-# COMPLETE SkyDataProofH #-}

-- * Instances

-- * Helpers

-- TODO: In the future, also support proof of non-inclusion.
{-# INLINEABLE applySkyDataProofH #-}
applySkyDataProofH :: SkyDataProofH -> Blake2b_256 -> Blake2b_256
applySkyDataProofH SkyDataProofH {..} messageDataHash =
  runIdentity $ do
    let messageLeaf = Leaf (proofMessageMetaDataH, messageDataHash) :: TrieNodeF Byte Bytes8 _ ()
    let messageLeafHash = computeDigest messageLeaf
    topicDataHash <- applyMerkleProof messageLeafHash proofMessageTriePathH
    let topicLeaf = Leaf (proofTopicMetaDataH, topicDataHash) :: TrieNodeF Byte Bytes8 _ ()
    let topicLeafHash = computeDigest topicLeaf
    daDataHash <- applyMerkleProof topicLeafHash proofTopicTriePathH
    return . computeDigest $ (proofDaMetaDataH, daDataHash)

skyDataProofToH ::
  (LiftDato r, DigestibleRef Blake2b_256 (LiftRef r)) =>
  (LiftRef r (MessageData r), SkyDataProof Blake2b_256) ->
  (Blake2b_256, SkyDataProofH)
skyDataProofToH (messageHash, SkyDataPath {..}) =
  ( refDigest messageHash,
    SkyDataProofH
      (refDigest pathDaMetaData)
      (fmap refDigest pathTopicTriePath)
      (refDigest pathTopicMetaData)
      (fmap refDigest pathMessageTriePath)
      (refDigest pathMessageMetaData)
  )

getSkyDataProofH ::
  ( Monad e,
    Functor e,
    LiftWrapping e r,
    LiftDato r,
    DigestibleRef Blake2b_256 (LiftRef r)
  ) =>
  (TopicId, MessageId) ->
  SkyDa r ->
  e (Maybe (Blake2b_256, SkyDataProofH))
getSkyDataProofH ids da = getSkyDataProof ids da >>= return . fmap skyDataProofToH

-- * Meta Declarations

-- PlutusTx.makeLift ''SkyDataProofH
-- PlutusTx.makeIsDataSchemaIndexed ''SkyDataProofH [('SkyDataProofH, 0)]
