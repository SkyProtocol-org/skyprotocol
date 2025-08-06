{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Contract.DaH where

import Common.Crypto
import Common.DA
import Common.Trie
import Common.Types
import Contract.TrieH
import Control.Monad (Monad)
import Data.Functor.Identity (Identity (..))
import GHC.Generics (Generic)
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Functor as PlutusTx
import PlutusTx.Prelude as PlutusTx
import Prelude qualified as HP

-- import Prelude qualified as HP

------------------------------------------------------------------------------
-- Core Data Types
------------------------------------------------------------------------------

-- * Types

data SkyDaH = SkyDaH
  { skyMetaDataH :: Hash,
    skyTopiTrieH :: Hash
  } -- H(DaMetaData), H(TopicTrie)
  deriving (HP.Eq, HP.Show)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

data DaMetaDataH = DaMetaDataH
  { daSchemaH :: Hash,
    daCommitteeH :: Hash
  }
  deriving (HP.Eq, HP.Show)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

type TopicEntryH = (Hash, Hash) -- H(TopicMetaDataH), H(MessageTrieH)

data TopicMetaDataH = TopicMetaDataH
  { topicSchemaH :: Hash,
    topicCommittee :: Hash
  }
  deriving (HP.Eq, HP.Show)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

type TrieTopicPathH t = TriePath Byte TopicId t

type TrieMessagePathH t = TriePath Byte MessageId t

data SkyDataProofH = SkyDataProofH
  { proofDaMetaDataH :: Hash,
    proofTopicTriePathH :: TrieTopicPathH Hash,
    proofTopicMetaDataH :: Hash,
    proofMessageTriePathH :: TrieMessagePathH Hash,
    proofMessageMetaDataH :: Hash
  }
  deriving (HP.Eq, HP.Show)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

-- * Instances

-- * Helpers

-- TODO: In the future, also support proof of non-inclusion.
{-# INLINEABLE applySkyDataProofH #-}
applySkyDataProofH :: SkyDataProofH -> Hash -> Hash
applySkyDataProofH SkyDataProofH {..} messageDataHash =
  runIdentity $ do
    let messageLeaf = Leaf (proofMessageMetaDataH, messageDataHash) :: TrieNodeF Byte Bytes8 _ ()
    let messageLeafHash = computeDigest messageLeaf
    topicDataHash <- applyTrieMerkleProof messageLeafHash proofMessageTriePathH
    let topicLeaf = Leaf (proofTopicMetaDataH, topicDataHash) :: TrieNodeF Byte Bytes8 _ ()
    let topicLeafHash = computeDigest topicLeaf
    daDataHash <- applyTrieMerkleProof topicLeafHash proofTopicTriePathH
    return . computeDigest $ (proofDaMetaDataH, daDataHash)

skyDataProofToH ::
  (LiftDato r, DigestibleRef Hash (LiftRef r)) =>
  (LiftRef r (MessageData r), SkyDataProof Hash) ->
  (Hash, SkyDataProofH)
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
    DigestibleRef Hash (LiftRef r)
  ) =>
  (TopicId, MessageId) ->
  SkyDa r ->
  e (Maybe (Hash, SkyDataProofH))
getSkyDataProofH ids da = getSkyDataProof ids da >>= return . fmap skyDataProofToH

-- * Meta Declarations

--
PlutusTx.makeLift ''SkyDaH
PlutusTx.makeIsDataSchemaIndexed ''SkyDaH [('SkyDaH, 0)]

PlutusTx.makeLift ''DaMetaDataH
PlutusTx.makeIsDataSchemaIndexed ''DaMetaDataH [('DaMetaDataH, 0)]

PlutusTx.makeLift ''TopicMetaDataH
PlutusTx.makeIsDataSchemaIndexed ''TopicMetaDataH [('TopicMetaDataH, 0)]

PlutusTx.makeLift ''SkyDataProofH
PlutusTx.makeIsDataSchemaIndexed ''SkyDataProofH [('SkyDataProofH, 0)]
