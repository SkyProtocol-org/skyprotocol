{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Contract.DaH where

import Common.Crypto
import Common.DA
import Common.Trie
import Common.Types
import Control.Monad (Monad)
import Data.Functor.Identity (Identity (..))
-- import Data.Kind (Type)
-- import PlutusLedgerApi.V1.Time (POSIXTime (..))
import PlutusTx as PlutusTx
import PlutusTx.Blueprint as PlutusTx
import PlutusTx.Functor as PlutusTx
import PlutusTx.Prelude as PlutusTx
import PlutusTx.Show as PlutusTx
-- import Prelude qualified as HP

------------------------------------------------------------------------------
-- Core Data Types
------------------------------------------------------------------------------

-- * Types

data SkyDaH = SkyDaH
  { skyMetaDataH :: Hash -- H(DaMetaData)
  , skyTopicTrieH :: Hash -- H(TopicTrie)
  } -- deriving (HP.Show, HP.Eq)

instance ToByteString SkyDaH where
  byteStringOut SkyDaH {..} isTerminal s = byteStringOut skyMetaDataH NonTerminal $ byteStringOut skyTopicTrieH isTerminal s

newtype DaMetaDataH = DaMetaDataOfTupleH {tupleOfDaMetaDataH :: (Hash, Hash)}
  deriving newtype (PlutusTx.Eq, PlutusTx.Show, ToByteString, FromByteString, ToData, FromData, UnsafeFromData)

-- deriving (HP.Eq, HP.Show)

pattern DaMetaDataH :: Hash -> Hash -> DaMetaDataH -- schema, committee
pattern DaMetaDataH {daSchemaH, daCommitteeH} = DaMetaDataOfTupleH (daSchemaH, daCommitteeH)

{-# COMPLETE DaMetaDataH #-}

type TopicEntryH = (Hash, Hash) -- H(TopicMetaDataH), H(MessageTrieH)

newtype TopicMetaDataH
  = TopicMetaDataOfTupleH {tupleOfTopicMetaDataH :: (Hash, Hash)}
  deriving newtype (PlutusTx.Eq, PlutusTx.Show, ToByteString, FromByteString, ToData, FromData, UnsafeFromData)

pattern TopicMetaDataH :: Hash -> Hash -> TopicMetaDataH
pattern TopicMetaDataH {topicSchemaH, topicCommitteeH} =
  TopicMetaDataOfTupleH (topicSchemaH, topicCommitteeH)

{-# COMPLETE TopicMetaDataH #-}

{-
-- TODO: add message length in the metadata
-- NB: same as MessageMetaData
newtype MessageMetaDataH
  = MessageMetaDataH {getMessageMetaDataH :: POSIXTime}
  deriving newtype (PlutusTx.Eq, PlutusTx.Show, ToByteString, FromByteString, ToData, FromData, UnsafeFromData)

-- NB: same as MessageData
type MessageDataH = BuiltinByteString

-- so we can publish it piecemeal on the chain and/or use ZK Proofs about it.
type Committee = MultiSigPubKey

newtype TopicId = TopicId {getTopicId :: Bytes8}
  deriving newtype
    ( ToInt,
      FromInt,
      UnsafeFromData,
      ToData,
      FromData,
      BitLogic,
      FromByteString,
      ToByteString,
      PlutusTx.Show,
      PlutusTx.Eq,
      HP.Show,
      HP.Eq
    )

topicIdFromInteger :: Integer -> TopicId
topicIdFromInteger = fromInt

instance TrieKey TopicId

instance TrieHeightKey Byte TopicId

newtype MessageId = MessageId {getMessageId :: Bytes8}
  deriving newtype
    ( ToInt,
      FromInt,
      UnsafeFromData,
      ToData,
      FromData,
      BitLogic,
      FromByteString,
      ToByteString,
      PlutusTx.Show,
      PlutusTx.Eq,
      HP.Show,
      HP.Eq
    )

messageIdFromInteger :: Integer -> MessageId
messageIdFromInteger = fromInt

instance TrieKey MessageId

instance TrieHeightKey Byte MessageId
-}

type TrieTopicPathH t = TriePath Byte TopicId t

type TrieMessagePathH t = TriePath Byte MessageId t

data SkyDataProofH
  = SkyDataProofH
  { proofDaMetaDataH :: Hash
  , proofTopicTriePathH :: TrieTopicPathH Hash
  , proofTopicMetaDataH :: Hash
  , proofMessageTriePathH :: TrieMessagePathH Hash
  , proofMessageMetaDataH :: Hash }
  deriving
    ( PlutusTx.Eq,
      PlutusTx.Show,
      ToByteString,
      FromByteString
    )

{-# COMPLETE SkyDataProofH #-}

-- * Instances

-- * Helpers

-- TODO: In the future, also support proof of non-inclusion.
{-# INLINEABLE applySkyDataProofH #-}
applySkyDataProofH :: SkyDataProofH -> Hash -> Hash
applySkyDataProofH SkyDataProofH {..} messageDataHash =
  runIdentity $ do
    let messageLeaf = Leaf (proofMessageMetaDataH, messageDataHash) :: TrieNodeF Byte Bytes8 _ ()
    let messageLeafHash = computeDigest $ messageLeaf
    topicDataHash <- applyMerkleProof messageLeafHash $ proofMessageTriePathH
    let topicLeaf = Leaf (proofTopicMetaDataH, topicDataHash) :: TrieNodeF Byte Bytes8 _ ()
    let topicLeafHash = computeDigest $ topicLeaf
    daDataHash <- applyMerkleProof topicLeafHash $ proofTopicTriePathH
    return . computeDigest $ (proofDaMetaDataH, daDataHash)

getSkyDataProofH :: (Monad e, Functor e, LiftWrapping e r, LiftDato r, DigestibleRef Hash (LiftRef r)) => (TopicId, MessageId) -> SkyDa r -> e (Maybe (Hash, SkyDataProofH))
getSkyDataProofH ids da =
  getSkyDataProof ids da >>=
    return .
      fmap
        \case
          (messageHash, SkyDataPath {..}) ->
            (refDigest messageHash,
             SkyDataProofH
               (refDigest pathDaMetaData)
               (fmap refDigest pathTopicTriePath)
               (refDigest pathTopicMetaData)
               (fmap refDigest pathMessageTriePath)
               (refDigest pathMessageMetaData))

-- * Meta Declarations

-- PlutusTx.makeLift ''SkyDataProofH
-- PlutusTx.makeIsDataSchemaIndexed ''SkyDataProofH [('SkyDataProofH, 0)]
