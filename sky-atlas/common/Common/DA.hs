{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.DA where

import Common.Crypto
import Common.Trie
import Common.Types
import Control.Monad (Monad)
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import PlutusLedgerApi.V1.Time (POSIXTime (..))
import PlutusTx
import PlutusTx.Functor
import PlutusTx.Prelude
import PlutusTx.Show

------------------------------------------------------------------------------
-- Core Data Types
------------------------------------------------------------------------------

-- * Types

data SkyDa r = SkyDA
  { skyMetaData :: LiftRef r (DaMetaData r),
    skyTopicTrie :: LiftRef r (TopicTrie r)
  }
  deriving (ToByteString)

newtype DaMetaData r
  = DaMetaDataOfTuple {tupleOfDaMetaData :: (DataHash, LiftRef r Committee)}
  deriving (Eq, ToByteString, FromByteString, ToData, FromData, UnsafeFromData) via (DataHash, LiftRef r Committee)

pattern DaMetaData :: forall r. DataHash -> LiftRef r Committee -> DaMetaData r
pattern DaMetaData {daSchema, daCommittee} = DaMetaDataOfTuple (daSchema, daCommittee)

{-# COMPLETE DaMetaData #-}

type TopicTrie r = Trie r Byte TopicId (TopicEntry r)

type TopicEntry r = (LiftRef r (TopicMetaData r), LiftRef r (MessageTrie r))

newtype TopicMetaData r
  = TopicMetaDataOfTuple {tupleOfTopicMetaData :: (DataHash, LiftRef r Committee)}
  deriving (Eq, ToByteString, FromByteString, ToData, FromData, UnsafeFromData) via (DataHash, LiftRef r Committee)

pattern TopicMetaData :: forall r. DataHash -> LiftRef r Committee -> TopicMetaData r
pattern TopicMetaData {topicSchema, topicCommittee} = TopicMetaDataOfTuple (topicSchema, topicCommittee)

{-# COMPLETE TopicMetaData #-}

type MessageTrie r = Trie r Byte MessageId (MessageEntry r)

type MessageEntry r = (LiftRef r (MessageMetaData r), LiftRef r (MessageData r))

newtype MessageMetaData (r :: Type -> Type)
  = MessageMetaDataOfTuple {tupleOfMessageMetaData :: (PubKey, POSIXTime)}
  deriving (Eq, ToByteString, FromByteString, ToData, FromData, UnsafeFromData) via (PubKey, POSIXTime)

pattern MessageMetaData :: PubKey -> POSIXTime -> MessageMetaData r
pattern MessageMetaData {messagePoster, messageTime} = MessageMetaDataOfTuple (messagePoster, messageTime)

{-# COMPLETE MessageMetaData #-}

type MessageData (r :: Type -> Type) = BuiltinByteString

-- so we can publish it piecemeal on the chain and/or use ZK Proofs about it.
type Committee = MultiSigPubKey

newtype TopicId = TopicId {getTopicId :: Bytes8}
  deriving
    ( ToInt,
      FromInt,
      UnsafeFromData,
      ToData,
      FromData,
      Show,
      BitLogic,
      FromByteString,
      ToByteString,
      Eq
    )
    via Bytes8

instance TrieKey TopicId

instance TrieHeightKey Byte TopicId

newtype MessageId = MessageId {getMessageId :: Bytes8}
  deriving
    ( ToInt,
      FromInt,
      UnsafeFromData,
      ToData,
      FromData,
      Show,
      BitLogic,
      FromByteString,
      ToByteString,
      Eq
    )
    via Bytes8

instance TrieKey MessageId

instance TrieHeightKey Byte MessageId

type Trie64 r c = Trie r Byte Bytes8 c

type Trie64NodeRef r c = TrieNodeRef r Byte Bytes8 c

type TrieTopicNodeRef r c = TrieNodeRef r Byte TopicId c

type TrieMessageNodeRef r c = TrieNodeRef r Byte MessageId c

type Trie64Path t = TriePath Byte Bytes8 t

type TrieTopicPath t = TriePath Byte TopicId t

type TrieMessagePath t = TriePath Byte MessageId t

newtype SkyDataPath (r :: Type -> Type)
  = SkyDataPathOfTuple
  { tupleOfSkyDataPath ::
      ( LiftRef r (DaMetaData r),
        TrieTopicPath (TrieTopicNodeRef r (TopicEntry r)),
        LiftRef r (TopicMetaData r),
        TrieMessagePath (TrieMessageNodeRef r (MessageEntry r)),
        LiftRef r (MessageMetaData r)
      )
  }
  deriving
    ( Eq,
      ToByteString,
      FromByteString,
      ToData,
      FromData,
      UnsafeFromData
    )
    via ( LiftRef r (DaMetaData r),
          TrieTopicPath (TrieTopicNodeRef r (TopicEntry r)),
          LiftRef r (TopicMetaData r),
          TrieMessagePath (TrieMessageNodeRef r (MessageEntry r)),
          LiftRef r (MessageMetaData r)
        )

pattern SkyDataPath :: forall r. LiftRef r (DaMetaData r) -> TrieTopicPath (TrieTopicNodeRef r (TopicEntry r)) -> LiftRef r (TopicMetaData r) -> TrieMessagePath (TrieMessageNodeRef r (MessageEntry r)) -> LiftRef r (MessageMetaData r) -> SkyDataPath r
pattern SkyDataPath {pathDaMetaData, pathTopicTriePath, pathTopicMetaData, pathMessageTriePath, pathMessageMetaData} = SkyDataPathOfTuple (pathDaMetaData, pathTopicTriePath, pathTopicMetaData, pathMessageTriePath, pathMessageMetaData)

{-# COMPLETE SkyDataPath #-}

type SkyDataProof hf = SkyDataPath (Digest hf)

-- * Instances

-- ** MessageMetaData

instance (LiftShow r) => Show (MessageMetaData r) where
  showsPrec prec (MessageMetaData p t) = showApp prec "MessageMetaData" [showArg p, showArg t]

-- ** TopicMetaData

instance (LiftShow r) => Show (TopicMetaData r) where
  showsPrec prec (TopicMetaData s c) = showApp prec "TopicMetaData" [showArg s, showArg c]

-- ** DaMetaData

instance (LiftShow r) => Show (DaMetaData r) where
  showsPrec prec (DaMetaData s c) = showApp prec "DaMetaData" [showArg s, showArg c]

-- ** SkyDataPath

instance (LiftShow r) => Show (SkyDataPath r) where
  showsPrec prec (SkyDataPath dmd ttp tmd mtp mmd) =
    showApp prec "SkyDataPath" [showArg dmd, showArg ttp, showArg tmd, showArg mtp, showArg mmd]

-- ** Trie64

instance TrieHeight Byte

instance TrieKey Bytes8

instance TrieHeightKey Byte Bytes8

-- * Helpers

-- TODO: generate a new Committee from PoS instead of just copying the PoA committee
-- Maybe it should depend on a seed based on the topicId and more.
generateCommittee :: (Monad e, Functor e, LiftWrapping e r, LiftDato r) => SkyDa r -> e (LiftRef r Committee)
generateCommittee SkyDA {..} = unwrap skyMetaData <&> committeeOfDaMetaData

committeeOfDaMetaData :: DaMetaData r -> LiftRef r Committee
committeeOfDaMetaData (DaMetaData _ c) = c

-- TODO: add authentication, payment, etc., if not in this function, in one that wraps around it.
insertTopic ::
  (Monad e, Functor e, LiftWrapping e r, LiftDato r) =>
  DataHash ->
  SkyDa r ->
  e (Maybe TopicId, SkyDa r)
insertTopic newTopicSchema da@SkyDA {..} =
  do
    newTopicCommittee <- generateCommittee da
    oldTopicTrie <- unwrap skyTopicTrie
    newTopicId <- nextIndex oldTopicTrie
    case newTopicId of
      Nothing -> return (Nothing, da)
      Just topicId -> do
        newMessageTrie <- empty
        rNewMessageTrie <- wrap newMessageTrie
        rNewTopicMetaData <- wrap $ TopicMetaData newTopicSchema newTopicCommittee
        let newTopic = (rNewTopicMetaData, rNewMessageTrie)
        skyTopicTrieNew <- insert newTopic topicId oldTopicTrie >>= wrap
        return (newTopicId, SkyDA {skyTopicTrie = skyTopicTrieNew, ..})

insertMessage ::
  (Monad e, Functor e, LiftWrapping e r, LiftDato r) =>
  PubKey ->
  POSIXTime ->
  BuiltinByteString ->
  TopicId ->
  SkyDa r ->
  e (Maybe MessageId, SkyDa r)
insertMessage poster timestamp newMessage topicId da@SkyDA {..} =
  do
    oldTopicTrie <- unwrap skyTopicTrie
    oldMaybeTopicEntry <- lookup topicId oldTopicTrie
    case oldMaybeTopicEntry of
      Nothing -> return (Nothing, da)
      Just (rTopicMetaData, rOldMessageTrie) -> do
        oldMessageTrie <- unwrap rOldMessageTrie
        rNewMessageMetaData <- wrap $ MessageMetaData poster timestamp
        rNewMessageData <- wrap newMessage
        newMessageId <- nextIndex oldMessageTrie
        case newMessageId of
          Nothing -> return (Nothing, da) -- error "table full"
          Just messageId -> do
            rNewMessageTrie <-
              insert (rNewMessageMetaData, rNewMessageData) messageId oldMessageTrie
                >>= wrap
            skyTopicTrieNew <-
              insert (rTopicMetaData, rNewMessageTrie) topicId oldTopicTrie
                >>= wrap
            return (Just messageId, SkyDA {skyTopicTrie = skyTopicTrieNew, ..})

-- TODO: In the future, also support proof of non-inclusion.
{-# INLINEABLE applySkyDataProof #-}
applySkyDataProof :: (HashFunction hf) => SkyDataProof hf -> DataDigest hf -> DataDigest hf
applySkyDataProof SkyDataPath {..} messageDataHash =
  runIdentity $ do
    let messageLeaf = Leaf (liftref pathMessageMetaData, messageDataHash) :: TrieNodeF Byte Bytes8 _ ()
    let messageLeafHash = castDigest . computeDigest $ messageLeaf
    topicDataHash <- applyMerkleProof messageLeafHash $ fmap (castDigest . liftref) pathMessageTriePath
    let topicLeaf = Leaf (liftref pathTopicMetaData, topicDataHash) :: TrieNodeF Byte Bytes8 _ ()
    let topicLeafHash = castDigest . computeDigest $ topicLeaf
    daDataHash <- applyMerkleProof topicLeafHash $ fmap (castDigest . liftref) pathTopicTriePath
    return . castDigest . computeDigest $ (liftref pathDaMetaData, daDataHash)

getSkyDataPath :: (Monad e, Functor e, LiftWrapping e r, LiftDato r) => (TopicId, MessageId) -> SkyDa r -> e (Maybe (LiftRef r (MessageData r), SkyDataPath r))
getSkyDataPath (topicId, messageId) SkyDA {..} = do
  (Zip rTopicEntry topicPath) <- unwrap skyTopicTrie >>= zipperOf >>= refocus topicId
  fr rTopicEntry >>= \case
    Leaf (rTopicMetaData, rMessageTrie) -> do
      (Zip rMessageEntry messagePath) <- unwrap rMessageTrie >>= zipperOf >>= refocus messageId
      fr rMessageEntry >>= \case
        Leaf (rMessageMetaData, rMessageData) ->
          return
            $ Just
              ( rMessageData,
                SkyDataPath skyMetaData topicPath rTopicMetaData messagePath rMessageMetaData
              )
        _ -> return Nothing
    _ -> return Nothing

getSkyDataProof :: (Monad e, HashFunction hf, Functor e, LiftWrapping e r, LiftDato r, DigestibleRef hf r) => (TopicId, MessageId) -> SkyDa r -> e (Maybe (LiftRef r (MessageData r), SkyDataProof hf))
getSkyDataProof ids da =
  getSkyDataPath ids da >>= \case
    Nothing -> return Nothing
    Just (rMessageData, path) -> return $ Just (rMessageData, proofOfSkyDataPath path)

proofOfSkyDataPath :: (HashFunction hf, LiftDato r, DigestibleRef hf r) => SkyDataPath r -> SkyDataProof hf
proofOfSkyDataPath SkyDataPath {..} =
  SkyDataPath
    (lcg pathDaMetaData)
    (fmap lcg pathTopicTriePath)
    (lcg pathTopicMetaData)
    (fmap lcg pathMessageTriePath)
    (lcg pathMessageMetaData)
  where
    lcg = LiftRef . castDigest . getDigest

initDa :: (LiftDato r, LiftWrapping e r) => DataHash -> Committee -> e (SkyDa r)
initDa daSchema daCommittee = do
  skyTopicTrie <- empty >>= wrap
  rCommittee <- wrap daCommittee
  skyMetaData <- wrap (DaMetaData daSchema rCommittee)
  return SkyDA {..}

-- TODO: implement a "monadic lens" instead?
updateDaCommittee :: (LiftDato r, LiftWrapping e r) => Committee -> SkyDa r -> e (SkyDa r)
updateDaCommittee newCommittee SkyDA {..} = do
  rNewCommittee <- wrap newCommittee
  DaMetaDataOfTuple (daSchema, _) <- unwrap skyMetaData
  skyMetaDataNew <- wrap $ DaMetaData daSchema rNewCommittee
  return SkyDA {skyMetaData = skyMetaDataNew, ..}

-- * Meta Declarations

{-
\$(PlutusTx.makeLift ''SkyDataPath)
\$(PlutusTx.makeIsDataSchemaIndexed ''SkyDataPath [('SkyDataPath, 0)])
-}
