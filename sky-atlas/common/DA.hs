{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DA where

import Control.Monad (Monad)
import Crypto
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import PlutusLedgerApi.V1.Time (POSIXTime (..))
import PlutusTx
import PlutusTx.Functor
import PlutusTx.Prelude
import PlutusTx.Show
import Trie
import Types

------------------------------------------------------------------------------
-- Core Data Types
------------------------------------------------------------------------------

-- * Types

type SkyDa r = (LiftRef r (DaMetaData r), LiftRef r (DaData r))

newtype DaMetaData r = DaMetaData_ {getDaMetaData :: (DataHash, LiftRef r Committee)}
  deriving (Eq, ToByteString, FromByteString, ToData, FromData, UnsafeFromData, Dato) via (DataHash, LiftRef r Committee)
pattern DaMetaData :: forall r . DataHash -> LiftRef r Committee -> DaMetaData r
pattern DaMetaData {daSchema, daCommittee} = DaMetaData_ (daSchema, daCommittee)
{-# COMPLETE DaMetaData #-}

type DaData r = TopicTrie r

type TopicTrie r = Trie64 r (TopicEntry r)

type TopicEntry r = (LiftRef r (TopicMetaData r), LiftRef r (MessageTrie r))

newtype TopicMetaData r = TopicMetaData_ (DataHash, LiftRef r Committee)
  deriving (Eq, ToByteString, FromByteString, ToData, FromData, UnsafeFromData, Dato) via (DataHash, LiftRef r Committee)
pattern TopicMetaData :: forall r . DataHash -> LiftRef r Committee -> TopicMetaData r
pattern TopicMetaData {topicSchema, topicCommittee} = TopicMetaData_ (topicSchema, topicCommittee)
{-# COMPLETE TopicMetaData #-}

type MessageTrie r = Trie64 r (MessageEntry r)

type MessageEntry r = (LiftRef r (MessageMetaData r), LiftRef r (MessageData r))

newtype MessageMetaData (r :: Type -> Type) = MessageMetaData_ (PubKey, POSIXTime)
  deriving (Eq, ToByteString, FromByteString, ToData, FromData, UnsafeFromData) via (PubKey, POSIXTime)
pattern MessageMetaData :: PubKey -> POSIXTime -> MessageMetaData r
pattern MessageMetaData {messagePoster, messageTime} = MessageMetaData_ (messagePoster, messageTime)
{-# COMPLETE MessageMetaData #-}

type MessageData (r :: Type -> Type) = BuiltinByteString

-- so we can publish it piecemeal on the chain and/or use ZK Proofs about it.
type Committee = MultiSigPubKey

type TopicId = Bytes8

type MessageId = Bytes8

type Trie64 r c = Trie r Byte Bytes8 c

type Trie64NodeRef r c = TrieNodeRef r Byte Bytes8 c

type Trie64Path t = TriePath Byte Bytes8 t

newtype SkyDataPath (r :: Type -> Type) = SkyDataPath_ (LiftRef r (DaMetaData r), Trie64Path (Trie64NodeRef r (TopicEntry r)), LiftRef r (TopicMetaData r), Trie64Path (Trie64NodeRef r (MessageEntry r)), LiftRef r (MessageMetaData r))
  deriving (Eq, ToByteString, FromByteString, ToData, FromData, UnsafeFromData) via (LiftRef r (DaMetaData r), Trie64Path (Trie64NodeRef r (TopicEntry r)), LiftRef r (TopicMetaData r), Trie64Path (Trie64NodeRef r (MessageEntry r)), LiftRef r (MessageMetaData r))
pattern SkyDataPath :: forall r . LiftRef r (DaMetaData r) -> Trie64Path (Trie64NodeRef r (TopicEntry r)) -> LiftRef r (TopicMetaData r) -> Trie64Path (Trie64NodeRef r (MessageEntry r)) -> LiftRef r (MessageMetaData r) -> SkyDataPath r
pattern SkyDataPath {pathDaMetaData, pathTopicTriePath, pathTopicMetaData, pathMessageTriePath, pathMessageMetaData} = SkyDataPath_ (pathDaMetaData, pathTopicTriePath, pathTopicMetaData, pathMessageTriePath, pathMessageMetaData)
{-# COMPLETE SkyDataPath #-}

type SkyDataProof hf = SkyDataPath (Digest hf)

-- * Instances

-- ** MessageMetaData

instance (LiftShow r) => Show (MessageMetaData r) where
  showsPrec prec (MessageMetaData p t) = showApp prec "MessageMetaData" [showArg p, showArg t]

instance (LiftDato r) => Dato (MessageMetaData r)

-- ** TopicMetaData

instance (LiftShow r) => Show (TopicMetaData r) where
  showsPrec prec (TopicMetaData s c) = showApp prec "TopicMetaData" [showArg s, showArg c]

-- ** DaMetaData

instance (LiftShow r) => Show (DaMetaData r) where
  showsPrec prec (DaMetaData s c) = showApp prec "DaMetaData" [showArg s, showArg c]

-- ** SkyDataPath

instance ConvertTo (LiftRef r (DaMetaData r), Trie64Path (Trie64NodeRef r (TopicEntry r)), LiftRef r (TopicMetaData r), Trie64Path (Trie64NodeRef r (MessageEntry r)), LiftRef r (MessageMetaData r)) (SkyDataPath r) where
  convertTo = uncurry5 SkyDataPath
instance ConvertFrom (LiftRef r (DaMetaData r), Trie64Path (Trie64NodeRef r (TopicEntry r)), LiftRef r (TopicMetaData r), Trie64Path (Trie64NodeRef r (MessageEntry r)), LiftRef r (MessageMetaData r)) (SkyDataPath r) where
  convertFrom (SkyDataPath a b c d e) = (a, b, c, d, e)

instance (LiftShow r) => Show (SkyDataPath r) where
  showsPrec prec (SkyDataPath dmd ttp tmd mtp mmd) =
    showApp prec "SkyDataPath" [showArg dmd, showArg ttp, showArg tmd, showArg mtp, showArg mmd]

instance (LiftDato r) => Dato (SkyDataPath r)

-- ** Trie64

instance TrieHeight Byte

instance TrieKey Bytes8

instance TrieHeightKey Byte Bytes8

-- * Helpers

tupleOfMessageMetaData :: MessageMetaData r -> (PubKey, POSIXTime)
tupleOfMessageMetaData MessageMetaData {..} = (messagePoster, messageTime)

tupleOfTopicMetaData :: TopicMetaData r -> (DataHash, LiftRef r Committee)
tupleOfTopicMetaData TopicMetaData {..} = (topicSchema, topicCommittee)

tupleOfSkyDataPath :: SkyDataPath r -> (LiftRef r (DaMetaData r), Trie64Path (Trie64NodeRef r (TopicEntry r)), LiftRef r (TopicMetaData r), Trie64Path (Trie64NodeRef r (MessageEntry r)), LiftRef r (MessageMetaData r))
tupleOfSkyDataPath SkyDataPath {..} = (pathDaMetaData, pathTopicTriePath, pathTopicMetaData, pathMessageTriePath, pathMessageMetaData)

-- TODO: generate a new Committee from PoS instead of just copying the PoA committee
-- Maybe it should depend on a seed based on the topicId and more.
generateCommittee :: (Monad e, Functor e, LiftWrapping e r, LiftDato r) => SkyDa r -> e (LiftRef r Committee)
generateCommittee (m, _) = unwrap m <&> committeeOfDaMetaData

committeeOfDaMetaData :: DaMetaData r -> LiftRef r Committee
committeeOfDaMetaData (DaMetaData _ c) = c


-- TODO: add authentication, payment, etc., if not in this function, in one that wraps around it.
insertTopic ::
  (Monad e, Functor e, LiftWrapping e r, LiftDato r) =>
  DataHash ->
  SkyDa r ->
  e (Maybe TopicId, SkyDa r)
insertTopic newTopicSchema da@(rDaMetaData, rOldTopicTrie) =
  do
    newTopicCommittee <- generateCommittee da
    oldTopicTrie <- unwrap rOldTopicTrie
    newTopicId <- nextIndex oldTopicTrie
    case newTopicId of
      Nothing -> return (Nothing, da)
      Just topicId -> do
        newMessageTrie <- empty
        rNewMessageTrie <- wrap newMessageTrie
        rNewTopicMetaData <- wrap $ TopicMetaData newTopicSchema newTopicCommittee
        let newTopic = (rNewTopicMetaData, rNewMessageTrie)
        rNewTopicTrie <- insert newTopic topicId oldTopicTrie >>= wrap
        return (newTopicId, (rDaMetaData, rNewTopicTrie))

insertMessage ::
  (Monad e, Functor e, LiftWrapping e r, LiftDato r) =>
  PubKey ->
  POSIXTime ->
  BuiltinByteString ->
  TopicId ->
  SkyDa r ->
  e (Maybe MessageId, SkyDa r)
insertMessage poster timestamp newMessage topicId da@(rDaMetaData, rOldTopicTrie) =
  do
    oldTopicTrie <- unwrap rOldTopicTrie
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
            rNewTopicTrie <-
              insert (rTopicMetaData, rNewMessageTrie) topicId oldTopicTrie
                >>= wrap
            return (Just messageId, (rDaMetaData, rNewTopicTrie))

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
getSkyDataPath (topicId, messageId) (rDaMetaData, rTopicTrie) = do
  (Zip rTopicEntry topicPath) <- unwrap rTopicTrie >>= zipperOf >>= refocus topicId
  fr rTopicEntry >>= \case
    Leaf (rTopicMetaData, rMessageTrie) -> do
      (Zip rMessageEntry messagePath) <- unwrap rMessageTrie >>= zipperOf >>= refocus messageId
      fr rMessageEntry >>= \case
        Leaf (rMessageMetaData, rMessageData) ->
          return
            $ Just
              ( rMessageData,
                SkyDataPath rDaMetaData topicPath rTopicMetaData messagePath rMessageMetaData
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
  rMessageTrie <- empty >>= wrap
  rCommittee <- wrap daCommittee
  rMetaData <- wrap (DaMetaData daSchema rCommittee)
  return (rMetaData, rMessageTrie)

-- TODO: implement a "monadic lens" instead?
updateDaCommittee :: (LiftDato r, LiftWrapping e r) => Committee -> SkyDa r -> e (SkyDa r)
updateDaCommittee newCommittee (rDaMetaData, rTopicTrie) = do
  rNewCommittee <- wrap newCommittee
  DaMetaData_ (daSchema, _) <- unwrap rDaMetaData
  rNewDaMetaData <- wrap $ DaMetaData daSchema rNewCommittee
  return (rNewDaMetaData, rTopicTrie)

-- * Meta Declarations

{-
$(PlutusTx.makeLift ''SkyDataPath)
$(PlutusTx.makeIsDataSchemaIndexed ''SkyDataPath [('SkyDataPath, 0)])
-}

foo :: LiftFromByteString r => BuiltinByteString -> (DaMetaData r, TopicMetaData r, MessageMetaData r, TopicEntry r, MessageEntry r)
foo x = (fromByteString x, fromByteString x, fromByteString x, fromByteString x, fromByteString x)

