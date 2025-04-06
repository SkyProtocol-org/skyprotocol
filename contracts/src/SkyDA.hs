{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
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
{-# LANGUAGE PartialTypeSignatures      #-}
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
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module SkyDA where

import SkyBase
import SkyCrypto
import Trie

import PlutusTx.Prelude
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Builtins
import PlutusTx.Functor
import PlutusTx.Show
import PlutusTx.Utils

import PlutusLedgerApi.V1.Crypto (PubKeyHash(..))
import PlutusLedgerApi.V1.Time (POSIXTime(..)) -- in millisecond since Unix Epoch
import PlutusLedgerApi.V1.Value (CurrencySymbol(..))

import Control.Composition ((-.))
import Control.Monad (Monad, (>=>))
import Data.Function ((&))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import GHC.Generics (Generic)

------------------------------------------------------------------------------
-- Core Data Types
------------------------------------------------------------------------------

-- * Types
type SkyDa r = (LiftRef r (DaMetaData r), LiftRef r (DaData r))
data DaMetaData r = DaMetaData
  { daSchema :: DataHash
  , daCommittee :: LiftRef r Committee }
type DaData r = TopicTrie r
type TopicTrie r = Trie64 r (TopicEntry r)
type TopicEntry r = (LiftRef r (TopicMetaData r), LiftRef r (MessageTrie r))
data TopicMetaData r = TopicMetaData
  { topicSchema :: DataHash
  , topicCommittee :: LiftRef r Committee }
type MessageTrie r = Trie64 r (MessageEntry r)
type MessageEntry r = (LiftRef r (MessageMetaData r), LiftRef r (MessageData r))
data MessageMetaData (r :: Type -> Type) = MessageMetaData
  { messagePoster :: PubKey
  , messageTime :: POSIXTime }
type MessageData (r :: Type -> Type) = VariableLengthByteString

-- so we can publish it piecemeal on the chain and/or use ZK Proofs about it.
type Committee = MultiSigPubKey
type TopicId = Bytes8
type MessageId = Bytes8
type Trie64 r c = Trie r Byte Bytes8 c
type Trie64NodeRef r c = TrieNodeRef r Byte Bytes8 c
type Trie64Path t = TriePath Byte Bytes8 t

data SkyDataPath (r :: Type -> Type) = SkyDataPath
  { pathDaMetaData :: LiftRef r (DaMetaData r)
  , pathTopicTriePath :: Trie64Path (Trie64NodeRef r (TopicEntry r))
  , pathTopicMetaData :: LiftRef r (TopicMetaData r)
  , pathMessageTriePath :: Trie64Path (Trie64NodeRef r (MessageEntry r))
  , pathMessageMetaData :: LiftRef r (MessageMetaData r) }

type SkyDataProof = SkyDataPath Hash

-- * Instances

-- ** MessageMetaData
instance LiftEq r => Eq (MessageMetaData r) where
  MessageMetaData p t == MessageMetaData p' t' = p == p' && t == t'
instance LiftShow r => Show (MessageMetaData r) where
  showsPrec prec (MessageMetaData p t) = showApp prec "MessageMetaData" [showArg p, showArg t]
instance ToByteString (MessageMetaData r) where
  toByteString = toByteStringOut
instance ByteStringOut (MessageMetaData r) where
  byteStringOut = byteStringOut . tupleOfMessageMetaData
instance LiftDato r => Dato (MessageMetaData r) where
instance LiftByteStringIn r => ByteStringIn (MessageMetaData r) where
  byteStringIn = byteStringIn <&> uncurry MessageMetaData

-- ** TopicMetaData
instance LiftEq r => Eq (TopicMetaData r) where
  TopicMetaData s c == TopicMetaData s' c' = s == s' && c == c'
instance LiftShow r => Show (TopicMetaData r) where
  showsPrec prec (TopicMetaData s c) = showApp prec "TopicMetaData" [showArg s, showArg c]
instance LiftByteStringOut r => ToByteString (TopicMetaData r) where
  toByteString = toByteStringOut
instance LiftByteStringOut r => ByteStringOut (TopicMetaData r) where
  byteStringOut = byteStringOut . tupleOfTopicMetaData
instance LiftDato r => Dato (TopicMetaData r) where
instance LiftByteStringIn r => ByteStringIn (TopicMetaData r) where
  byteStringIn = byteStringIn <&> uncurry TopicMetaData

-- ** DaMetaData
instance LiftEq r => Eq (DaMetaData r) where
  DaMetaData s c == DaMetaData s' c' = s == s' && c == c'
instance LiftShow r => Show (DaMetaData r) where
  showsPrec prec (DaMetaData s c) = showApp prec "DaMetaData" [showArg s, showArg c]
instance LiftByteStringOut r => ToByteString (DaMetaData r) where
  toByteString = toByteStringOut
instance LiftByteStringOut r => ByteStringOut (DaMetaData r) where
  byteStringOut = byteStringOut . tupleOfDaMetaData
instance LiftDato r => Dato (DaMetaData r) where
instance LiftByteStringIn r => ByteStringIn (DaMetaData r) where
  byteStringIn = byteStringIn <&> uncurry DaMetaData

-- ** SkyDataPath
instance (LiftEq r) => Eq (SkyDataPath r) where
  SkyDataPath dmd ttp tmd mtp mmd == SkyDataPath dmd' ttp' tmd' mtp' mmd' =
    dmd == dmd' && ttp == ttp' && tmd == tmd' && mtp == mtp' && mmd == mmd'
instance (LiftShow r) => Show (SkyDataPath r) where
  showsPrec prec (SkyDataPath dmd ttp tmd mtp mmd) =
    showApp prec "SkyDataPath" [showArg dmd, showArg ttp, showArg tmd, showArg mtp, showArg mmd]
instance LiftDato r => ByteStringOut (SkyDataPath r) where
  byteStringOut = byteStringOut . tupleOfSkyDataPath
instance LiftByteStringIn r => ByteStringIn (SkyDataPath r) where
  byteStringIn = byteStringIn <&> uncurry5 SkyDataPath

-- ** Trie64
instance TrieHeight Byte where
instance TrieKey Bytes8 where
instance TrieHeightKey Byte Bytes8 where

-- * Helpers
tupleOfMessageMetaData :: MessageMetaData r -> (PubKey, POSIXTime)
tupleOfMessageMetaData MessageMetaData {..} = (messagePoster, messageTime)
tupleOfTopicMetaData :: TopicMetaData r -> (DataHash, LiftRef r Committee)
tupleOfTopicMetaData TopicMetaData {..} = (topicSchema, topicCommittee)
tupleOfDaMetaData :: DaMetaData r -> (DataHash, LiftRef r Committee)
tupleOfDaMetaData DaMetaData {..} = (daSchema, daCommittee)
tupleOfSkyDataPath :: SkyDataPath r -> (LiftRef r (DaMetaData r), Trie64Path (Trie64NodeRef r (TopicEntry r)), LiftRef r (TopicMetaData r), Trie64Path (Trie64NodeRef r (MessageEntry r)), LiftRef r (MessageMetaData r))
tupleOfSkyDataPath SkyDataPath {..} = (pathDaMetaData, pathTopicTriePath, pathTopicMetaData, pathMessageTriePath, pathMessageMetaData)

-- TODO: generate a new Committee from PoS instead of just copying the PoA committee
-- Maybe it should depend on a seed based on the topicId and more.
generateCommittee :: (Monad e, Functor e, LiftWrapping e r, LiftDato r) => SkyDa r -> e (LiftRef r Committee)
generateCommittee (m, _) = unwrap m <&> daCommittee

-- TODO: add authentication, payment, etc., if not in this function, in one that wraps around it.
insertTopic :: (Monad e, Functor e, LiftWrapping e r, LiftDato r) =>
  DataHash -> SkyDa r -> e (TopicId, SkyDa r)
insertTopic newTopicSchema da@(rDaMetaData, rOldTopicTrie) =
  do
    newTopicCommittee <- generateCommittee da
    oldTopicTrie <- unwrap rOldTopicTrie
    newTopicId <- nextIndex oldTopicTrie
    newMessageTrie <- empty
    rNewMessageTrie <- wrap newMessageTrie
    rNewTopicMetaData <- wrap $ TopicMetaData newTopicSchema newTopicCommittee
    let newTopic = (rNewTopicMetaData, rNewMessageTrie)
    rNewTopicTrie <- insert newTopic newTopicId oldTopicTrie >>= wrap
    return (newTopicId, (rDaMetaData, rNewTopicTrie))

insertMessage :: (Monad e, Functor e, LiftWrapping e r, LiftDato r) =>
  PubKey -> POSIXTime -> VariableLengthByteString -> TopicId -> SkyDa r -> e (Maybe MessageId, SkyDa r)
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
        rNewMessageTrie <- insert (rNewMessageMetaData, rNewMessageData) newMessageId oldMessageTrie >>= wrap
        rNewTopicTrie <- insert (rTopicMetaData, rNewMessageTrie) topicId oldTopicTrie >>= wrap
        return (Just newMessageId, (rDaMetaData, rNewTopicTrie))

-- TODO: In the future, also support proof of non-inclusion.
{-# INLINEABLE applySkyDataProof #-}
applySkyDataProof :: SkyDataProof -> DataHash -> DataHash
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
getSkyDataPath (topicId, messageId) da@(rDaMetaData, rTopicTrie) = do
  (Zip rTopicEntry topicPath) <- unwrap rTopicTrie >>= zipperOf >>= refocus topicId
  fr rTopicEntry >>= \case
    Empty -> return Nothing
    Leaf (rTopicMetaData, rMessageTrie) -> do
      zMessageTrie@(Zip rMessageEntry messagePath) <- unwrap rMessageTrie >>= zipperOf >>= refocus messageId
      fr rMessageEntry >>= \case
        Empty -> return Nothing
        Leaf (rMessageMetaData, rMessageData) ->
          return $ Just (rMessageData,
                         SkyDataPath rDaMetaData topicPath rTopicMetaData messagePath rMessageMetaData)

getSkyDataProof :: (Monad e, Functor e, LiftWrapping e r, LiftDato r, DigestibleRef Blake2b_256 r) => (TopicId, MessageId) -> SkyDa r -> e (Maybe (LiftRef r (MessageData r), SkyDataProof))
getSkyDataProof ids da =
  getSkyDataPath ids da >>= \case
    Nothing -> return Nothing
    Just (rMessageData, path) -> return $ Just (rMessageData, proofOfSkyDataPath path)

proofOfSkyDataPath :: (LiftDato r, DigestibleRef Blake2b_256 r) => SkyDataPath r -> SkyDataProof
proofOfSkyDataPath SkyDataPath {..} =
  SkyDataPath
    (lcg $ pathDaMetaData)
    (fmap lcg pathTopicTriePath)
    (lcg pathTopicMetaData)
    (fmap lcg pathMessageTriePath)
    (lcg pathMessageMetaData) where
  lcg = LiftRef . castDigest . getDigest

-- * Meta Declarations
