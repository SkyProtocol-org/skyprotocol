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
  , daCommittee :: LiftRef r Committee
  } deriving (Show, Eq)
type DaData r = TopicTrie r
type TopicTrie r = Trie64 r (TopicEntry r)
type TopicEntry r = (LiftRef r (TopicMetaData r), LiftRef r (MessageTrie r))
data TopicMetaData r = TopicMetaData
  { topicSchema :: DataHash
  , topicCommittee :: LiftRef r Committee
  } deriving (Show, Eq)
type MessageTrie r = Trie64 r (MessageEntry r)
type MessageEntry r = (LiftRef r (MessageMetaData r), LiftRef r (MessageData r))
data MessageMetaData (r :: Type -> Type) = MessageMetaData
  { messagePoster :: PubKey
  , messageTime :: POSIXTime
  } deriving (Show, Eq)
type MessageData (r :: Type -> Type) = VariableLengthByteString

-- TODO: In the future, turn MessageData into a Merkle Trie,
-- so we can publish it piecemeal on the chain and/or use ZK Proofs about it.
type Committee = MultiSigPubKey
type Trie64 r c = Trie r Byte Bytes8 c
type Trie64NodeRef r c = TrieNodeRef r Byte Bytes8 c
type Trie64Path t = TriePath Byte Bytes8 t

data SkyDataPath (r :: Type -> Type) = SkyDataPath
  { pathDaMetaData :: LiftRef r (DaMetaData r)
  , pathTopicTriePath :: Trie64Path (Trie64NodeRef r (TopicEntry r))
  , pathTopicMetaData :: LiftRef r (MessageMetaData r)
  , pathMessageTriePath :: Trie64Path (Trie64NodeRef r (MessageEntry r))
  , pathMessageMetaData :: LiftRef r (MessageMetaData r) }

type SkyDataProof = SkyDataPath Hash

-- * Instances

-- ** MessageMetaData
instance ToByteString (MessageMetaData r) where
  toByteString = toByteStringOut
instance ByteStringOut (MessageMetaData r) where
  byteStringOut = byteStringOut . tupleOfMessageMetaData
instance LiftDato r => Dato (MessageMetaData r) where
instance LiftByteStringIn r => ByteStringIn (MessageMetaData r) where
  byteStringIn = byteStringIn <&> uncurry MessageMetaData

-- ** TopicMetaData
instance LiftByteStringOut r => ToByteString (TopicMetaData r) where
  toByteString = toByteStringOut
instance LiftByteStringOut r => ByteStringOut (TopicMetaData r) where
  byteStringOut = byteStringOut . tupleOfTopicMetaData
instance LiftDato r => Dato (TopicMetaData r) where
instance LiftByteStringIn r => ByteStringIn (TopicMetaData r) where
  byteStringIn = byteStringIn <&> uncurry TopicMetaData

-- ** DaMetaData
instance LiftByteStringOut r => ToByteString (DaMetaData r) where
  toByteString = toByteStringOut
instance LiftByteStringOut r => ByteStringOut (DaMetaData r) where
  byteStringOut = byteStringOut . tupleOfDaMetaData
instance LiftDato r => Dato (DaMetaData r) where
instance LiftByteStringIn r => ByteStringIn (DaMetaData r) where
  byteStringIn = byteStringIn <&> uncurry DaMetaData

-- ** SkyDataPath
instance LiftDato r => ByteStringOut (SkyDataPath r) where
  byteStringOut = byteStringOut . tupleOfSkyDataPath
instance LiftByteStringIn r => ByteStringIn (SkyDataPath r) where
  byteStringIn = byteStringIn <&> uncurry5 SkyDataPath

-- ** Trie64
instance TrieHeight Byte where
instance TrieKey Bytes8 where
instance TrieHeightKey Byte Bytes8 where

-- ** SingleSig
instance ByteStringIn SingleSig where
  byteStringIn = byteStringIn <&> uncurry SingleSig

-- ** MultiSig
instance ByteStringIn MultiSig where
  byteStringIn = byteStringIn <&> MultiSig

-- ** MultiSigPubKey
instance Eq MultiSigPubKey where
  (MultiSigPubKey al an) == (MultiSigPubKey bl bn) = (al, an) == (bl, bn)
instance ByteStringOut MultiSigPubKey where
  byteStringOut = byteStringOut . tupleOfMultiSigPubKey
instance ToByteString MultiSigPubKey where
  toByteString = toByteStringOut
instance Show MultiSigPubKey where
  show (MultiSigPubKey l n) = "(MultiSigPubKey " <> show l <> " " <> show n <> ")"
instance Dato MultiSigPubKey where
instance ByteStringIn MultiSigPubKey where
  byteStringIn = byteStringIn <&> uncurry MultiSigPubKey

-- * Helpers
tupleOfMessageMetaData :: MessageMetaData r -> (PubKey, POSIXTime)
tupleOfMessageMetaData MessageMetaData {..} = (messagePoster, messageTime)
tupleOfTopicMetaData :: TopicMetaData r -> (DataHash, LiftRef r Committee)
tupleOfTopicMetaData TopicMetaData {..} = (topicSchema, topicCommittee)
tupleOfDaMetaData :: DaMetaData r -> (DataHash, LiftRef r Committee)
tupleOfDaMetaData DaMetaData {..} = (daSchema, daCommittee)
tupleOfSkyDataPath :: SkyDataPath r -> (LiftRef r (DaMetaData r), Trie64Path (Trie64NodeRef r (TopicEntry r)), LiftRef r (MessageMetaData r), Trie64Path (Trie64NodeRef r (MessageEntry r)), LiftRef r (MessageMetaData r))
tupleOfSkyDataPath SkyDataPath {..} = (pathDaMetaData, pathTopicTriePath, pathTopicMetaData, pathMessageTriePath, pathMessageMetaData)
tupleOfMultiSigPubKey :: MultiSigPubKey -> ([PubKey], UInt16)
tupleOfMultiSigPubKey MultiSigPubKey {..} = (multiSigPubKeyKeys, multiSigPubKeyThreshold)

-- TODO: generate a new Committee from PoS instead of just copying the PoA committee
-- Maybe it should depend on a seed based on the topicId and more.
generateCommittee :: (Monad e, Functor e, LiftWrapping e r, LiftDato r) => SkyDa r -> e (LiftRef r Committee)
generateCommittee (m, _) = unwrap m <&> daCommittee

-- TODO: add authentication, payment, etc., if not in this function, in one that wraps around it.
insertTopic :: (Monad e, Functor e, LiftWrapping e r, LiftDato r) =>
  DataHash -> SkyDa r -> e (Bytes8, SkyDa r)
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

{-
applySkyDataProof :: SkyDataProof -> DataHash -> DataHash
applySkyDataProof SkyDataProof {..} messageDataHash =

SkyDataZipper :: ()

getSkyDataProof :: Bytes8 -> Bytes8 -> SkyDa r -> SkyDataProof
getSkyDataProof =
-}

-- * Meta Declarations
