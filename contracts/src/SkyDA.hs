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
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module SkyDA where

import SkyBase
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
import GHC.Generics (Generic)

------------------------------------------------------------------------------
-- Core Data Types
------------------------------------------------------------------------------

-- List of data operators that must sign and minimum number of them that must sign
data MultiSigPubKey = MultiSigPubKey [PubKey] UInt16
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)
instance ByteStringIn MultiSigPubKey where
  byteStringIn = byteStringIn <&> \ (l, n) -> MultiSigPubKey l n
instance ByteStringOut MultiSigPubKey where
  byteStringOut (MultiSigPubKey l n) = byteStringOut (l, n)
instance ToByteString MultiSigPubKey where
  toByteString = toByteStringOut
instance Show MultiSigPubKey where
  show (MultiSigPubKey l n) = "(MultiSigPubKey " <> show l <> " " <> show n <> ")"

-- A single signature by a single data operator public key
data SingleSig = SingleSig PubKey Bytes64
  deriving (Eq)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)
instance ByteStringIn SingleSig where
  byteStringIn = byteStringIn <&> uncurry SingleSig

-- Signatures produced by data operators for top hash
data MultiSig = MultiSig [SingleSig]
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)
instance ByteStringIn MultiSig where
  byteStringIn = byteStringIn <&> MultiSig

{-
PlutusTx.makeIsDataSchemaIndexed ''Digest [('Digest, 0)]
PlutusTx.makeIsDataSchemaIndexed ''PubKey [('PubKey, 0)]
PlutusTx.makeIsDataSchemaIndexed ''MultiSigPubKey [('MultiSigPubKey, 0)]
PlutusTx.makeIsDataSchemaIndexed ''SingleSig [('SingleSig, 0)]
PlutusTx.makeIsDataSchemaIndexed ''MultiSig [('MultiSig, 0)]
PlutusTx.makeLift ''DataHash
PlutusTx.makeLift ''PubKey
PlutusTx.makeLift ''MultiSigPubKey
PlutusTx.makeLift ''SingleSig
PlutusTx.makeLift ''MultiSig
-}

-- SkyData
type SkyData = (Blake2b_256_Ref Committee, Blake2b_256_Ref TopicTrie)
type TopicTrie = Trie Blake2b_256_Ref Byte Bytes8 TopicEntry
type TopicEntry = (Blake2b_256_Ref TopicMetaData, Blake2b_256_Ref MessageTrie)
type MessageTrie = Trie Blake2b_256_Ref Byte Bytes8 MessageEntry
type MessageEntry = (Blake2b_256_Ref MessageMetaData, Blake2b_256_Ref MessageData)

instance Dato (Blake2b_256_Ref TopicTrie) where
instance Dato (Blake2b_256_Ref MessageTrie) where
instance Dato (Blake2b_256_Ref TopicMetaData) where

-- TODO: In the future, turn MessageData into a Merkle Trie,
-- so we can publish it piecemeal on the chain and/or use ZK Proofs about it.
type Committee = MultiSigPubKey
type TopicMetaData = Committee

data MessageMetaData = MessageMetaData
  { -- messageId, topicId :: Bytes64
    messagePoster :: PubKey
  , timePosted :: POSIXTime
    -- data about who can publish now?
  } deriving (Eq, Show)
instance ByteStringOut MessageMetaData where
  byteStringOut MessageMetaData {..}  = byteStringOut (messagePoster, timePosted)
instance ToByteString MessageMetaData where
  toByteString = toByteStringOut
instance Dato MessageMetaData where
instance Dato (Blake2b_256_Ref MessageMetaData) where

type MessageData = VariableLengthByteString
instance Dato (Blake2b_256_Ref MessageData) where

-- MerkleProof for metadata and data in Sky
type Trie64Proof = TrieProof Byte Bytes8 Blake2b_256
instance Dato Byte where
instance TrieHeight Byte where
instance Dato Bytes8 where
instance TrieKey Bytes8 where
instance TrieHeightKey Byte Bytes8 where

-- Chain metadata, Path to Topic, Topic metadata, Path to Message, Message metadata, Message
data SkyDataProof = SkyDataProof
  { skyMessageMetaDataHash :: DataHash
  , skyMessageInTopicProof :: Trie64Proof
  , skyTopicMetaDataHash :: DataHash
  , skyTopicInDaProof :: Trie64Proof
  , skyDaMetaDataHash :: DataHash }
  deriving (Eq, Show)
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)
instance ByteStringIn SkyDataProof where
  byteStringIn = byteStringIn <&> uncurry5 SkyDataProof

{-# INLINEABLE applySkyDataProof #-}
applySkyDataProof :: SkyDataProof -> DataHash -> DataHash
applySkyDataProof SkyDataProof {..} messageDataHash =
  runIdentity $ do
     let messageLeaf :: TrieNodeF Byte Bytes8 (DataHash, DataHash) ()
         messageLeaf = Leaf (skyMessageMetaDataHash, messageDataHash)
     let messageLeafHash = computeDigest messageLeaf :: DataHash
     topicDataHash <- applyMerkleProof messageLeafHash skyMessageInTopicProof
     let topicLeaf = Leaf (skyTopicMetaDataHash, topicDataHash) :: TrieNodeF Byte Bytes8 (DataHash, DataHash) ()
     let topicLeafHash = computeDigest topicLeaf :: DataHash
     daDataHash <- applyMerkleProof topicLeafHash skyTopicInDaProof
     return . computeDigest $ (skyDaMetaDataHash, daDataHash)


