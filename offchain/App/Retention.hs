{-# OPTIONS_GHC -fno-warn-orphans #-}

module App.Retention where

import Data.Functor.Identity (Identity (..))
import Data.PQueue.Min as Q
import GHC.Generics

import PlutusLedgerApi.V1.Time (POSIXTime (..))
import qualified PlutusTx as P
import PlutusTx.Prelude ((<&>))
import qualified PlutusTx.Prelude as P
import qualified PlutusTx.Show as P

import Common.DA
import Common.Types
import Common.Trie as T

-- For each topic we follow, a queue of moments at which to forget past data
newtype RetentionState = RetentionState
  { -- | Trie of (time, height)
    topicRetentionQueues :: T.Trie Identity Byte TopicId (Q.MinQueue TopicRetentionMoment)
  } deriving (Generic)
  deriving (P.Eq, P.Show) via T.Trie Identity Byte TopicId (Q.MinQueue TopicRetentionMoment)

newtype TopicRetentionMoment = TopicRetentionMomentOfTuple
  { tupleOfTopicRetentionMoment :: (POSIXTime, MessageId)
  } deriving (Eq, Show, P.Eq, P.Show, ToByteString, FromByteString, P.ToData, P.FromData, P.UnsafeFromData) via (POSIXTime, MessageId)

pattern TopicRetentionMoment :: POSIXTime -> MessageId -> TopicRetentionMoment
pattern TopicRetentionMoment {h, t} = TopicRetentionMomentOfTuple (h, t)

{-# COMPLETE TopicRetentionMoment #-}

instance Ord TopicRetentionMoment where
  (TopicRetentionMoment m _) <= (TopicRetentionMoment m' _) = m <= m'

instance (ToByteString a, Ord a) => ToByteString (MinQueue a) where
  byteStringOut q = byteStringOut (Q.toList q)

instance (FromByteString a, Ord a) => FromByteString (MinQueue a) where
  byteStringIn isTerminal = byteStringIn isTerminal <&> Q.fromList

instance (P.ToData a, Ord a) => P.ToData (MinQueue a) where
  toBuiltinData = P.toBuiltinData . Q.toList

instance (P.FromData a, Ord a) => P.FromData (MinQueue a) where
  fromBuiltinData d = P.fromBuiltinData d <&> Q.fromList

instance (P.UnsafeFromData a, Ord a) => P.UnsafeFromData (MinQueue a) where
  unsafeFromBuiltinData = Q.fromList . P.unsafeFromBuiltinData

instance (P.Show a, Ord a) => P.Show (MinQueue a) where
  showsPrec p q = showApp p "Data.PQueue.Min.fromList" [showArg (Q.toList q)]

instance (P.Eq a, Ord a) => P.Eq (MinQueue a) where
  q == q' = Q.toAscList q == Q.toAscList q'

initialRetentionState :: RetentionState
initialRetentionState = runIdentity (T.empty <&> RetentionState)

topicRetentionDurationInMilliseconds :: LiftWrapping e r => SkyDa r -> TopicId -> e Integer
topicRetentionDurationInMilliseconds _ _ = return oneDayInMilliseconds

oneDayInMilliseconds :: Integer
oneDayInMilliseconds = 24 * 60 * 1_000_000

applyDataRetentionPolicy :: (LiftWrapping e r) =>
  POSIXTime -> SkyDa r -> RetentionState -> e (SkyDa r, RetentionState)
applyDataRetentionPolicy t da rs = do
  let rsl = runIdentity . T.listOf $ rs.topicRetentionQueues
  let ttz = da.skyTopicTrie
  -- XXX TODO -- IMPLEMENT IT XXX
  return (da, rs)
