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

import Common.Crypto
import Common.DA
import Common.Types
import Common.Trie as T

-- For each topic we follow, a queue of moments at which to forget past data
newtype RetentionState = RetentionState
  { -- | Trie of (time, queue)
    topicRetentionQueues :: T.Trie Identity Byte TopicId (Q.MinQueue TopicRetentionMoment)
  } deriving (Generic)
  deriving (P.Eq, P.Show) via T.Trie Identity Byte TopicId (Q.MinQueue TopicRetentionMoment)

newtype TopicRetentionMoment = TopicRetentionMomentOfTuple
  { tupleOfTopicRetentionMoment :: (POSIXTime, MessageId)
  } deriving (Eq, Show, P.Eq, P.Show, ToByteString, FromByteString, P.ToData, P.FromData, P.UnsafeFromData) via (POSIXTime, MessageId)

pattern TopicRetentionMoment :: POSIXTime -> MessageId -> TopicRetentionMoment
pattern TopicRetentionMoment {topicRetentionTime, topicRetentionHeight} = TopicRetentionMomentOfTuple (topicRetentionTime, topicRetentionHeight)

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

-- TODO: at some point, make that depend on per-topic metadata
topicRetentionDurationInMilliseconds :: Integer
topicRetentionDurationInMilliseconds = oneDayInMilliseconds

oneDayInMilliseconds :: Integer
oneDayInMilliseconds = 24 * 60 * 1_000_000

topicRetentionCutoff :: POSIXTime -> Q.MinQueue TopicRetentionMoment -> (Maybe MessageId, Maybe (Q.MinQueue TopicRetentionMoment))
topicRetentionCutoff currentTime queue =
  case Q.getMin queue of
    Nothing -> (Nothing, Just queue)
    Just (TopicRetentionMoment mt mh) ->
      let isRetained t =
            getPOSIXTime t >= getPOSIXTime currentTime - topicRetentionDurationInMilliseconds
          loop h q =
            let qq = Q.deleteMin q in
              case Q.getMin qq of
                Nothing -> (Just h, Nothing)
                Just (TopicRetentionMoment mmt mmh) ->
                  if isRetained mmt
                  then (Just h, Just qq)
                  else loop mmh qq in
        if isRetained mt
        then (Nothing, Just queue)
        else loop mh queue

applyDataRetentionPolicy :: (LiftWrapping e r, LiftDato r, MaybeRef e (LiftRef r), LiftWrapping e Identity) =>
  POSIXTime -> SkyDa r -> RetentionState -> e (SkyDa r, RetentionState)
applyDataRetentionPolicy currentTime da rs = do
  let stt = skyTopicTrie da
  tA <- unwrap stt
  let tB = rs.topicRetentionQueues
  eA <- rf T.Empty
  eB <- rf T.Empty
  walkTriePair tA tB
    {-recurse-} id
    {-emptyCase-} (\ _ _ -> return (eA, eB))
    {-leafCase-} (\_ te@(ltmd, lmt) rq ->
                    let (mh, mq) = topicRetentionCutoff currentTime rq in
                      case mh of
                        Nothing -> do
                          a' <- rf $ Leaf te
                          b' <- rf $ Leaf rq
                          return (a', b')
                        Just h -> do
                          mt <- unwrap lmt
                          mt' <- forgetBefore h mt
                          lmt' <- wrap mt'
                          a' <- rf $ Leaf (ltmd, lmt')
                          b' <- case mq of
                                  Nothing -> return eB
                                  Just q -> rf $ Leaf q
                          return (a', b'))
    {-branchCase-} (\_ _ (la, lb) (ra, rb) -> do
                      a' <- makeBranch la ra
                      b' <- makeBranch lb rb
                      return (a', b'))
    {-skipCase-} (\_ _ hBits bits (a, b) -> do
                      a' <- stepUp (SkipStep hBits bits) a
                      b' <- stepUp (SkipStep hBits bits) b
                      return (a', b'))
    {-onlyACase-} (\_ _ a -> return (a, eB))
    {-onlyBCase-} (\_ _ b -> return (eA, b))
    {-finish-} (\h (a, b) -> do
                  ta' <- makeTop h a
                  stt' <- wrap ta'
                  rs' <- makeTop h b
                  return (da {skyTopicTrie=stt'}, RetentionState rs'))
