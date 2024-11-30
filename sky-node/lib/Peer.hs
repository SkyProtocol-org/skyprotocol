module Peer
  ( Peer (..),
  )
where

-- | Aux data structure to keep info about 'Peer'.
newtype Peer = Peer
  { -- | Id of the 'Peer'
    id :: String
  }
  deriving (Show)
