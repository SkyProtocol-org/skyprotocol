module Peer
  ( Peer (..),
  )
where

import qualified Data.ByteString as BS

-- | Aux data structure to keep info about 'Peer'.
newtype Peer = Peer
  { -- | Id of the 'Peer'
    id :: BS.ByteString
  }
  deriving (Show, Eq)
