{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Effect.SocketEff where

import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.TH (makeEffect)
import qualified Network.Socket as S

data SocketEff :: Effect where
  RecvCommand :: SocketEff m ()
  SendCommand :: SocketEff m ()

-- TODO replace with makeEffect_ and add docs for type sigs, or add docs to the GADT
makeEffect ''SocketEff

runSocketEffIO :: (IOE :> es) => S.Socket -> Eff (SocketEff : es) a -> Eff es a
runSocketEffIO sock = interpret $ \_ -> \case
  RecvCommand -> undefined
  SendCommand -> undefined
