module Utils where

import Effectful
import qualified Effectful.Reader.Dynamic as D
import qualified Effectful.Reader.Static as S

-- | Utility class to abstract over (Reader rec) environment.
-- Allows for easier refactoring, since you can constrain what functions need what from the environment.
class Has field rec where
  getField :: rec -> field

askFieldS :: (Has field env, S.Reader env :> es) => Eff es field
askFieldS = S.asks getField

askFieldD :: (Has field env, D.Reader env :> es) => Eff es field
askFieldD = D.asks getField
