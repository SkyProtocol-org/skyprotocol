{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:preserve-logging #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}
{-# OPTIONS_GHC -O0 #-} -- don't optimize errors away

module Spec.CryptoSpec where

import Common.Types
import Common.Crypto
import Common.Trie
import Common.TypesSpec

import PlutusTx.Prelude
import qualified PlutusTx.Prelude as P
import PlutusTx
import PlutusTx.Builtins
import PlutusTx.Builtins.Internal (BuiltinString (..))
import PlutusTx.List (map, zip)
import qualified PlutusTx.Show as PS
import PlutusTx.Utils

import GHC.Base ((++))
import qualified GHC.Base as GB
import qualified GHC.Err as GE
import qualified GHC.Show as GS
import qualified Debug.Trace as DT

import Data.Bits (unsafeShiftL)
import Data.Functor.Identity (Identity (..))
import Data.String (String, IsString, fromString)
import Data.Text (pack, unpack)
import Test.Hspec
import Test.QuickCheck hiding ((.&.))
import Control.Exception
import Prelude (Int, putStrLn)

cryptoSpec = do
  describe "SkyCrypto" $ do
    it "simple tries" $ do
      let t1 = TrieTop 4 (2989 :: Integer)
      t1 `shouldBeHex2` "00050bad"
      PS.show t1 `shouldBe` "TrieTop 4 2989"
    it "simple hashes" $ do
      computeHash (hexB "") `shouldBeHex2` "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"
      computeHash (hexB "00") `shouldBeHex2` "03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314"
      computeHash (ofHex "abcd" :: UInt16) `shouldBeHex2` "9606e52f00c679e548b5155af5026f5af4130d7a15c990a791fff8d652c464f5"
