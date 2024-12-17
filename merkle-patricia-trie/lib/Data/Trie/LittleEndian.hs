{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Trie.LittleEndian where

import Data.Bits
import Data.Kind (Type)
import Data.Trie.Internal
import Data.WideWord (Word256)
import Data.Word (Word8)
import Prelude hiding (lookup)

newtype LEK a = LittleEndian a deriving (Enum, Eq, Ord, Real, Bits, Num, Integral)

newtype Trie k a = Trie {unTrie :: Trie' (LEK k) (TrieHeight k) a} deriving (Show, Eq, Functor)

instance (Bits k, Num k, Integral k) => TrieKey (LEK k) where
  mask (LittleEndian a) (LittleEndian b) = LittleEndian $ a .&. (b - 1)
  branchingBit (LittleEndian a) (LittleEndian b) = LittleEndian . lowestBit $ a `xor` b
    where
      lowestBit x = x .&. complement x
