{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Trie.BigEndian where

import Data.Bits
import Data.Kind (Type)
import Data.Trie.Internal
import Data.WideWord (Word256)
import Data.Word (Word8)
import Prelude hiding (lookup)

newtype BEK a = BigEndian a deriving (Enum, Eq, Ord, Real, Bits, Num, Integral)

newtype Trie k a = Trie {unTrie :: Trie' (BEK k) (TrieHeight k) a} deriving (Show, Eq, Functor)

instance (Bits k, Num k, Integral k) => TrieKey (BEK k) where
  mask (BigEndian a) (BigEndian b) = BigEndian $ (a .|. (b - 1)) .&. complement b

  -- TODO figure out how to properly implement branchingBit for BigEndian
  -- since it needs more args than just a and b(it also needs prefixes)
  branchingBit _ _ = undefined
