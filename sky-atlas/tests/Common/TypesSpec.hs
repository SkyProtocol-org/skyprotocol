{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.TypesSpec where

import Common.Types
import Control.Exception
import Data.String (String)
import GHC.Base qualified as GB
import GHC.Show qualified as GS
import PlutusTx.Builtins
import PlutusTx.List
import PlutusTx.Prelude
import PlutusTx.Show qualified as PS
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- * Helpers

shouldBeHex :: (ToByteString a) => a -> GB.String -> Assertion
a `shouldBeHex` h =
  hexOf a @?= h

shouldBeHex2 :: (Eq a, ToByteString a, FromByteString a) => a -> String -> Assertion
a `shouldBeHex2` h = do
  a `shouldBeHex` h
  let b = ofHex h
  a == b @?= True
  hexOf b @?= h

genUInt :: Integer -> Gen Integer
genUInt len = choose (0, exponential 256 len - 1)

genByteString :: Integer -> Gen BuiltinByteString
genByteString n = genUInt n >>= return . integerToByteString LittleEndian n

toBytes4 :: Integer -> Bytes4
toBytes4 = fromInt

toBytes8 :: Integer -> Bytes8
toBytes8 = fromInt

-- * QuickCheck support

instance Arbitrary Byte where
  arbitrary = genUInt 1 >>= return . Byte

instance Arbitrary UInt16 where
  arbitrary = genUInt 2 >>= return . UInt16

instance Arbitrary UInt32 where
  arbitrary = genUInt 4 >>= return . UInt32

instance Arbitrary UInt64 where
  arbitrary = genUInt 8 >>= return . UInt64

instance Arbitrary Bytes4 where
  arbitrary = genByteString 4 >>= return . Bytes4

instance Arbitrary Bytes8 where
  arbitrary = genByteString 8 >>= return . Bytes8

instance Arbitrary BuiltinByteString where
  arbitrary = choose (0, 31) >>= genByteString

instance Exception String

-- * Tests

typesSpec :: TestTree
typesSpec =
  testGroup "SkyBase"
    $ [ testCase "builtins" $ do
          replicateByte 3 4 `shouldBeHex2` "040404"
          appendByteString (replicateByte 3 4) (replicateByte 2 3) `shouldBeHex2` "0404040303"
          integerToByteString BigEndian 6 0x1234567890 `shouldBeHex2` "001234567890"
          let fsb = findFirstSetBit . toByteString . toUInt32
          -- NOPE: logInfo @String "foo" `shouldBe` True
          fsb 0x8004 @?= 2
          fsb 0x00ff @?= 0
          fsb 0xff00 @?= 8
          fsb 0x00f0 @?= 4
          fsb 0x0000 @?= -1
          fsb 0xffffffff @?= 0
          fsb 0x80000000 @?= 31,
        testCase "ToByteString" $ do
          toByteString @[BuiltinByteString] [] @?= "\NUL\NUL",
        testCase "serialization 1" $ PS.show (Byte 42) @?= "42",
        testCase "serialization 2" $ do
          PS.show (UInt16 0xf00d) @?= "61453"
          PS.show (toUInt16 0xbad) @?= "2989"
          PS.show (maybeFromInt 0x11111 :: Maybe UInt16) @?= "Nothing",
        testCase "serialization 3" $ PS.show (toUInt32 0x10ffff) @?= "1114111",
        testCase "serialization 4" $ PS.show (fromInt 0x5678901234567890 :: Bytes8) @?= "5678901234567890",
        testCase "serialization 5" $ do
          let unicodeMax :: Integer
              unicodeMax = 0x10ffff
          unicodeMax == 1114111 @?= True
          PS.show unicodeMax @?= "1114111"
          (unicodeMax, 33 :: Integer) `shouldBeHex2` "000310ffff21",
        testCase "serialization 7"
          $
          -- \| Plutus doesn't quote not readably output its build
          PS.show (5 :: Integer, "23" :: BuiltinString, hexB "0badf00d")
          @?= "(5,\"23\",0badf00d)",
        testCase "serialization 8" $ do
          Byte 42 `shouldBeHex2` "2a"
          (ofHex "abcd" :: BuiltinByteString) `shouldBeHex` "abcd",
        testCase "multiplyByExponential" $ do
          multiplyByExponential 3 2 10 @?= 3072
          multiplyByExponential 2 3 5 @?= 486,
        testCase "exponential" $ do
          exponential 7 4 @?= 2401
          exponential 4 7 @?= 16384
          exponential 2 63 @?= 9223372036854775808
          exponential 3 42 @?= 109418989131512359209
          bitLength (exponential 3 42) @?= 67
          -- a Mersenne number, large enough to require Bignums, yet not too large either (66 bytes)
          let m521 = exponential 2 521 - 1
          PS.show m521 @?= "6864797660130609714981900799081393217269435300143305409394463459185543183397656052122559640661454554977296311391480858037121987999716643812574028291115057151"
          bitLength m521 @?= 521
          map (exponential 2) [0 .. 16]
            @?= [1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536],
        testCase "toByte" $ do
          toByte 0 `shouldBeHex2` "00"
          toByte 42 `shouldBeHex2` "2a"
          toByte 255 `shouldBeHex2` "ff",
        -- toByte 259 `shouldThrow` anyException
        -- toByte -10 `shouldThrow` anyException
        -- 000302020100016101000162030000000000000000000163
        testCase "toUInt16" $ do
          toUInt16 0 `shouldBeHex2` "0000"
          toUInt16 42 `shouldBeHex2` "002a"
          toUInt16 256 `shouldBeHex2` "0100"
          toUInt16 259 `shouldBeHex2` "0103"
          -- toUInt16 -10 `shouldThrow` anyException
          toUInt16 65535 `shouldBeHex2` "ffff",
        -- toUInt16 65536 `shouldThrow` anyException
        testCase "equalizeByteStringLength" $ do
          let e x y = (hexOf ex :: BuiltinString, hexOf ey :: BuiltinString) where (ex, ey) = equalizeByteStringLength (ofHex x) (ofHex y)
          e "abcd" "ef01" @?= ("abcd", "ef01")
          e "abcd" "" @?= ("abcd", "0000")
          e "ab" "ef01" @?= ("00ab", "ef01")
          e "1234567890" "00" @?= ("1234567890", "0000000000"),
        testCase "bitLength" $ do
          map bitLength2 [0, 1, 2, 3] @?= [0, 1, 2, 2]
          map bitLength4 [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15] @?= [0, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4]
          map bitLength8 [0, 3, 4, 15, 18, 42, 126, 255] @?= [0, 2, 3, 4, 5, 6, 7, 8]
          let nums32 = [0, 1, 2, 3, 4, 15, 18, 42, 126, 128, 255, 256, 1000, 0xffff, 0x12345, 0x1badf00d, 0xdeadbeef, 0xffffffff]
              bls32 = [0, 1, 2, 2, 3, 4, 5, 6, 7, 8, 8, 9, 10, 16, 17, 29, 32, 32]
              bli = bitLength :: Integer -> Integer
          map bitLength nums32 @?= bls32
          map (bitLength . toUInt32) nums32 @?= bls32
          map (bitLength . toBytes4) nums32 @?= bls32
          map bli [-1, -2, -3, -4, -5, -128, -129, -256, -257, -1000] @?= [0, 1, 2, 2, 3, 7, 8, 8, 9, 10]
          bli 1000000000000066600000000000001 @?= 100 -- Belphegor's Prime
      ]
    <> let lbci = [0, 1, 2, 3, 5, 7, 1151]
           lbco = [0, 1, 0, 2, 1, 3, 7]
           lbcin = [-1, -2, -5, -100, -1153]
           lbcon = [-1, 0, 2, 0, 7]
           lbbyte = [0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 6, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 7, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 6, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 8]
        in [ testCase "lowestBitClear 1.2" $ do
               map (lowestBitClear :: Integer -> Integer) lbci @?= lbco,
             testCase "lowestBitClear 1.4" $ do
               map (lowestBitClear :: Integer -> Integer) lbcin @?= lbcon,
             testCase "lowestBitClear 1.6" $ do
               map (lowestBitClear . toUInt64 :: Integer -> Integer) lbci @?= lbco,
             testCase "lowestBitClear 1.8" $ do
               map (lowestBitClear . toBytes4 :: Integer -> Integer) lbci @?= lbco,
             testCase "lowestBitClear 2" $ do
               map (lowestBitClear . toByte :: Integer -> Integer) [0 .. 255] @?= lbbyte,
             testCase "lowestBitClear 3" $ do
               map (lowestBitClear . toBytes4 . fromInt :: Integer -> Integer) [0 .. 255] @?= lbbyte,
             {-      map (\(n :: Integer) -> (n,
                                            exponential 2 0,
                                            n `quotient` 2,
                                            lowestBitClear n,
                                            isBitSet 0 n))
                       [0] @?= [] -}
             testBitLogic "Integer" fromInt toInt (-1 :: Integer) 16 False,
             testBitLogic "Bytes4" fromInt toInt (toBytes4 0xffffffff) 4 True,
             testBitLogic "UInt64" fromInt toInt (toUInt64 0xffffffffffffffff) 8 True,
             testBitLogic "Bytes8" fromInt toInt (toBytes8 0xffffffffffffffff) 8 True,
             testCase "shiftLeftWithBits" $ do
               (toBytes8 128 `shiftLeft` 56) `shouldBeHex2` "8000000000000000"
               PS.show (shiftLeftWithBits (toBytes8 1) 63 (fromInt 0)) @?= "8000000000000000"
           ]

testBitLogic ::
  (BitLogic a, FromInt a, Dato a, Eq a, GS.Show a, Arbitrary a) =>
  String ->
  (Integer -> a) ->
  (a -> Integer) ->
  a ->
  Integer ->
  Bool ->
  TestTree
testBitLogic typ i t allBits len isUnsigned =
  let nBits = len * 8
      -- bM = t $ lowBitsMask nBits
      maxi = i $ exponential 256 len - 1
      -- genA = genUInt len
      m1 = t maxi `quotient` 255
      rep b = i $ b * m1 -- or we could add a method using replicateByte for ByteString's
      r17 = rep 17
      aAnd x y = t $ i x `logicalAnd` i y
      aOr x y = t $ i x `logicalOr` i y
      aXor x y = t $ i x `logicalXor` i y
      aLowestBitClear n = lowestBitClear (i n)
      ebf l height bits = t $ extractBitField l height (i bits)
      itt x = testCase $ typ GB.++ " " GB.++ x
   in testGroup
        ("BitLogic " GB.++ typ)
        [ itt "bitLength" $ do
            bitLength (i 17) @?= 5
            bitLength r17 @?= (len * 8) - 3,
          itt "lowestBitClear" $ do
            aLowestBitClear 0 @?= 0
            aLowestBitClear 159 @?= 5
            lowestBitClear (maxi `shiftRight` (len * 2)) @?= len * 6
            lowestBitClear allBits @?= if isUnsigned then nBits else -1,
          itt "isBitSet" $ do
            isBitSet 5 (i 0) @?= False
            isBitSet 0 (i 5) @?= True
            isBitSet 2 (i 7) @?= True
            isBitSet 7 (i 2) @?= False,
          itt "logicalAnd" $ do
            240 `aAnd` 85 @?= 80
            42 `aAnd` 1 @?= 0,
          itt "logicalOr" $ do
            240 `aOr` 85 @?= 245
            42 `aOr` 1 @?= 43,
          itt "logicalXor" $ do
            240 `aXor` 85 @?= 165
            42 `aXor` 1 @?= 43,
          --    it "lowBitsMask" $ do
          let l = t . lowBitsMask :: Integer -> Integer
           in itt "lowBitMask" $ do
                map l [0, 1, 2, 3, 4, 7, 8, 16, 32]
                  @?= [0, 1, 3, 7, 15, 127, 255, 65535, 4294967295]
                (lowBitsMask 63 :: UInt64) @?= toUInt64 9223372036854775807,
          --    it "shiftRight" $ do
          --    it "shiftLeft" $ do
          itt "extractBitField" $ do
            ebf 3 4 37 @?= 2
            ebf 4 2 11 @?= 2
        ]

-- it "QuickCheck property: lowBitsMask and lowestBitClear" $
{- XXX debug that with Ya
      property $
        \ (n :: a) -> do
          let b = lowestBitClear n
          -1 <= b && b <= nBits `shouldBe` True
          0 <= b || (n == i 0 && not isUnsigned) `shouldBe` True
          b < nBits && isBitSet n b `shouldBe` False
          n `logicalOr` (allBits `logicalXor` lowBitsMask b) == allBits `shouldBe` True -}
