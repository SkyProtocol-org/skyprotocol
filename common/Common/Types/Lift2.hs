{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Common.Types.Lift2 where

import Common.Types.Base
import Common.Types.Cons ()
import Common.Types.Lift
import Control.Monad (Monad)
import qualified Prelude as HP
import PlutusTx as P
import PlutusTx.Blueprint as P
import PlutusTx.Builtins as P
import PlutusTx.Prelude as P
import PlutusTx.Show as P

-- * Types

-- | Wrapper for (r a)
newtype LiftRef2 r a b = LiftRef2 {liftref2 :: r a b}

-- | Fixed-Point
data Fix2 f a = Fix2 {getFix2 :: f a (Fix2 f a)}

-- * Typeclasses

type LiftDato2 r a =
  ( LiftToByteString2 r a,
    LiftFromByteString2 r a,
    LiftToData2 r a,
    LiftFromData2 r a,
    LiftUnsafeFromData2 r a,
    LiftShow2 r a,
    LiftEq2 r a)

class LiftEq2 r a where
  liftEq2 :: (P.Eq b) => r a b -> r a b -> Bool

class LiftShow2 r a where
  liftShowsPrec2 :: (P.Show b) => Integer -> r a b -> P.ShowS

class LiftToByteString2 r a where
  liftToByteString2 :: (Dato b) => r a b -> P.BuiltinByteString
  liftToByteString2 b = liftByteStringOut2 b Terminal emptyByteString

  liftByteStringOut2 :: (Dato b) => r a b -> IsTerminal -> P.BuiltinByteString -> P.BuiltinByteString
  liftByteStringOut2 b _ = appendByteString $ liftToByteString2 b

class LiftFromByteString2 r a where
  liftFromByteString2 :: (FromByteString b) => P.BuiltinByteString -> r a b
  liftFromByteString2 = fromByteStringIn_ liftByteStringIn2

  liftByteStringIn2 :: (FromByteString b) => IsTerminal -> ByteStringReader (r a b)

class LiftToData2 r a where
  liftToBuiltinData2 :: (P.ToData b) => r a b -> P.BuiltinData

class LiftUnsafeFromData2 r a where
  liftUnsafeFromBuiltinData2 :: (P.UnsafeFromData b) => P.BuiltinData -> r a b

class LiftFromData2 r a where
  liftFromBuiltinData2 :: (P.FromData b) => P.BuiltinData -> Maybe (r a b)

class LiftHasBlueprintSchema2 r a where
  liftSchema2 :: (Proxy (r a b), Schema referencedTypes) -> Schema referencedTypes

class (Monad e) => LiftPreWrapping2 e r a where
  liftWrap2 :: (Dato b) => b -> e (r a b)

class (LiftPreWrapping2 e r a) => LiftWrapping2 e r a where
  liftUnwrap2 :: (Dato b) => r a b -> e b

-- * Instances

-- ** Fix2: Y-combinator or fixed point combinator for types

instance
  (LiftEq2 f a) =>
  P.Eq (Fix2 f a)
  where
  (==) x y = liftEq2 (getFix2 x) (getFix2 y)

instance
  (LiftEq2 f a) =>
  HP.Eq (Fix2 f a)
  where
  (==) = (P.==)

instance
  (LiftShow2 f a) =>
  P.Show (Fix2 f a)
  where
  showsPrec prec (Fix2 x) = showApp prec "Fix2" [liftShowsPrec2 11 x]

instance
  (LiftShow2 f a) =>
  HP.Show (Fix2 f a)
  where
  show = HP.show . P.show

instance
  (LiftDato2 f a) =>
  ToByteString (Fix2 f a)
  where
  toByteString = liftToByteString2 . getFix2
  byteStringOut = liftByteStringOut2 . getFix2

instance
  (LiftToData2 f a) =>
  P.ToData (Fix2 f a)
  where
  toBuiltinData = liftToBuiltinData2 . getFix2

instance
  (LiftFromData2 f a) =>
  P.FromData (Fix2 f a)
  where
  fromBuiltinData d = Fix2 <$> liftFromBuiltinData2 d

instance
  (LiftUnsafeFromData2 f a) =>
  P.UnsafeFromData (Fix2 f a)
  where
  unsafeFromBuiltinData = liftUnsafeFromBuiltinData2 -. Fix2

instance
  (LiftFromByteString2 f a) =>
  FromByteString (Fix2 f a)
  where
  fromByteString = Fix2 . liftFromByteString2
  byteStringIn isTerminal = liftByteStringIn2 isTerminal <&> Fix2

instance
  (LiftHasBlueprintSchema2 r a) =>
  P.HasBlueprintSchema (Fix2 r a) referencedTypes
  where
  schema = liftSchema2 @r @a (Proxy, schema @(Fix2 r a))

-- ** LiftRef2

instance (LiftEq2 r a, P.Eq b) => P.Eq (LiftRef2 r a b) where
  LiftRef2 x == LiftRef2 y = x `liftEq2` y

instance (LiftEq2 r a, P.Eq b) => HP.Eq (LiftRef2 r a b) where
  (==) = (HP.==)

instance (LiftShow2 r a, P.Show b) => P.Show (LiftRef2 r a b) where
  showsPrec prec (LiftRef2 rab) = showApp prec "LiftRef2" [liftShowsPrec2 11 rab]

instance (LiftShow2 r a, P.Show b) => HP.Show (LiftRef2 r a b) where
  show = HP.show . P.show

instance (LiftToData2 r a, P.ToData b) => P.ToData (LiftRef2 r a b) where
  toBuiltinData = liftToBuiltinData2 . liftref2

instance (LiftUnsafeFromData2 r a, P.UnsafeFromData b) =>
  P.UnsafeFromData (LiftRef2 r a b) where
  unsafeFromBuiltinData = LiftRef2 . liftUnsafeFromBuiltinData2

instance (LiftFromData2 r a, P.FromData b) =>
  P.FromData (LiftRef2 r a b) where
  fromBuiltinData d = LiftRef2 <$> liftFromBuiltinData2 d

instance P.Functor (r a) => P.Functor (LiftRef2 r a) where
  fmap f (LiftRef2 x) = LiftRef2 (fmap f x)

instance
  (LiftToByteString2 r a, Dato b) =>
  ToByteString (LiftRef2 r a b)
  where
  toByteString = liftToByteString2 . liftref2
  byteStringOut = liftByteStringOut2 . liftref2

instance
  (LiftFromByteString2 r a, FromByteString b) =>
  FromByteString (LiftRef2 r a b)
  where
  byteStringIn isTerminal = liftByteStringIn2 isTerminal <&> LiftRef2

instance
  (LiftPreWrapping2 e r a, Dato b) =>
  PreWrapping e (LiftRef2 r a) b
  where
  wrap b = do rab <- liftWrap2 b; return $ LiftRef2 rab

instance
  (LiftWrapping2 e r a, Dato b) =>
  Wrapping e (LiftRef2 r a) b
  where
  unwrap = liftUnwrap2 . liftref2

instance
  (LiftHasBlueprintSchema2 r a, P.HasBlueprintSchema b referencedTypes) =>
  P.HasBlueprintSchema (LiftRef2 r a b) referencedTypes where
  schema = liftSchema2 @r @a (Proxy, schema @b)
