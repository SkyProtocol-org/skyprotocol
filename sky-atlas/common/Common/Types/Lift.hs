{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Common.Types.Lift where

import Common.Types.Base
import Common.Types.Cons()
import Control.Monad (Monad)
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import qualified Prelude as HP
import PlutusTx as P
import PlutusTx.Blueprint as P
import PlutusTx.Builtins as P
import PlutusTx.Prelude as P
import PlutusTx.Show as P

-- * Types

-- | Wrapper for (r a)
newtype LiftRef r a = LiftRef {liftref :: r a}

-- | Fixed-Point
data Fix f = Fix {getFix :: f (Fix f)}

-- * Typeclasses

type LiftDato r =
  ( LiftToByteString r,
    LiftFromByteString r,
    LiftToData r,
    LiftFromData r,
    LiftUnsafeFromData r,
    LiftShow r,
    LiftEq r)

class LiftEq r where
  liftEq :: (P.Eq a) => r a -> r a -> Bool

class LiftShow r where
  liftShowsPrec :: (P.Show a) => Integer -> r a -> P.ShowS

class LiftToByteString r where
  liftToByteString :: (Dato a) => r a -> P.BuiltinByteString
  liftToByteString a = liftByteStringOut a Terminal emptyByteString

  liftByteStringOut :: (Dato a) => r a -> IsTerminal -> P.BuiltinByteString -> P.BuiltinByteString
  liftByteStringOut a _ = appendByteString $ liftToByteString a

class LiftFromByteString r where
  liftFromByteString :: (FromByteString a) => P.BuiltinByteString -> r a
  liftFromByteString = fromByteStringIn_ liftByteStringIn

  liftByteStringIn :: (FromByteString a) => IsTerminal -> ByteStringReader (r a)

class LiftToData r where
  liftToBuiltinData :: (P.ToData a) => r a -> P.BuiltinData

class LiftUnsafeFromData r where
  liftUnsafeFromBuiltinData :: (P.UnsafeFromData a) => P.BuiltinData -> r a

class LiftFromData r where
  liftFromBuiltinData :: (P.FromData a) => P.BuiltinData -> Maybe (r a)

class LiftHasBlueprintSchema (r :: Type -> Type) where
  liftSchema :: (Proxy (r a), Schema referencedTypes) -> Schema referencedTypes

class (Monad e, Dato a) => PreWrapping e r a where
  wrap :: a -> e (r a)

-- | Wrapping : a value can be wrapped, and wrapped value that can be unwrapped
class (PreWrapping e r a) => Wrapping e r a where
  unwrap :: r a -> e a

class (Monad e) => LiftPreWrapping e r where
  liftWrap :: (Dato a) => a -> e (r a)

class (LiftPreWrapping e r) => LiftWrapping e r where
  liftUnwrap :: (Dato a) => r a -> e a

-- * Instances

instance LiftEq Identity where
--  liftEq (Identity a) (Identity b) = a == b
  liftEq = (==)

instance LiftShow Identity where
--  liftShowsPrec prec (Identity x) = showApp prec "Identity" [showArg x]
  liftShowsPrec = showsPrec

instance LiftToByteString Identity where
  liftByteStringOut = byteStringOut . runIdentity
  liftToByteString = toByteString . runIdentity

instance LiftFromByteString Identity where
  liftFromByteString = Identity . fromByteString
  liftByteStringIn it = byteStringIn it <&> Identity

instance LiftToData Identity where
  liftToBuiltinData = toBuiltinData . runIdentity

instance LiftFromData Identity where
  liftFromBuiltinData = fmap Identity . fromBuiltinData

instance LiftUnsafeFromData Identity where
  liftUnsafeFromBuiltinData = Identity . unsafeFromBuiltinData

instance LiftHasBlueprintSchema Identity where
  liftSchema (Proxy, s) = s

instance LiftPreWrapping Identity Identity where
  liftWrap = wrap

instance LiftWrapping Identity Identity where
  liftUnwrap = unwrap

-- ** Fix: Y-combinator or fixed point combinator for types

instance
  (LiftEq f) =>
  P.Eq (Fix f)
  where
  (==) x y = liftEq (getFix x) (getFix y)

instance
  (LiftEq f) =>
  HP.Eq (Fix f)
  where
  (==) = (P.==)

instance
  (LiftShow f) =>
  P.Show (Fix f)
  where
  showsPrec prec (Fix x) = showApp prec "Fix" [liftShowsPrec 11 x]

instance
  (LiftShow f) =>
  HP.Show (Fix f)
  where
  show = HP.show . P.show

instance
  (LiftDato f) =>
  ToByteString (Fix f)
  where
  toByteString = liftToByteString . getFix
  byteStringOut = liftByteStringOut . getFix

instance
  (LiftToData f) =>
  P.ToData (Fix f)
  where
  toBuiltinData = liftToBuiltinData . getFix

instance
  (LiftFromData f) =>
  P.FromData (Fix f)
  where
  fromBuiltinData d = Fix <$> liftFromBuiltinData d

instance
  (LiftUnsafeFromData f) =>
  P.UnsafeFromData (Fix f)
  where
  unsafeFromBuiltinData = liftUnsafeFromBuiltinData -. Fix

instance
  (LiftFromByteString f) =>
  FromByteString (Fix f)
  where
  fromByteString = Fix . liftFromByteString
  byteStringIn isTerminal = liftByteStringIn isTerminal <&> Fix

instance
  (LiftHasBlueprintSchema r) =>
  P.HasBlueprintSchema (Fix r) referencedTypes
  where
  schema = liftSchema @r (Proxy, schema @(Fix r))

-- ** LiftRef

instance (LiftEq r, P.Eq a) => P.Eq (LiftRef r a) where
  LiftRef x == LiftRef y = x `liftEq` y

instance (LiftEq r, P.Eq a) => HP.Eq (LiftRef r a) where
  (==) = (HP.==)

instance (LiftShow r, P.Show a) => P.Show (LiftRef r a) where
  showsPrec prec (LiftRef ra) = showApp prec "LiftRef" [liftShowsPrec 11 ra]

instance (LiftShow r, P.Show a) => HP.Show (LiftRef r a) where
  show = HP.show . P.show

instance (LiftToData r, P.ToData a) => P.ToData (LiftRef r a) where
  toBuiltinData = liftToBuiltinData . liftref

instance (LiftUnsafeFromData r, P.UnsafeFromData a) =>
  P.UnsafeFromData (LiftRef r a) where
  unsafeFromBuiltinData = LiftRef . liftUnsafeFromBuiltinData

instance (LiftFromData r, P.FromData a) =>
  P.FromData (LiftRef r a) where
  fromBuiltinData d = LiftRef <$> liftFromBuiltinData d

instance P.Functor r => P.Functor (LiftRef r) where
  fmap f (LiftRef x) = LiftRef (fmap f x)

instance
  (LiftToByteString r, Dato a) =>
  ToByteString (LiftRef r a)
  where
  toByteString = liftToByteString . liftref
  byteStringOut = liftByteStringOut . liftref

instance
  (LiftFromByteString r, FromByteString a) =>
  FromByteString (LiftRef r a)
  where
  byteStringIn isTerminal = liftByteStringIn isTerminal <&> LiftRef

instance
  (LiftPreWrapping e r, Dato a) =>
  PreWrapping e (LiftRef r) a
  where
  wrap a = do ra <- liftWrap a; return $ LiftRef ra

instance
  (LiftWrapping e r, Dato a) =>
  Wrapping e (LiftRef r) a
  where
  unwrap = liftUnwrap . liftref

instance
  (LiftHasBlueprintSchema r, P.HasBlueprintSchema a referencedTypes) =>
  P.HasBlueprintSchema (LiftRef r a) referencedTypes where
  schema = liftSchema @r (Proxy, schema @a)

-- ** (Pre)Wrapping

instance (Dato a) => PreWrapping Identity Identity a where
  wrap = Identity . Identity

instance (Dato a) => Wrapping Identity Identity a where
  unwrap x = x

