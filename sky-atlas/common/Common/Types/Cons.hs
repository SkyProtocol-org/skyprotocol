{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Common.Types.Cons where

import Common.Types.Base
import Data.Functor.Identity (Identity (..))
import PlutusTx as P
import PlutusTx.Builtins as P
import PlutusTx.Prelude as P
import PlutusTx.Show as P

-- * Instances

-- ** Unit

instance ToByteString () where
  toByteString () = emptyByteString
  byteStringOut () _ s = s

instance FromByteString () where
  -- XXX  {-# INLINEABLE fromByteString #-}
  -- XXX  fromByteString = fromJust . maybeFromByteString -- repeat default to make Plutus happy
  byteStringIn _ = return ()

-- ** Sums and Tuples

-- *** Either

instance (ToByteString a, ToByteString b) =>
  ToByteString (P.Either a b) where
  byteStringOut (P.Left a) isTerminal =
    byteStringOut (Byte 0) NonTerminal . byteStringOut a isTerminal
  byteStringOut (P.Right b) isTerminal =
    byteStringOut (Byte 1) NonTerminal . byteStringOut b isTerminal

instance (FromByteString a, FromByteString b) =>
  FromByteString (Either a b) where
  byteStringIn isTerminal =
    byteStringIn NonTerminal >>= \b ->
      if b == Byte 0
        then byteStringIn isTerminal <&> Left
        else
          if b == Byte 1
            then byteStringIn isTerminal <&> Right
            else byteStringReaderFail

-- *** Maybe

instance (ToByteString a) => ToByteString (Maybe a) where
  byteStringOut Nothing _ = byteStringOut (Byte 0) NonTerminal
  byteStringOut (Just a) isTerminal = byteStringOut (Byte 1) NonTerminal . byteStringOut a isTerminal

instance (FromByteString a) => FromByteString (Maybe a) where
  byteStringIn isTerminal =
    byteStringIn NonTerminal >>= \b ->
      if b == Byte 0
        then return Nothing
        else
          if b == Byte 1
            then byteStringIn isTerminal <&> Just
            else byteStringReaderFail

-- *** (,) or builtin Pairs

instance (ToByteString a, ToByteString b) => ToByteString (a, b) where
  byteStringOut (a, b) isTerminal s =
    byteStringOut a NonTerminal
      $ byteStringOut b isTerminal s

instance (FromByteString a, FromByteString b) => FromByteString (a, b) where
  byteStringIn isTerminal =
    byteStringIn NonTerminal >>= \a ->
      byteStringIn isTerminal >>= \b ->
        return (a, b)

-- *** (,,) or builtin Triplets

instance (P.Eq a, P.Eq b, P.Eq c) => P.Eq (a, b, c) where
  (a, b, c) == (a', b', c') = a == a' && b == b' && c == c'

instance (ToByteString a, ToByteString b, ToByteString c) =>
  ToByteString (a, b, c) where
  byteStringOut (a, b, c) isTerminal s =
    byteStringOut a NonTerminal
      $ byteStringOut b NonTerminal
      $ byteStringOut c isTerminal s

instance (FromByteString a, FromByteString b, FromByteString c) =>
  FromByteString (a, b, c) where
  byteStringIn isTerminal =
    byteStringIn NonTerminal >>= \a ->
      byteStringIn NonTerminal >>= \b ->
        byteStringIn isTerminal >>= \c ->
          return (a, b, c)

-- *** (,,,) or builtin Quadruplets

instance (P.Eq a, P.Eq b, P.Eq c, P.Eq d) => P.Eq (a, b, c, d) where
  (a, b, c, d) == (a', b', c', d') = a == a' && b == b' && c == c' && d == d'

instance (ToByteString a, ToByteString b, ToByteString c, ToByteString d) =>
  ToByteString (a, b, c, d) where
  byteStringOut (a, b, c, d) isTerminal s =
    byteStringOut a NonTerminal
      $ byteStringOut b NonTerminal
      $ byteStringOut c NonTerminal
      $ byteStringOut d isTerminal s

instance (FromByteString a, FromByteString b, FromByteString c, FromByteString d) =>
  FromByteString (a, b, c, d) where
  byteStringIn isTerminal =
    byteStringIn NonTerminal >>= \a ->
      byteStringIn NonTerminal >>= \b ->
        byteStringIn NonTerminal >>= \c ->
          byteStringIn isTerminal >>= \d ->
            return (a, b, c, d)

-- *** (,,,,) or builtin Quintuplets

instance (P.Eq a, P.Eq b, P.Eq c, P.Eq d, P.Eq e) =>
  P.Eq (a, b, c, d, e) where
  (a, b, c, d, e) == (a', b', c', d', e') =
    a == a' && b == b' && c == c' && d == d' && e == e'

instance
  (ToByteString a, ToByteString b, ToByteString c, ToByteString d, ToByteString e) =>
  ToByteString (a, b, c, d, e) where
  byteStringOut (a, b, c, d, e) isTerminal s =
    byteStringOut a NonTerminal
      $ byteStringOut b NonTerminal
      $ byteStringOut c NonTerminal
      $ byteStringOut d NonTerminal
      $ byteStringOut e isTerminal s

instance
  (FromByteString a, FromByteString b, FromByteString c, FromByteString d, FromByteString e) =>
  FromByteString (a, b, c, d, e) where
  byteStringIn isTerminal =
    byteStringIn NonTerminal >>= \a ->
      byteStringIn NonTerminal >>= \b ->
        byteStringIn NonTerminal >>= \c ->
          byteStringIn NonTerminal >>= \d ->
            byteStringIn isTerminal >>= \e ->
              return (a, b, c, d, e)

instance (P.ToData a, P.ToData b, P.ToData c, P.ToData d, P.ToData e) =>
  P.ToData (a, b, c, d, e) where
  toBuiltinData (a, b, c, d, e) = toBuiltinData [toBuiltinData a, toBuiltinData b, toBuiltinData c, toBuiltinData d, toBuiltinData e]

instance (P.FromData a, P.FromData b, P.FromData c, P.FromData d, P.FromData e) =>
  P.FromData (a, b, c, d, e) where
  fromBuiltinData x =
    fromBuiltinData x >>= \case
      [aa, bb, cc, dd, ee] -> do
        a <- fromBuiltinData aa
        b <- fromBuiltinData bb
        c <- fromBuiltinData cc
        d <- fromBuiltinData dd
        e <- fromBuiltinData ee
        return (a, b, c, d, e)
      _ -> failNow

instance
  (P.UnsafeFromData a, P.UnsafeFromData b, P.UnsafeFromData c, P.UnsafeFromData d, P.UnsafeFromData e) =>
  P.UnsafeFromData (a, b, c, d, e) where
  unsafeFromBuiltinData x =
    case unsafeFromBuiltinData x of
      [a, b, c, d, e] -> (unsafeFromBuiltinData a, unsafeFromBuiltinData b, unsafeFromBuiltinData c, unsafeFromBuiltinData d, unsafeFromBuiltinData e)
      _ -> failNow

-- *** (,,,,,) or builtin Sextuplets

instance (P.Eq a, P.Eq b, P.Eq c, P.Eq d, P.Eq e, P.Eq f) =>
  P.Eq (a, b, c, d, e, f) where
  (a, b, c, d, e, f) == (a', b', c', d', e', f') =
    a == a' && b == b' && c == c' && d == d' && e == e' && f == f'

instance
  (ToByteString a, ToByteString b, ToByteString c, ToByteString d, ToByteString e, ToByteString f) =>
  ToByteString (a, b, c, d, e, f) where
  byteStringOut (a, b, c, d, e, f) isTerminal s =
    byteStringOut a NonTerminal
      $ byteStringOut b NonTerminal
      $ byteStringOut c NonTerminal
      $ byteStringOut d NonTerminal
      $ byteStringOut e NonTerminal
      $ byteStringOut f isTerminal s

instance
  (FromByteString a, FromByteString b, FromByteString c, FromByteString d, FromByteString e, FromByteString f) =>
  FromByteString (a, b, c, d, e, f) where
  byteStringIn isTerminal =
    byteStringIn NonTerminal >>= \a ->
      byteStringIn NonTerminal >>= \b ->
        byteStringIn NonTerminal >>= \c ->
          byteStringIn NonTerminal >>= \d ->
            byteStringIn NonTerminal >>= \e ->
              byteStringIn isTerminal >>= \f ->
                return (a, b, c, d, e, f)

instance
  (P.ToData a, P.ToData b, P.ToData c, P.ToData d, P.ToData e, P.ToData f) =>
  P.ToData (a, b, c, d, e, f) where
  toBuiltinData (a, b, c, d, e, f) = toBuiltinData [toBuiltinData a, toBuiltinData b, toBuiltinData c, toBuiltinData d, toBuiltinData e, toBuiltinData f]

instance
  (P.FromData a, P.FromData b, P.FromData c, P.FromData d, P.FromData e, P.FromData f) =>
  P.FromData (a, b, c, d, e, f) where
  fromBuiltinData x =
    fromBuiltinData x >>= \case
      [aa, bb, cc, dd, ee, ff] -> do
        a <- fromBuiltinData aa
        b <- fromBuiltinData bb
        c <- fromBuiltinData cc
        d <- fromBuiltinData dd
        e <- fromBuiltinData ee
        f <- fromBuiltinData ff
        return (a, b, c, d, e, f)
      _ -> failNow

instance
  (P.UnsafeFromData a, P.UnsafeFromData b, P.UnsafeFromData c, P.UnsafeFromData d, P.UnsafeFromData e, P.UnsafeFromData f) =>
  P.UnsafeFromData (a, b, c, d, e, f) where
  unsafeFromBuiltinData x =
    case unsafeFromBuiltinData x of
      [a, b, c, d, e, f] -> (unsafeFromBuiltinData a, unsafeFromBuiltinData b, unsafeFromBuiltinData c, unsafeFromBuiltinData d, unsafeFromBuiltinData e, unsafeFromBuiltinData f)
      _ -> failNow

-- ** Lists

instance (ToByteString a) => ToByteString [a] {- length limit 65535. -} where
  byteStringOut l isTerminal s =
    byteStringOut (toUInt16 $ length l) NonTerminal $ loop s l
    where
      loop bs [] = bs
      loop bs [a] = byteStringOut a isTerminal bs
      loop bs (a : l') = byteStringOut a NonTerminal (loop bs l')

instance (FromByteString a) => FromByteString [a] {- length limit 65535 -} where
  byteStringIn isTerminal = byteStringIn NonTerminal >>= \(UInt16 len) -> loop len
    where
      loop n =
        if n == 0
          then return []
          else byteStringIn (if n == 1 then isTerminal else NonTerminal) >>= \a -> loop (n - 1) <&> (a :)

-- ** Identity

instance (P.Eq a) => P.Eq (Identity a) where
  x == y = runIdentity x == runIdentity y

instance (P.Show a) => P.Show (Identity a) where
  showsPrec prec (Identity x) = showApp prec "Identity" [showArg x]

instance (ToByteString a) => ToByteString (Identity a) where
  toByteString = toByteString . runIdentity
  byteStringOut = byteStringOut . runIdentity

instance (FromByteString a) => FromByteString (Identity a) where
  -- fromByteString = Identity . fromByteString
  byteStringIn isTerminal = byteStringIn isTerminal <&> Identity

-- *** Sums and Tuples

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 f (a, b, c, d, e) = f a b c d e

uncurry6 :: (a -> b -> c -> d -> e -> f -> g) -> (a, b, c, d, e, f) -> g
uncurry6 g (a, b, c, d, e, f) = g a b c d e f

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)

curry4 :: ((a, b, c, d) -> e) -> a -> b -> c -> d -> e
curry4 f a b c d = f (a, b, c, d)

curry5 :: ((a, b, c, d, e) -> f) -> a -> b -> c -> d -> e -> f
curry5 f a b c d e = f (a, b, c, d, e)

curry6 :: ((a, b, c, d, e, f) -> g) -> a -> b -> c -> d -> e -> f -> g
curry6 g a b c d e f = g (a, b, c, d, e, f)


failNow :: a
failNow = traceError "FOO"

-- Plutus refuses to compile noReturn = noReturn
-- which isn't because it won't loop forever at times when not asked!
noReturn :: a -> a
noReturn = noReturn

trace' :: (P.Show s) => s -> e -> e
trace' s = trace (show s)

{-
trace1 :: (Show s, Show a, Show r) => s -> (a -> r) -> a -> r
trace1 s f a = trace (">> " ++ show s ++ " " ++ show a) $
  let r = f a in trace ("<< " ++ show s ++ " " ++ show r) r
trace2 :: (Show s, Show a, Show b, Show r) => s -> (a -> b -> r) -> a -> b -> r
trace2 s f a b = trace (">> " ++ show s ++ " " ++ show a ++ " " ++ show b) $
  let r = f a b in trace ("<< " ++ show s ++ " " ++ show r) r
trace3 :: (Show s, Show a, Show b, Show c, Show r) => s -> (a -> b -> c -> r) -> a -> b -> c -> r
trace3 s f a b c = trace (">> " ++ show s ++ " " ++ show a ++ " " ++ show b ++ " " ++ show c) $
  let r = f a b c in trace ("<< " ++ show s ++ " " ++ show r) r
etrace1 :: (Monad e, Show s, Show a, Show r) => s -> (a -> e r) -> a -> e r
etrace1 s f a = trace (">> " ++ show s ++ " " ++ show a) $
  f a >>= \ r -> trace ("<< " ++ show s ++ " " ++ show r) $ return r
etrace2 :: (Monad e, Show s, Show a, Show b, Show r) => s -> (a -> b -> e r) -> a -> b -> e r
etrace2 s f a b = trace (">> " ++ show s ++ " " ++ show a ++ " " ++ show b) $
  f a b >>= \ r -> trace ("<< " ++ show s ++ " " ++ show r) $ return r
etrace3 :: (Monad e, Show s, Show a, Show b, Show c, Show r) => s -> (a -> b -> c -> e r) -> a -> b -> c -> e r
etrace3 s f a b c = trace (">> " ++ show s ++ " " ++ show a ++ " " ++ show b ++ " " ++ show c) $
  f a b c >>= \ r -> trace ("<< " ++ show s ++ " " ++ show r) $ return r
-}

-- * Meta declarations
