{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Data.Utils where

import Data.Functor.Const
import Data.Functor.Identity

-- | Type of natural transformation between 'f' and 'g'
type (~>) f g = forall x. f x -> g x

-- | Typeclass for extractible wrappers 'w'
class (Functor w, Applicative w) => Wrapper w where
  unwrap :: w a -> a

  wrap :: a -> w a
  wrap = pure

instance Wrapper Identity where
  unwrap = runIdentity

-- Not sure if this can even be used
class (Wrapper w1, Wrapper w2) => Transform w1 w2 where
  transform :: w1 a -> w2 b

-- class (Functor f, Functor g) => NaturalTransformation f g where
--   alpha :: f a -> g a
