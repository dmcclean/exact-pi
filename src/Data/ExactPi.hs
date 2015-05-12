{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Kaos.Math.AugmentedRational
Description : Exact rational multiples of powers of pi
License     : MIT
Maintainer  : douglas.mcclean@gmail.com
Stability   : experimental

This type is sufficient to exactly express the closure of Q ∪ {π} under multiplication and division.
As a result it is useful for representing conversion factors
between physical units. Approximate values are included both to close the remainder
of the arithmetic operations in the `Num` typeclass and to encode conversion
factors defined experimentally.
-}
module Data.ExactPi
(
  ExactPi(..),
  approximateValue,
  isExactZero
)
where

import Data.Monoid
import Data.Group
import Prelude

-- | Represents an exact or approximate real value.
-- The exactly representable values are rational multiples of an integer power of pi.
data ExactPi = Exact Integer Rational -- ^ @'Exact' z q@ = q * pi^z. Note that this means there are many representations of zero.
             | Approximate (forall a.Floating a => a) -- ^ An approximate value. This representation was chosen because it allows conversion to floating types using their native definition of 'pi'.

-- | Approximates an exact or approximate value, converting it to a `Floating` type.
-- This uses the value of `pi` supplied by the destination type, to provide the appropriate
-- precision.
approximateValue :: Floating a => ExactPi -> a
approximateValue (Exact z q) = (pi ^ z) * (fromRational q)
approximateValue (Approximate x) = x

-- | Identifies whether an 'ExactPi' is an exact representation of zero.
isExactZero :: ExactPi -> Bool
isExactZero (Exact _ 0) = True
isExactZero _ = False

instance Show ExactPi where
  show (Exact z q) | z == 0 = "Exactly " ++ show q
                   | z == 1 = "Exactly pi * " ++ show q
                   | otherwise = "Exactly pi^" ++ show z ++ " * " ++ show q
  show (Approximate x) = "Approximately " ++ show (x :: Double)

instance Num ExactPi where
  fromInteger n = Exact 0 (fromInteger n)
  (Exact z1 q1) * (Exact z2 q2) = Exact (z1 + z2) (q1 * q2)
  (Exact _ 0) * _ = 0
  _ * (Exact _ 0) = 0
  x * y = Approximate $ approximateValue x * approximateValue y
  (Exact z1 q1) + (Exact z2 q2) | z1 == z2 = Exact z1 (q1 + q2) -- by distributive property
  x + y = Approximate $ approximateValue x + approximateValue y
  abs (Exact z q) = Exact z (abs q)
  abs (Approximate x) = Approximate $ abs x
  signum (Exact _ q) = Exact 0 (signum q)
  signum (Approximate x) = Approximate $ signum x -- we leave this tagged as approximate because we don't know "how" approximate the input was. a case could be made for exact answers here.
  negate x = (-1) * x

instance Fractional ExactPi where
  fromRational = Exact 0
  recip (Exact z q) = Exact z (recip q)

instance Floating ExactPi where
  pi = Exact 1 1
  exp x | isExactZero x = 1
        | otherwise = approx1 exp x
  log (Exact 0 1) = 0
  log x = approx1 log x
  -- It would be possible to give tighter bounds to the trig functions, preserving exactness for arguments that have an exactly representable result.
  sin = approx1 sin
  cos = approx1 cos
  tan = approx1 tan
  asin = approx1 asin
  atan = approx1 atan
  acos = approx1 acos
  sinh = approx1 sinh
  cosh = approx1 cosh
  tanh = approx1 tanh
  asinh = approx1 asinh
  acosh = approx1 acosh
  atanh = approx1 atanh

approx1 :: (forall a.Floating a => a -> a) -> ExactPi -> ExactPi
approx1 f x = Approximate (f (approximateValue x))

-- | The multiplicative monoid over augmented rationals.
instance Monoid ExactPi where
  mempty = 1
  mappend = (*)

-- | The multiplicative group over augmented rationals.
instance Group ExactPi where
  invert = recip

instance Abelian ExactPi