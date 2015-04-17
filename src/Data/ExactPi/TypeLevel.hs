{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.ExactPi.TypeLevel
where

import Data.ExactPi
import Data.Proxy
import Data.Ratio
import GHC.TypeLits

data ExactPi' = ExactPi' Nat Nat Nat

class KnownExactPi (v :: ExactPi') where
  exactPiVal :: Proxy v -> ExactPi

instance (KnownNat z, KnownNat p, KnownNat q) => KnownExactPi ('ExactPi' z p q) where
  exactPiVal _ = Exact z' (p' % q')
    where
      z' = natVal (Proxy :: Proxy z)
      p' = natVal (Proxy :: Proxy p)
      q' = natVal (Proxy :: Proxy q)