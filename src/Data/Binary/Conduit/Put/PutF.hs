--
-- Copyright 2017 Warlock <internalmike@gmail.com>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--

-- | This module provides the 'PutF' functor,
-- and all functions, which could not be defined using 'PutF' public interface only.

module Data.Binary.Conduit.Put.PutF
  ( Encoding
  , encodingBytesGoingWrite
  , encodingGoingPut
  , PutF
  , runPutF
  , putF
  , PutM
  ) where

import qualified Data.ByteString as S (ByteString)
import Data.Conduit
import Data.Semigroup
import Data.Word

-- | 'PutF' wrapper state.
newtype Encoding = Encoding
  { encodingBytesGoingWrite :: Word64 -- ^ Get the total number of bytes going to write during encoding.
  } deriving Show

-- | Set the total number of bytes going to write during encoding.
encodingGoingPut :: Word64 -> Encoding
encodingGoingPut = Encoding

instance Monoid Encoding where
  mempty = Encoding 0
  {-# INLINE mempty #-}
  mappend (Encoding a) (Encoding b) = Encoding (a + b)
  {-# INLINE mappend #-}

-- | Monad wrapper for 'Put' with host monad @m@, and encoder result @a@ (usually @()@).
newtype PutF m a = F
  { runPutF :: (m a, Encoding) -- ^ Run a 'Put' functor, unwrapping all wrappers in a reversible way. @uncarry 'putF' . runPutF = 'id'@
  }

-- | Custom 'Put'.
-- @uncarry putF . 'runPutF' = 'id'@
putF :: m a -> Encoding -> PutF m a
putF x y = F (x, y)
{-# INLINE putF #-}

deriving instance Functor m => Functor (PutF m)
instance Applicative m => Applicative (PutF m) where
  pure a = F (pure a, mempty)
  {-# INLINE pure #-}
  F (f, f_enc) <*> F (x, x_enc) = F (f <*> x, f_enc `mappend` x_enc)
  {-# INLINE (<*>) #-}
instance Monad m => Semigroup (PutF m a) where
  F (a, a_enc) <> F (b, b_enc) = F (a >> b, a_enc `mappend` b_enc)
  {-# INLINE (<>) #-}
instance Monad m => Monoid (PutF m ()) where
  mempty = F (return (), mempty)
  {-# INLINE mempty #-}
  F (a, a_enc) `mappend` F (b, b_enc) = F (a >> b, a_enc `mappend` b_enc)
  {-# INLINE mappend #-}

-- | A 'ConduitM' with internal transformers supposed to a binary serialization.
type PutM i m = PutF (ConduitM i S.ByteString m)
