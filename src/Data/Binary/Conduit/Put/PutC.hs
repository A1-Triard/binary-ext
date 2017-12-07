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

-- | This module provides the 'PutC' monad transformer,
-- and all functions, which could not be defined using 'PutC' public interface only.

module Data.Binary.Conduit.Put.PutC
  ( Encoding
  , startEncoding
  , encodingBytesWrote
  , encodingPut
  , PutC
  , ByteChunk
  , Put
  , runPutC
  , putC
  , putChunk
  , putChunkOr
  ) where

#include <haskell>

import Data.Binary.Conduit.Base

-- | 'PutC' monad state.
newtype Encoding = Encoding
  { encodingBytesWrote :: Word64 -- ^ Get the total number of bytes wrote to this point.
  } deriving Show

-- | Construct 'PutC' initial state.
startEncoding :: Word64 -> Encoding
startEncoding !bytes_read_before = Encoding { encodingBytesWrote = bytes_read_before }
{-# INLINE startEncoding #-}

-- | Modify 'PutC' state: mark byte string @inp@ as wrote.
-- See 'putC' for usage example.
encodingPut :: Word64 -> Encoding -> Encoding
encodingPut !bytes_count !s = Encoding{ encodingBytesWrote = encodingBytesWrote s + bytes_count }
{-# INLINE encodingPut #-}

-- | Internal transformers for 'Put' with error type @e@, host monad @m@ and encoder result @a@.
newtype PutC m a = C { runC :: StateT Encoding m a }

instance MonadTrans PutC where
  lift = C . lift
  {-# INLINE lift #-}
deriving instance Monad m => Monad (PutC m)
deriving instance Functor m => Functor (PutC m)
deriving instance MonadFix m => MonadFix (PutC m)
deriving instance MonadFail m => MonadFail (PutC m)
deriving instance MonadError e m => MonadError e (PutC m)
deriving instance (Functor m, Monad m) => Applicative (PutC m)
deriving instance MonadIO m => MonadIO (PutC m)
deriving instance (Functor m, MonadPlus m) => Alternative (PutC m)
deriving instance MonadPlus m => MonadPlus (PutC m)

instance MonadTransControl (PutC) where
  type StT (PutC) a = StT (StateT Encoding) a
  liftWith = defaultLiftWith C runC
  {-# INLINE liftWith #-}
  restoreT = defaultRestoreT C
  {-# INLINE restoreT #-}

instance MonadBase b m => MonadBase b (PutC m) where
  liftBase = liftBaseDefault
  {-# INLINE liftBase #-}

instance MonadBaseControl b m => MonadBaseControl b (PutC m) where
  type StM (PutC m) a = ComposeSt (PutC) m a
  liftBaseWith = defaultLiftBaseWith
  {-# INLINE liftBaseWith #-}
  restoreM = defaultRestoreM
  {-# INLINE restoreM #-}

-- | A 'ConduitM' with internal transformers supposed to a binary serialization.
type Put i m = ConduitM i ByteChunk (PutC m)

instance MonadBase b m => MonadBase b (Put i m) where
  liftBase = liftBaseDefault
  {-# INLINE liftBase #-}

-- | Run a 'Put' monad, unwrapping all internal transformers in a reversible way.
-- @'putC' . 'flip' runPutC = 'id'@
runPutC :: Monad m => Encoding -> Put i m a -> ConduitM i S.ByteString m (a, Encoding)
runPutC !encoding = runStateC encoding . transPipe runC . mapOutput bs
{-# INLINE runPutC #-}

-- | Custom 'Put'.
-- @putC . 'flip' 'runPutC' = 'id'@
putC :: Monad m => (Encoding -> ConduitM i S.ByteString m (a, Encoding)) -> Put i m a
putC = mapOutput ByteChunk . transPipe C . stateC
{-# INLINE putC #-}

-- | Send a value downstream to the next component to consume.
-- If the downstream component terminates, this call will never return control.
-- If you would like to register a cleanup function, please use 'putChunkOr' instead.
-- putChunk is 'yield' with injected inner 'encodingPut'.
putChunk :: Monad m => S.ByteString -> Put i m ()
putChunk !o = do
  lift $ C $ modify' $ encodingPut $ fromIntegral $ SB.length o
  yield $ ByteChunk o
{-# INLINE putChunk #-}

-- | Similar to 'putChunk', but additionally takes a finalizer to be run
-- if the downstream component terminates.
-- putChunkOr is 'yieldOr' with injected inner 'encodingPut'.
putChunkOr :: Monad m
  => S.ByteString
  -> PutC m () -- ^ Finalizer.
  -> Put i m ()
putChunkOr !o f = do
  lift $ C $ modify' $ encodingPut $ fromIntegral $ SB.length o
  yieldOr (ByteChunk o) f
{-# INLINE putChunkOr #-}
