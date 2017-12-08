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

-- | Despite binary's 'S.Put' is fully-functional construction (unlike 'S.Get'),
-- we decided to provide this module for symmetry with 'Data.Binary.Conduit.Get'.

module Data.Binary.Conduit.Put
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
  , runPut
  , bytesWrote
  , castPut
  , putWord8
  , putInt8
  , putByteString
  , putLazyByteString
  , putShortByteString
  ) where

#include <haskell>

import Data.Binary.Conduit.Put.PutC

-- | The shortening of 'PutM' for the most common use case.
type Put a = forall i m. Monad m => a -> PutM i m ()

-- | Run an encoder presented as a 'Put' monad.
-- Returns encoder result and produced bytes count.
runPut :: Monad m => PutM i m a -> ConduitM i S.ByteString m (a, Word64)
runPut !p = (\(!r, !s) -> (r, encodingBytesWrote s)) <$> runPutC (startEncoding 0) p
{-# INLINE runPut #-}

-- | Get the total number of bytes wrote to this point.
bytesWrote :: Monad m => PutM i m Word64
bytesWrote = putC $ \ !x -> return (encodingBytesWrote x, x)
{-# INLINE bytesWrote #-}

-- | Run the given 'S.PutM' monad from binary package
-- and convert result into 'Put'.
castPut :: (a -> S.Put) -> Put a
castPut !p = mapM_ putChunk . B.toChunks . S.runPut . p
{-# INLINE castPut #-}

-- | Efficiently write a byte into the output buffer.
putWord8 :: Put Word8
putWord8 = castPut S.putWord8
{-# INLINE putWord8 #-}

-- | Efficiently write a signed byte into the output buffer.
putInt8 :: Put Int8
putInt8 = castPut S.putInt8
{-# INLINE putInt8 #-}

-- | An efficient primitive to write a strict 'S.ByteString' into the output buffer.
-- It flushes the current buffer, and writes the argument into a new chunk.
putByteString :: Put S.ByteString
putByteString = castPut S.putByteString
{-# INLINE putByteString #-}

-- | Write a lazy 'ByteString' efficiently, simply appending the lazy 'ByteString' chunks to the output buffer.
putLazyByteString :: Put ByteString
putLazyByteString = castPut S.putLazyByteString
{-# INLINE putLazyByteString #-}

-- | Write 'ShortByteString' to the buffer.
putShortByteString :: Put ShortByteString
putShortByteString = castPut S.putShortByteString
{-# INLINE putShortByteString #-}
