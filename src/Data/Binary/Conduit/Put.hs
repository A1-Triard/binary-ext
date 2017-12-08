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
  , runPutM
  , bytesWrote
  , castPut
  , putWord8
  , putInt8
  , putByteString
  , putLazyByteString
  , putShortByteString
  , putWord16be
  , putWord32be
  , putWord64be
  , putInt16be
  , putInt32be
  , putInt64be
  , putFloatbe
  , putDoublebe
  , putWord16le
  , putWord32le
  , putWord64le
  , putInt16le
  , putInt32le
  , putInt64le
  , putFloatle
  , putDoublele
  , putWordhost
  , putWord16host
  , putWord32host
  , putWord64host
  , putInthost
  , putInt16host
  , putInt32host
  , putInt64host
  , putFloathost
  , putDoublehost
  , putCharUtf8
  , putStringUtf8
  ) where

import Control.Applicative
import Control.Error.Util
import Control.Monad hiding (fail)
import Control.Monad.Base
import Control.Monad.Error.Class
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Loops
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import qualified Data.Binary.Put as S
import Data.Binary.IEEE754 (floatToWord, wordToFloat, doubleToWord, wordToDouble)
import qualified Data.Binary.IEEE754 as S hiding (floatToWord, wordToFloat, doubleToWord, wordToDouble)
import qualified Data.ByteString as S (ByteString)
import qualified Data.ByteString as SB hiding (ByteString, head, last, init, tail)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B hiding (ByteString, head, last, init, tail)
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as HB (ShortByteString)
import Data.Conduit
import Data.Conduit.Lift
import Data.Int
import Data.Maybe hiding (fromJust)
import Data.Semigroup hiding (Option)
import Data.Word
import Data.Binary.Conduit.Put.PutC

-- | The shortening of 'PutM' for the most common use case.
type Put a = forall i m. Monad m => a -> PutM i m ()

-- | Run an encoder presented as a 'PutM' monad.
-- Returns encoder result and produced bytes count.
runPutM :: Monad m => PutM i m a -> ConduitM i S.ByteString m (a, Word64)
runPutM !p = (\(!r, !s) -> (r, encodingBytesWrote s)) <$> runPutC (startEncoding 0) p
{-# INLINE runPutM #-}

-- | Run an encoder presented as a 'Put'.
-- Returns produced bytes count.
runPut :: Monad m => (a -> PutM i m ()) -> a -> ConduitM i S.ByteString m Word64
runPut = (((snd <$>) . runPutM) .)
{-# INLINE runPut #-}

-- | Get the total number of bytes wrote to this point.
bytesWrote :: Monad m => PutM i m Word64
bytesWrote = putC $ \ !x -> return (encodingBytesWrote x, x)
{-# INLINE bytesWrote #-}

-- | Run the given @a -> 'S.Put'@ encoder from binary package
-- and convert result into @'Put' a@.
castPut :: (a -> S.Put) -> Put a
castPut = ((mapM_ putChunk . B.toChunks . S.runPut) .)
{-# INLINE castPut #-}

-- | Write a byte.
putWord8 :: Put Word8
putWord8 = castPut S.putWord8
{-# INLINE putWord8 #-}

-- | Write a signed byte.
putInt8 :: Put Int8
putInt8 = castPut S.putInt8
{-# INLINE putInt8 #-}

-- | Write a strict 'S.ByteString'.
putByteString :: Put S.ByteString
putByteString = castPut S.putByteString
{-# INLINE putByteString #-}

-- | Write a lazy 'ByteString'.
putLazyByteString :: Put ByteString
putLazyByteString = castPut S.putLazyByteString
{-# INLINE putLazyByteString #-}

-- | Write a 'ShortByteString'.
putShortByteString :: Put ShortByteString
putShortByteString = castPut S.putShortByteString
{-# INLINE putShortByteString #-}

-- | Write a 'Word16' in big endian format.
putWord16be :: Put Word16
putWord16be = castPut S.putWord16be
{-# INLINE putWord16be #-}

-- | Write a 'Word32' in big endian format.
putWord32be :: Put Word32
putWord32be = castPut S.putWord32be
{-# INLINE putWord32be #-}

-- | Write a 'Word64' in big endian format.
putWord64be :: Put Word64
putWord64be = castPut S.putWord64be
{-# INLINE putWord64be #-}

-- | Write an 'Int16' in big endian format.
putInt16be :: Put Int16
putInt16be = castPut S.putInt16be
{-# INLINE putInt16be #-}

-- | Write an 'Int32' in big endian format.
putInt32be :: Put Int32
putInt32be = castPut S.putInt32be
{-# INLINE putInt32be #-}

-- | Write an 'Int64' in big endian format.
putInt64be :: Put Int64
putInt64be = castPut S.putInt64be
{-# INLINE putInt64be #-}

-- | Write a 'Float' in big endian IEEE-754 format.
putFloatbe :: Put Float
putFloatbe = castPut S.putFloat32be
{-# INLINE putFloatbe #-}

-- | Write a 'Double' in big endian IEEE-754 format.
putDoublebe :: Put Double
putDoublebe = castPut S.putFloat64be
{-# INLINE putDoublebe #-}

-- | Write a 'Word16' in little endian format.
putWord16le :: Put Word16
putWord16le = castPut S.putWord16le
{-# INLINE putWord16le #-}

-- | Write a 'Word32' in little endian format.
putWord32le :: Put Word32
putWord32le = castPut S.putWord32le
{-# INLINE putWord32le #-}

-- | Write a 'Word64' in little endian format.
putWord64le :: Put Word64
putWord64le = castPut S.putWord64le
{-# INLINE putWord64le #-}

-- | Write an 'Int16' in little endian format.
putInt16le :: Put Int16
putInt16le = castPut S.putInt16le
{-# INLINE putInt16le #-}

-- | Write an 'Int32' in little endian format.
putInt32le :: Put Int32
putInt32le = castPut S.putInt32le
{-# INLINE putInt32le #-}

-- | Write an 'Int64' in little endian format.
putInt64le :: Put Int64
putInt64le = castPut S.putInt64le
{-# INLINE putInt64le #-}

-- | Write a 'Float' in little endian IEEE-754 format.
putFloatle :: Put Float
putFloatle = castPut S.putFloat32le
{-# INLINE putFloatle #-}

-- | Write a 'Double' in little endian IEEE-754 format.
putDoublele :: Put Double
putDoublele = castPut S.putFloat64le
{-# INLINE putDoublele #-}

-- | Write a single native machine word. The word is written in host order,
-- host endian form, for the machine you're on.
-- On a 64 bit machine the 'Word' is an 8 byte value, on a 32 bit machine, 4 bytes.
-- Values written this way are not portable to different endian or word sized machines, without conversion.
putWordhost :: Put Word
putWordhost = castPut S.putWordhost
{-# INLINE putWordhost #-}

-- | Write a 'Word16' in native host order and host endianness. For portability issues see 'putWordhost'.
putWord16host :: Put Word16
putWord16host = castPut S.putWord16host
{-# INLINE putWord16host #-}

-- | Write a 'Word32' in native host order and host endianness. For portability issues see 'putWordhost'.
putWord32host :: Put Word32
putWord32host = castPut S.putWord32host
{-# INLINE putWord32host #-}

-- | Write a 'Word64' in native host order On a 32 bit machine we write two host order 'Word32's,
-- in big endian form. For portability issues see 'putWordhost'.
putWord64host :: Put Word64
putWord64host = castPut S.putWord64host
{-# INLINE putWord64host #-}

-- | Write a single native machine word. The word is written in host order, host endian form,
-- for the machine you're on.
-- On a 64 bit machine the 'Int' is an 8 byte value, on a 32 bit machine, 4 bytes.
-- Values written this way are not portable to different endian or word sized machines, without conversion.
putInthost :: Put Int
putInthost = castPut S.putInthost
{-# INLINE putInthost #-}

-- | Write an 'Int16' in native host order and host endianness. For portability issues see 'putInthost'.
putInt16host :: Put Int16
putInt16host = castPut S.putInt16host
{-# INLINE putInt16host #-}

-- | Write an 'Int32' in native host order and host endianness. For portability issues see 'putInthost'.
putInt32host :: Put Int32
putInt32host = castPut S.putInt32host
{-# INLINE putInt32host #-}

-- | Write an 'Int64' in native host order On a 32 bit machine we write two host order 'Int32's,
-- in big endian form. For portability issues see putInthost.
putInt64host :: Put Int64
putInt64host = castPut S.putInt64host
{-# INLINE putInt64host #-}

-- | Write a 'Float' in native in IEEE-754 format and host endian.
putFloathost :: Put Float
putFloathost = castPut S.putWord32host . floatToWord
{-# INLINE putFloathost #-}

-- | Write a 'Double' in native in IEEE-754 format and host endian.
putDoublehost :: Put Double
putDoublehost = castPut S.putWord64host . doubleToWord
{-# INLINE putDoublehost #-}

-- | Write a character using UTF-8 encoding.
putCharUtf8 :: Put Char
putCharUtf8 = castPut S.putCharUtf8
{-# INLINE putCharUtf8 #-}

-- | Write a 'String' using UTF-8 encoding.
putStringUtf8 :: Put String
putStringUtf8 = castPut S.putStringUtf8
{-# INLINE putStringUtf8 #-}
