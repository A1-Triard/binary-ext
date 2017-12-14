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

module Data.Conduit.Parsers.Binary.Put
  ( PutM
  , DefaultEncodingState
  , Put
  , runPut
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
  ) where

import qualified Data.Binary.Put as S
import Data.Binary.IEEE754 (floatToWord, doubleToWord)
import qualified Data.Binary.IEEE754 as S hiding (floatToWord, wordToFloat, doubleToWord, wordToDouble)
import Data.Bits
import qualified Data.ByteString as S (ByteString)
import qualified Data.ByteString as SB hiding (ByteString, head, last, init, tail)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B hiding (ByteString, head, last, init, tail)
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as HB hiding (ShortByteString)
import Data.Conduit
import Data.Int
import Data.Word
import Data.Conduit.Parsers.Binary.ByteOffset
import Data.Conduit.Parsers.PutS

class (EncodingState s, EncodingToken s ~ Word64, EncodingBytesWrote s) => DefaultEncodingState s where

instance (EncodingState s, EncodingToken s ~ Word64, EncodingBytesWrote s) => DefaultEncodingState s where

-- | The shortening of 'PutM' for the most common use case.
type Put = forall s i m. (DefaultEncodingState s, Monad m) => PutM s i S.ByteString m ()

-- | Run an encoder presented as a 'Put' monad.
-- Returns 'Producer'.
runPut :: PutM ByteOffset i o m () -> ConduitM i o m ()
runPut !p = runEncoding $ snd $ runPutS p $ startEncoding $ ByteOffset 0
{-# INLINE runPut #-}

-- | Get the total number of bytes wrote to this point.
-- Can be used with 'mfix' to result bytes count prediction:
-- > putWithSize :: (DefaultEncodingState s, Monad m) => PutM s i S.ByteString m () -> PutM s i S.ByteString m ()
-- > putWithSize !p = void $ mfix $ \size -> do
-- >   putWord64le size
-- >   before <- bytesWrote
-- >   p
-- >   after <- bytesWrote
-- >   return $ after - before
bytesWrote :: EncodingBytesWrote s => PutM s i o m Word64
bytesWrote = putS $ \ !s -> (encodingBytesWrote s, s)
{-# INLINE bytesWrote #-}

-- | Run the given 'S.Put' encoder from binary package
-- producing the given bytes count
-- and convert result into a 'Put'.
castPut :: (EncodingState s, Monad m) => EncodingToken s -> S.Put -> PutM s i S.ByteString m ()
castPut !n !p = putS $ \ !t -> ((), encoded (mapM_ yield $ B.toChunks $ S.runPut p, n) t)
{-# INLINE castPut #-}

-- | Write a byte.
putWord8 :: (EncodingState s, Num (EncodingToken s), Monad m) => Word8 -> PutM s i S.ByteString m ()
putWord8 = castPut 1 . S.putWord8
{-# INLINE putWord8 #-}

-- | Write a signed byte.
putInt8 :: (EncodingState s, Num (EncodingToken s), Monad m) => Int8 -> PutM s i S.ByteString m ()
putInt8 = castPut 1 . S.putInt8
{-# INLINE putInt8 #-}

-- | Write a strict 'S.ByteString'.
putByteString :: (EncodingState s, Num (EncodingToken s), Monad m) => S.ByteString -> PutM s i S.ByteString m ()
putByteString b = castPut (fromIntegral $ SB.length b) $ S.putByteString b
{-# INLINE putByteString #-}

-- | Write a lazy 'ByteString'.
putLazyByteString :: (EncodingState s, Num (EncodingToken s), Monad m) => ByteString -> PutM s i S.ByteString m ()
putLazyByteString b = castPut (fromIntegral $ B.length b) $ S.putLazyByteString b
{-# INLINE putLazyByteString #-}

-- | Write a 'ShortByteString'.
putShortByteString :: (EncodingState s, Num (EncodingToken s), Monad m) => ShortByteString -> PutM s i S.ByteString m ()
putShortByteString b = castPut (fromIntegral $ HB.length b) $ S.putShortByteString b
{-# INLINE putShortByteString #-}

-- | Write a 'Word16' in big endian format.
putWord16be :: (EncodingState s, Num (EncodingToken s), Monad m) => Word16 -> PutM s i S.ByteString m ()
putWord16be = castPut 2 . S.putWord16be
{-# INLINE putWord16be #-}

-- | Write a 'Word32' in big endian format.
putWord32be :: (EncodingState s, Num (EncodingToken s), Monad m) => Word32 -> PutM s i S.ByteString m ()
putWord32be = castPut 4 . S.putWord32be
{-# INLINE putWord32be #-}

-- | Write a 'Word64' in big endian format.
putWord64be :: (EncodingState s, Num (EncodingToken s), Monad m) => Word64 -> PutM s i S.ByteString m ()
putWord64be = castPut 8 . S.putWord64be
{-# INLINE putWord64be #-}

-- | Write an 'Int16' in big endian format.
putInt16be :: (EncodingState s, Num (EncodingToken s), Monad m) => Int16 -> PutM s i S.ByteString m ()
putInt16be = castPut 2 . S.putInt16be
{-# INLINE putInt16be #-}

-- | Write an 'Int32' in big endian format.
putInt32be :: (EncodingState s, Num (EncodingToken s), Monad m) => Int32 -> PutM s i S.ByteString m ()
putInt32be = castPut 4 . S.putInt32be
{-# INLINE putInt32be #-}

-- | Write an 'Int64' in big endian format.
putInt64be :: (EncodingState s, Num (EncodingToken s), Monad m) => Int64 -> PutM s i S.ByteString m ()
putInt64be = castPut 8 . S.putInt64be
{-# INLINE putInt64be #-}

-- | Write a 'Float' in big endian IEEE-754 format.
putFloatbe :: (EncodingState s, Num (EncodingToken s), Monad m) => Float -> PutM s i S.ByteString m ()
putFloatbe = castPut 4 . S.putFloat32be
{-# INLINE putFloatbe #-}

-- | Write a 'Double' in big endian IEEE-754 format.
putDoublebe :: (EncodingState s, Num (EncodingToken s), Monad m) => Double -> PutM s i S.ByteString m ()
putDoublebe = castPut 8 . S.putFloat64be
{-# INLINE putDoublebe #-}

-- | Write a 'Word16' in little endian format.
putWord16le :: (EncodingState s, Num (EncodingToken s), Monad m) => Word16 -> PutM s i S.ByteString m ()
putWord16le = castPut 2 . S.putWord16le
{-# INLINE putWord16le #-}

-- | Write a 'Word32' in little endian format.
putWord32le :: (EncodingState s, Num (EncodingToken s), Monad m) => Word32 -> PutM s i S.ByteString m ()
putWord32le = castPut 4 . S.putWord32le
{-# INLINE putWord32le #-}

-- | Write a 'Word64' in little endian format.
putWord64le :: (EncodingState s, Num (EncodingToken s), Monad m) => Word64 -> PutM s i S.ByteString m ()
putWord64le = castPut 8 . S.putWord64le
{-# INLINE putWord64le #-}

-- | Write an 'Int16' in little endian format.
putInt16le :: (EncodingState s, Num (EncodingToken s), Monad m) => Int16 -> PutM s i S.ByteString m ()
putInt16le = castPut 2 . S.putInt16le
{-# INLINE putInt16le #-}

-- | Write an 'Int32' in little endian format.
putInt32le :: (EncodingState s, Num (EncodingToken s), Monad m) => Int32 -> PutM s i S.ByteString m ()
putInt32le = castPut 4 . S.putInt32le
{-# INLINE putInt32le #-}

-- | Write an 'Int64' in little endian format.
putInt64le :: (EncodingState s, Num (EncodingToken s), Monad m) => Int64 -> PutM s i S.ByteString m ()
putInt64le = castPut 8 . S.putInt64le
{-# INLINE putInt64le #-}

-- | Write a 'Float' in little endian IEEE-754 format.
putFloatle :: (EncodingState s, Num (EncodingToken s), Monad m) => Float -> PutM s i S.ByteString m ()
putFloatle = castPut 4 . S.putFloat32le
{-# INLINE putFloatle #-}

-- | Write a 'Double' in little endian IEEE-754 format.
putDoublele :: (EncodingState s, Num (EncodingToken s), Monad m) => Double -> PutM s i S.ByteString m ()
putDoublele = castPut 8 . S.putFloat64le
{-# INLINE putDoublele #-}

-- | Write a single native machine word. The word is written in host order,
-- host endian form, for the machine you're on.
-- On a 64 bit machine the 'Word' is an 8 byte value, on a 32 bit machine, 4 bytes.
-- Values written this way are not portable to different endian or word sized machines, without conversion.
putWordhost :: (EncodingState s, Num (EncodingToken s), Monad m) => Word -> PutM s i S.ByteString m ()
putWordhost = castPut (fromIntegral $ finiteBitSize (0 :: Word)) . S.putWordhost
{-# INLINE putWordhost #-}

-- | Write a 'Word16' in native host order and host endianness. For portability issues see 'putWordhost'.
putWord16host :: (EncodingState s, Num (EncodingToken s), Monad m) => Word16 -> PutM s i S.ByteString m ()
putWord16host = castPut 2 . S.putWord16host
{-# INLINE putWord16host #-}

-- | Write a 'Word32' in native host order and host endianness. For portability issues see 'putWordhost'.
putWord32host :: (EncodingState s, Num (EncodingToken s), Monad m) => Word32 -> PutM s i S.ByteString m ()
putWord32host = castPut 4 . S.putWord32host
{-# INLINE putWord32host #-}

-- | Write a 'Word64' in native host order On a 32 bit machine we write two host order 'Word32's,
-- in big endian form. For portability issues see 'putWordhost'.
putWord64host :: (EncodingState s, Num (EncodingToken s), Monad m) => Word64 -> PutM s i S.ByteString m ()
putWord64host = castPut 8 . S.putWord64host
{-# INLINE putWord64host #-}

-- | Write a single native machine word. The word is written in host order, host endian form,
-- for the machine you're on.
-- On a 64 bit machine the 'Int' is an 8 byte value, on a 32 bit machine, 4 bytes.
-- Values written this way are not portable to different endian or word sized machines, without conversion.
putInthost :: (EncodingState s, Num (EncodingToken s), Monad m) => Int -> PutM s i S.ByteString m ()
putInthost = castPut (fromIntegral $ finiteBitSize (0 :: Int)) . S.putInthost
{-# INLINE putInthost #-}

-- | Write an 'Int16' in native host order and host endianness. For portability issues see 'putInthost'.
putInt16host :: (EncodingState s, Num (EncodingToken s), Monad m) => Int16 -> PutM s i S.ByteString m ()
putInt16host = castPut 2 . S.putInt16host
{-# INLINE putInt16host #-}

-- | Write an 'Int32' in native host order and host endianness. For portability issues see 'putInthost'.
putInt32host :: (EncodingState s, Num (EncodingToken s), Monad m) => Int32 -> PutM s i S.ByteString m ()
putInt32host = castPut 4 . S.putInt32host
{-# INLINE putInt32host #-}

-- | Write an 'Int64' in native host order On a 32 bit machine we write two host order 'Int32's,
-- in big endian form. For portability issues see putInthost.
putInt64host :: (EncodingState s, Num (EncodingToken s), Monad m) => Int64 -> PutM s i S.ByteString m ()
putInt64host = castPut 8 . S.putInt64host
{-# INLINE putInt64host #-}

-- | Write a 'Float' in native in IEEE-754 format and host endian.
putFloathost :: (EncodingState s, Num (EncodingToken s), Monad m) => Float -> PutM s i S.ByteString m ()
putFloathost = castPut 4 . S.putWord32host . floatToWord
{-# INLINE putFloathost #-}

-- | Write a 'Double' in native in IEEE-754 format and host endian.
putDoublehost :: (EncodingState s, Num (EncodingToken s), Monad m) => Double -> PutM s i S.ByteString m ()
putDoublehost = castPut 8 . S.putWord64host . doubleToWord
{-# INLINE putDoublehost #-}
