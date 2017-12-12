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
  , encodingBytesWrote
  , startEncoding
  , encoded
  , PutS
  , runPutS
  , putS
  , PutM
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
import Data.Binary.Conduit.Put.PutS

-- | The shortening of 'PutM' for the most common use case.
type Put = forall i m. Monad m => PutM i m ()

-- | Run an encoder presented as a 'Put' monad.
-- Returns 'Producer'.
runPut :: PutM i m () -> ConduitM i S.ByteString m ()
runPut !p = runEncoding $ snd $ runPutS p $ startEncoding 0 $ return ()
{-# INLINE runPut #-}

-- | Get the total number of bytes wrote to this point.
-- Can be used with 'mfix' to result bytes count prediction:
-- > putWithSize :: Monad m => PutM i m () -> PutM i m ()
-- > putWithSize !p = void $ mfix $ \size -> do
-- >   putWord64le size
-- >   before <- bytesWrote
-- >   p
-- >   after <- bytesWrote
-- >   return $ after - before
bytesWrote :: PutM i m Word64
bytesWrote = putS $ \ !s -> (encodingBytesWrote s, s)
{-# INLINE bytesWrote #-}

-- | Run the given 'S.Put' encoder from binary package
-- producing the given bytes count
-- and convert result into a 'Put'.
castPut :: Word64 -> S.Put -> Put
castPut !n !p = putS $ \ !s -> ((), mapM_ yield (B.toChunks $ S.runPut p) `encoded` n $ s)
{-# INLINE castPut #-}

-- | Write a byte.
putWord8 :: Word8 -> Put
putWord8 = castPut 1 . S.putWord8
{-# INLINE putWord8 #-}

-- | Write a signed byte.
putInt8 :: Int8 -> Put
putInt8 = castPut 1 . S.putInt8
{-# INLINE putInt8 #-}

-- | Write a strict 'S.ByteString'.
putByteString :: S.ByteString -> Put
putByteString b = castPut (fromIntegral $ SB.length b) $ S.putByteString b
{-# INLINE putByteString #-}

-- | Write a lazy 'ByteString'.
putLazyByteString :: ByteString -> Put
putLazyByteString b = castPut (fromIntegral $ B.length b) $ S.putLazyByteString b
{-# INLINE putLazyByteString #-}

-- | Write a 'ShortByteString'.
putShortByteString :: ShortByteString -> Put
putShortByteString b = castPut (fromIntegral $ HB.length b) $ S.putShortByteString b
{-# INLINE putShortByteString #-}

-- | Write a 'Word16' in big endian format.
putWord16be :: Word16 -> Put
putWord16be = castPut 2 . S.putWord16be
{-# INLINE putWord16be #-}

-- | Write a 'Word32' in big endian format.
putWord32be :: Word32 -> Put
putWord32be = castPut 4 . S.putWord32be
{-# INLINE putWord32be #-}

-- | Write a 'Word64' in big endian format.
putWord64be :: Word64 -> Put
putWord64be = castPut 8 . S.putWord64be
{-# INLINE putWord64be #-}

-- | Write an 'Int16' in big endian format.
putInt16be :: Int16 -> Put
putInt16be = castPut 2 . S.putInt16be
{-# INLINE putInt16be #-}

-- | Write an 'Int32' in big endian format.
putInt32be :: Int32 -> Put
putInt32be = castPut 4 . S.putInt32be
{-# INLINE putInt32be #-}

-- | Write an 'Int64' in big endian format.
putInt64be :: Int64 -> Put
putInt64be = castPut 8 . S.putInt64be
{-# INLINE putInt64be #-}

-- | Write a 'Float' in big endian IEEE-754 format.
putFloatbe :: Float -> Put
putFloatbe = castPut 4 . S.putFloat32be
{-# INLINE putFloatbe #-}

-- | Write a 'Double' in big endian IEEE-754 format.
putDoublebe :: Double -> Put
putDoublebe = castPut 8 . S.putFloat64be
{-# INLINE putDoublebe #-}

-- | Write a 'Word16' in little endian format.
putWord16le :: Word16 -> Put
putWord16le = castPut 2 . S.putWord16le
{-# INLINE putWord16le #-}

-- | Write a 'Word32' in little endian format.
putWord32le :: Word32 -> Put
putWord32le = castPut 4 . S.putWord32le
{-# INLINE putWord32le #-}

-- | Write a 'Word64' in little endian format.
putWord64le :: Word64 -> Put
putWord64le = castPut 8 . S.putWord64le
{-# INLINE putWord64le #-}

-- | Write an 'Int16' in little endian format.
putInt16le :: Int16 -> Put
putInt16le = castPut 2 . S.putInt16le
{-# INLINE putInt16le #-}

-- | Write an 'Int32' in little endian format.
putInt32le :: Int32 -> Put
putInt32le = castPut 4 . S.putInt32le
{-# INLINE putInt32le #-}

-- | Write an 'Int64' in little endian format.
putInt64le :: Int64 -> Put
putInt64le = castPut 8 . S.putInt64le
{-# INLINE putInt64le #-}

-- | Write a 'Float' in little endian IEEE-754 format.
putFloatle :: Float -> Put
putFloatle = castPut 4 . S.putFloat32le
{-# INLINE putFloatle #-}

-- | Write a 'Double' in little endian IEEE-754 format.
putDoublele :: Double -> Put
putDoublele = castPut 8 . S.putFloat64le
{-# INLINE putDoublele #-}

-- | Write a single native machine word. The word is written in host order,
-- host endian form, for the machine you're on.
-- On a 64 bit machine the 'Word' is an 8 byte value, on a 32 bit machine, 4 bytes.
-- Values written this way are not portable to different endian or word sized machines, without conversion.
putWordhost :: Word -> Put
putWordhost = castPut (fromIntegral $ finiteBitSize (0 :: Word)) . S.putWordhost
{-# INLINE putWordhost #-}

-- | Write a 'Word16' in native host order and host endianness. For portability issues see 'putWordhost'.
putWord16host :: Word16 -> Put
putWord16host = castPut 2 . S.putWord16host
{-# INLINE putWord16host #-}

-- | Write a 'Word32' in native host order and host endianness. For portability issues see 'putWordhost'.
putWord32host :: Word32 -> Put
putWord32host = castPut 4 . S.putWord32host
{-# INLINE putWord32host #-}

-- | Write a 'Word64' in native host order On a 32 bit machine we write two host order 'Word32's,
-- in big endian form. For portability issues see 'putWordhost'.
putWord64host :: Word64 -> Put
putWord64host = castPut 8 . S.putWord64host
{-# INLINE putWord64host #-}

-- | Write a single native machine word. The word is written in host order, host endian form,
-- for the machine you're on.
-- On a 64 bit machine the 'Int' is an 8 byte value, on a 32 bit machine, 4 bytes.
-- Values written this way are not portable to different endian or word sized machines, without conversion.
putInthost :: Int -> Put
putInthost = castPut (fromIntegral $ finiteBitSize (0 :: Int)) . S.putInthost
{-# INLINE putInthost #-}

-- | Write an 'Int16' in native host order and host endianness. For portability issues see 'putInthost'.
putInt16host :: Int16 -> Put
putInt16host = castPut 2 . S.putInt16host
{-# INLINE putInt16host #-}

-- | Write an 'Int32' in native host order and host endianness. For portability issues see 'putInthost'.
putInt32host :: Int32 -> Put
putInt32host = castPut 4 . S.putInt32host
{-# INLINE putInt32host #-}

-- | Write an 'Int64' in native host order On a 32 bit machine we write two host order 'Int32's,
-- in big endian form. For portability issues see putInthost.
putInt64host :: Int64 -> Put
putInt64host = castPut 8 . S.putInt64host
{-# INLINE putInt64host #-}

-- | Write a 'Float' in native in IEEE-754 format and host endian.
putFloathost :: Float -> Put
putFloathost = castPut 4 . S.putWord32host . floatToWord
{-# INLINE putFloathost #-}

-- | Write a 'Double' in native in IEEE-754 format and host endian.
putDoublehost :: Double -> Put
putDoublehost = castPut 8 . S.putWord64host . doubleToWord
{-# INLINE putDoublehost #-}
