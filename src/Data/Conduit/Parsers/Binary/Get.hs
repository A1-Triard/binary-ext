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

-- | At the first look, Data.Binary.Conduit.Get module is very similar with Data.Binary.Get.
-- The main differences between them are the following.
-- While the 'S.Get' from binary is a very custom monad,
-- the local 'Get' is 'ConduitM', which leads to easy integration in complicated format parsing.
-- The Data.Binary.Get module does not have a function to create custom 'S.Get' monad,
-- this module provides 'getC'.
-- Unlike 'isolate' from binary, local 'isolate' does not "cut" bytes counter.
-- While the binary's 'S.Get' is 'MonadFail', which leads to very ugly errors handling
-- in complicated cases, local 'Get' is 'MonadError'.

module Data.Conduit.Parsers.Binary.Get
  ( MonadMapError (..)
  , (?=>>)
  , (?>>)
  , DefaultDecodingState
  , GetM
  , Get
  , runGet
  , bytesRead
  , castGet
  , skip
  , isolate
  , getByteString
  , getLazyByteString
  , getLazyByteStringNul
  , getRemainingLazyByteString
  , getWord8
  , getInt8
  , getWord16be
  , getWord32be
  , getWord64be
  , getWord16le
  , getWord32le
  , getWord64le
  , getWordhost
  , getWord16host
  , getWord32host
  , getWord64host
  , getInt16be
  , getInt32be
  , getInt64be
  , getInt16le
  , getInt32le
  , getInt64le
  , getInthost
  , getInt16host
  , getInt32host
  , getInt64host
  , getFloatbe
  , getFloatle
  , getFloathost
  , getDoublebe
  , getDoublele
  , getDoublehost
  , endOfInput
  ) where

import qualified Data.Binary.Get as S
import Data.Binary.IEEE754 (wordToFloat, wordToDouble)
import qualified Data.Binary.IEEE754 as S hiding (floatToWord, wordToFloat, doubleToWord, wordToDouble)
import qualified Data.ByteString as S (ByteString)
import qualified Data.ByteString as SB hiding (ByteString, head, last, init, tail)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B hiding (ByteString, head, last, init, tail)
import Data.Conduit
import Data.Int
import Data.Maybe
import Data.Semigroup hiding (Option)
import Data.Word
import Control.Monad.Error.Map
import Data.Conduit.Parsers
import Data.Conduit.Parsers.Binary ()
import Data.Conduit.Parsers.Binary.ByteOffset
import Data.Conduit.Parsers.GetC

class (DecodingState s, DecodingToken s ~ S.ByteString, DecodingElemsRead s) => DefaultDecodingState s where

instance (DecodingState s, DecodingToken s ~ S.ByteString, DecodingElemsRead s) => DefaultDecodingState s where

-- | The shortening of 'GetM' for the most common use case.
type Get e a = forall s o m. (DefaultDecodingState s, Monad m) => GetM s S.ByteString o e m a

-- | Run a decoder presented as a 'Get' monad.
-- Returns decoder result and consumed bytes count.
runGet :: Monad m => GetM ByteOffset i o e m a -> ConduitM i o m (Either e a)
runGet !g = fst <$> runGetC (startDecoding $ ByteOffset 0) g
{-# INLINE runGet #-}

-- | Get the total number of bytes read to this point.
bytesRead :: (DecodingState s, DecodingElemsRead s, Monad m) => GetM s i o e m Word64
bytesRead = elemsRead
{-# INLINE bytesRead #-}

-- | Run the given 'S.Get' monad from binary package
-- and convert result into 'Get'.
castGet :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => S.Get a -> GetM s S.ByteString o String m a
castGet !g = getC $
  go (S.runGetIncremental g) SB.empty
  where
  go (S.Done !rest _ !result) !chunk !decoding =
    if SB.null rest
      then return (Right result, decoded chunk decoding)
      else leftover rest >> return (Right result, decoded (SB.take (SB.length chunk - SB.length rest) chunk) decoding)
  go (S.Fail _ _ !err) !chunk !decoding = return (Left err, decoded chunk decoding)
  go (S.Partial !continue) !chunk !decoding = do
    next <- await
    go (continue next) (fromMaybe SB.empty next) (decoded chunk decoding)
{-# INLINE castGet #-}

voidError :: Monad m => GetM s i o e m a -> GetM s i o () m a
voidError = mapError (const ())
{-# INLINE voidError #-}

-- | An efficient get method for strict 'S.ByteString's. Fails if fewer than @n@
-- bytes are left in the input. If @n <= 0@ then the empty string is returned.
getByteString :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => Int -> GetM s S.ByteString o () m S.ByteString
getByteString !n = getC $
  go SB.empty 0
  where
  go consumed !consumed_length !decoding
    | consumed_length >= n = return (Right consumed, decoding)
    | otherwise = do
      !mi <- await
      case mi of
        Nothing -> return (Left (), decoding)
        Just !i -> do
          let !gap = n - consumed_length
          if gap >= SB.length i
            then do
              go (consumed <> i) (consumed_length + fromIntegral (SB.length i)) (decoded i decoding)
            else do
              let (!got, !rest) = SB.splitAt gap i
              leftover rest
              return (Right (consumed <> got), decoded got decoding)
{-# INLINE getByteString #-}

-- | An efficient get method for lazy 'ByteString's. Fails if fewer than @n@
-- bytes are left in the input. If @n <= 0@ then the empty string is returned.
getLazyByteString :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => Int64 -> GetM s S.ByteString o () m ByteString
getLazyByteString n = getC $
  go B.empty 0
  where
  go consumed !consumed_length !decoding
    | consumed_length >= n = return (Right consumed, decoding)
    | otherwise = do
      !mi <- await
      case mi of
        Nothing -> return (Left (), decoding)
        Just !i -> do
          let !gap = n - consumed_length
          if gap >= fromIntegral (SB.length i)
            then do
              go (consumed <> B.fromStrict i) (consumed_length + fromIntegral (SB.length i)) (decoded i decoding)
            else do
              let (!got, !rest) = SB.splitAt (fromIntegral gap) i
              leftover rest
              return (Right (consumed <> B.fromStrict got), decoded got decoding)
{-# INLINE getLazyByteString #-}

-- | Get a lazy 'ByteString' that is terminated with a NUL byte.
-- The returned string does not contain the NUL byte.
-- Fails if it reaches the end of input without finding a NUL.
getLazyByteStringNul :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o () m ByteString
getLazyByteStringNul = getC $
  go B.empty
  where
  go consumed !decoding = do
    !mi <- await
    case mi of
      Nothing -> return (Left (), decoding)
      Just !i -> do
        let (!h, !t) = SB.span (/= 0) i
        let r = consumed <> B.fromStrict h
        let !d = decoded h decoding
        if SB.length t == 0
          then go r d
          else do
            let (!z, !zt) = SB.splitAt 1 t
            leftover zt
            return (Right r, decoded z $ decoded h decoding)
{-# INLINE getLazyByteStringNul #-}

-- | Get the remaining bytes as a lazy 'ByteString'.
-- Note that this can be an expensive function to use as it
-- forces reading all input and keeping the string in-memory.
getRemainingLazyByteString :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o e m ByteString
getRemainingLazyByteString = getC $
  go B.empty
  where
  go consumed !decoding = do
    !mi <- await
    case mi of
      Nothing -> return (Right consumed, decoding)
      Just !i -> go (consumed <> B.fromStrict i) (decoded i decoding)

voidCastGet :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => S.Get a -> GetM s S.ByteString o () m a
voidCastGet = voidError . castGet
{-# INLINE voidCastGet #-}

-- | Read a 'Word8' from the monad state.
getWord8 :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o () m Word8
getWord8 = voidCastGet S.getWord8
{-# INLINE getWord8 #-}

-- | Read an 'Int8' from the monad state.
getInt8 :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o () m Int8
getInt8 = voidCastGet S.getInt8
{-# INLINE getInt8 #-}

-- | Read a 'Word16' in big endian format.
getWord16be :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o () m Word16
getWord16be = voidCastGet S.getWord16be
{-# INLINE getWord16be #-}

-- | Read a 'Word32' in big endian format.
getWord32be :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o () m Word32
getWord32be = voidCastGet S.getWord32be
{-# INLINE getWord32be #-}

-- | Read a 'Word64' in big endian format.
getWord64be :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o () m Word64
getWord64be = voidCastGet S.getWord64be
{-# INLINE getWord64be #-}

-- | Read a 'Word16' in little endian format.
getWord16le :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o () m Word16
getWord16le = voidCastGet S.getWord16le
{-# INLINE getWord16le #-}

-- | Read a 'Word32' in little endian format.
getWord32le :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o () m Word32
getWord32le = voidCastGet S.getWord32le
{-# INLINE getWord32le #-}

-- | Read a 'Word64' in little endian format.
getWord64le :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o () m Word64
getWord64le = voidCastGet S.getWord64le
{-# INLINE getWord64le #-}

-- | Read a single native machine word. The word is read in
-- host order, host endian form, for the machine you're on. On a 64 bit
-- machine the Word is an 8 byte value, on a 32 bit machine, 4 bytes.
getWordhost :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o () m Word
getWordhost = voidCastGet S.getWordhost
{-# INLINE getWordhost #-}

-- | Read a 2 byte 'Word16' in native host order and host endianness.
getWord16host :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o () m Word16
getWord16host = voidCastGet S.getWord16host
{-# INLINE getWord16host #-}

-- | Read a 4 byte 'Word32' in native host order and host endianness.
getWord32host :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o () m Word32
getWord32host = voidCastGet S.getWord32host
{-# INLINE getWord32host #-}

-- | Read a 8 byte 'Word64' in native host order and host endianness.
getWord64host :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o () m Word64
getWord64host = voidCastGet S.getWord64host
{-# INLINE getWord64host #-}

-- | Read an 'Int16' in big endian format.
getInt16be :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o () m Int16
getInt16be = voidCastGet S.getInt16be
{-# INLINE getInt16be #-}

-- | Read an 'Int32' in big endian format.
getInt32be :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o () m Int32
getInt32be = voidCastGet S.getInt32be
{-# INLINE getInt32be #-}

-- | Read an 'Int64' in big endian format.
getInt64be :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o () m Int64
getInt64be = voidCastGet S.getInt64be
{-# INLINE getInt64be #-}

-- | Read an 'Int16' in little endian format.
getInt16le :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o () m Int16
getInt16le = voidCastGet S.getInt16le
{-# INLINE getInt16le #-}

-- | Read an 'Int32' in little endian format.
getInt32le :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o () m Int32
getInt32le = voidCastGet S.getInt32le
{-# INLINE getInt32le #-}

-- | Read an 'Int64' in little endian format.
getInt64le :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o () m Int64
getInt64le = voidCastGet S.getInt64le
{-# INLINE getInt64le #-}

-- | Read a single native machine word. It works in the same way as 'getWordhost'.
getInthost :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o () m Int
getInthost = voidCastGet S.getInthost
{-# INLINE getInthost #-}

-- | Read a 2 byte 'Int16' in native host order and host endianness.
getInt16host :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o () m Int16
getInt16host = voidCastGet S.getInt16host
{-# INLINE getInt16host #-}

-- | Read a 4 byte 'Int32' in native host order and host endianness.
getInt32host :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o () m Int32
getInt32host = voidCastGet S.getInt32host
{-# INLINE getInt32host #-}

-- | Read a 8 byte 'Int64' in native host order and host endianness.
getInt64host :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o () m Int64
getInt64host = voidCastGet S.getInt64host
{-# INLINE getInt64host #-}

-- | Read a 'Float' in big endian IEEE-754 format.
getFloatbe :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o () m Float
getFloatbe = voidCastGet S.getFloat32be
{-# INLINE getFloatbe #-}

-- | Read a 'Float' in little endian IEEE-754 format.
getFloatle :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o () m Float
getFloatle = voidCastGet S.getFloat32le
{-# INLINE getFloatle #-}

-- | Read a 'Float' in IEEE-754 format and host endian.
getFloathost :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o () m Float
getFloathost = wordToFloat <$> voidCastGet S.getWord32host
{-# INLINE getFloathost #-}

-- | Read a 'Double' in big endian IEEE-754 format.
getDoublebe :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o () m Double
getDoublebe = voidCastGet S.getFloat64be
{-# INLINE getDoublebe #-}

-- | Read a 'Double' in little endian IEEE-754 format.
getDoublele :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o () m Double
getDoublele = voidCastGet S.getFloat64le
{-# INLINE getDoublele #-}

-- | Read a 'Double' in IEEE-754 format and host endian.
getDoublehost :: (DecodingState s, DecodingToken s ~ S.ByteString, Monad m) => GetM s S.ByteString o () m Double
getDoublehost = wordToDouble <$> voidCastGet S.getWord64host
{-# INLINE getDoublehost #-}
