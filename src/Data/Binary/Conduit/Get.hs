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

module Data.Binary.Conduit.Get
  ( Decoding
  , startDecoding
  , decodingBytesRead
  , decodingGot
  , decodingUngot
  , GetC
  , ByteChunk
  , GetM
  , runGetC
  , getC
  , getChunk
  , ungetChunk
  , mapError
  , Get
  , runGet
  , bytesRead
  , castGet
  , endOfInput
  , onError
  , withError
  , ifError
  , voidError
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
  ) where

#include <haskell>

import Data.Binary.Conduit.Get.GetC

-- | The shortening of 'GetM' for most common use case.
type Get o e a = forall m. Monad m => GetM o e m a

-- | Run a decoder presented as a 'Get' monad.
-- Returns decoder result and consumed bytes count.
runGet :: Monad m => GetM o e m a -> ConduitM S.ByteString o m (Either e a, Word64)
runGet !g = (\(!r, !s) -> (r, decodingBytesRead s)) <$> runGetC (startDecoding 0) g
{-# INLINE runGet #-}

-- | Get the total number of bytes read to this point.
bytesRead :: Get o e Word64
bytesRead = getC $ \ !x -> return (Right $ decodingBytesRead x, x)
{-# INLINE bytesRead #-}

-- | Run the given 'S.Get' monad from binary package
-- and convert result into 'Get'.
castGet :: S.Get a -> Get o String a
castGet !g =
  go (S.runGetIncremental g)
  where
    go (S.Done !t _ !r) = ungetChunk t >> return r
    go (S.Fail !t _ !e) = ungetChunk t >> throwError e
    go (S.Partial !c) = go =<< c <$> getChunk
{-# INLINE castGet #-}

-- | 'True' if there are no input elements left.
-- This function may remove empty leading chunks from the stream, but otherwise will not modify it.
endOfInput :: Get o e Bool
endOfInput =
  untilJust $ maybe
    (return $ Just True)
    (\i -> if SB.null i then return Nothing else ungetChunk i >> return (Just False))
    =<< getChunk
{-# INLINE endOfInput #-}

-- | 'onError' is 'mapError' with its arguments flipped.
onError :: Monad m => GetM o e m a -> (e -> e') -> GetM o e' m a
onError = flip mapError
{-# INLINE onError #-}

-- | Set decoder error. If the decoder fails, the given error will be used
-- as the error message.
withError :: Monad m => e -> GetM o () m a -> GetM o e m a
withError e = mapError (const e)
{-# INLINE withError #-}

-- | 'ifError' is 'withError' with its arguments flipped.
ifError :: Monad m => GetM o () m a -> e -> GetM o e m a
ifError = flip withError
{-# INLINE ifError #-}

-- | Map any error into '()'.
voidError :: Monad m => GetM o e m a -> GetM o () m a
voidError = mapError (const ())
{-# INLINE voidError #-}

-- | Skip ahead @n@ bytes. Fails if fewer than @n@ bytes are available.
skip :: Word64 -> Get o () ()
skip !n = do
  !consumed <- go 0
  if consumed < n
    then throwError ()
    else return ()
  where
    go consumed
      | consumed > n = error "Data.Binary.Conduit.Get.skip"
      | consumed == n = return n
      | otherwise = do
        !mi <- getChunk
        case mi of
          Nothing -> return consumed
          Just !i -> do
              let !gap = n - consumed
              if gap >= fromIntegral (SB.length i)
                then do
                  go $ consumed + fromIntegral (SB.length i)
                else do
                  ungetChunk $ SB.drop (fromIntegral gap) i
                  return n
{-# INLINE skip #-}

-- | Isolate a decoder to operate with a fixed number of bytes, and fail if
-- fewer bytes were consumed, or more bytes were attempted to be consumed.
-- Unlike 'Data.Binary.Get.isolate' from binary package,
-- offset from 'bytesRead' will NOT be relative to the start of 'isolate'.
isolate :: Monad m
  => Word64 -- ^ The number of bytes that must be consumed.
  -> e -- ^ The error if if fewer than @n@ bytes are available.
  -> (Word64 -> e) -- ^ The error if fewer than @n@ bytes were consumed.
  -> GetM o e m a -- ^ The decoder to isolate.
  -> GetM o e m a
isolate !n unexpected_eof f !g = do
  !o1 <- bytesRead
  !r <- getC $ flip runStateC $ runExceptC $ go 0 =$= (exceptC $ stateC $ flip runGetC $ g)
  !o2 <- bytesRead
  if o2 - o1 < n
    then throwError $ f $ o2 - o1
    else return r
  where
    go consumed
      | consumed > n = error "Data.Binary.Conduit.Get.isolate"
      | consumed == n = return ()
      | otherwise = do
          !i <- maybe (throwError unexpected_eof) return =<< await
          let !gap = n - consumed
          if gap >= fromIntegral (SB.length i)
            then do
              yield i
              go $ consumed + fromIntegral (SB.length i)
            else do
              let (!h, !t) = SB.splitAt (fromIntegral gap) i
              yield h
              leftover t
{-# INLINE isolate #-}

-- | An efficient get method for strict 'S.ByteString's. Fails if fewer than @n@
-- bytes are left in the input. If @n <= 0@ then the empty string is returned.
getByteString :: Int -> Get o () S.ByteString
getByteString !n = do
  go SB.empty
  where
    go consumed
      | SB.length consumed >= n = do
        let (!h, !t) = SB.splitAt n consumed
        if SB.null t then return () else ungetChunk t
        return h
      | otherwise = do
        !i <- maybe (throwError ()) return =<< getChunk
        go $ consumed <> i
{-# INLINE getByteString #-}

-- | An efficient get method for lazy 'ByteString's. Fails if fewer than @n@
-- bytes are left in the input. If @n <= 0@ then the empty string is returned.
getLazyByteString :: Int64 -> Get o () ByteString
getLazyByteString n = do
  go B.empty
  where
    go consumed
      | B.length consumed >= n = do
        let (!h, !t) = B.splitAt n consumed
        if B.null t then return () else forM_ (reverse $ B.toChunks t) ungetChunk
        return h
      | otherwise = do
        !i <- maybe (throwError ()) return =<< getChunk
        go $ consumed <> B.fromStrict i
{-# INLINE getLazyByteString #-}

-- | Get a lazy 'ByteString' that is terminated with a NUL byte.
-- The returned string does not contain the NUL byte.
-- Fails if it reaches the end of input without finding a NUL.
getLazyByteStringNul :: Get o () ByteString
getLazyByteStringNul =
  go B.empty
  where
  go consumed = do
    !i <- maybe (throwError ()) return =<< getChunk
    let (!h, !t) = SB.span (/= 0) i
    let !r = consumed <> B.fromStrict h
    if SB.length t == 0
      then go r
      else ungetChunk (SB.drop 1 t) >> return r

-- | Get the remaining bytes as a lazy 'ByteString'.
-- Note that this can be an expensive function to use as it
-- forces reading all input and keeping the string in-memory.
getRemainingLazyByteString :: Get o e ByteString
getRemainingLazyByteString =
  go B.empty
  where
  go consumed = do
    !mi <- getChunk
    case mi of
      Nothing -> return consumed
      Just !i -> go $ consumed <> B.fromStrict i

voidCastGet :: S.Get a -> Get o () a
voidCastGet = voidError . castGet
{-# INLINE voidCastGet #-}

-- | Read a 'Word8' from the monad state.
getWord8 :: Get o () Word8
getWord8 = voidCastGet S.getWord8
{-# INLINE getWord8 #-}

-- | Read an 'Int8' from the monad state.
getInt8 :: Get o () Int8
getInt8 = voidCastGet S.getInt8
{-# INLINE getInt8 #-}

-- | Read a 'Word16' in big endian format.
getWord16be :: Get o () Word16
getWord16be = voidCastGet S.getWord16be
{-# INLINE getWord16be #-}

-- | Read a 'Word32' in big endian format.
getWord32be :: Get o () Word32
getWord32be = voidCastGet S.getWord32be
{-# INLINE getWord32be #-}

-- | Read a 'Word64' in big endian format.
getWord64be :: Get o () Word64
getWord64be = voidCastGet S.getWord64be
{-# INLINE getWord64be #-}

-- | Read a 'Word16' in little endian format.
getWord16le :: Get o () Word16
getWord16le = voidCastGet S.getWord16le
{-# INLINE getWord16le #-}

-- | Read a 'Word32' in little endian format.
getWord32le :: Get o () Word32
getWord32le = voidCastGet S.getWord32le
{-# INLINE getWord32le #-}

-- | Read a 'Word64' in little endian format.
getWord64le :: Get o () Word64
getWord64le = voidCastGet S.getWord64le
{-# INLINE getWord64le #-}

-- | Read a single native machine word. The word is read in
-- host order, host endian form, for the machine you're on. On a 64 bit
-- machine the Word is an 8 byte value, on a 32 bit machine, 4 bytes.
getWordhost :: Get o () Word
getWordhost = voidCastGet S.getWordhost
{-# INLINE getWordhost #-}

-- | Read a 2 byte 'Word16' in native host order and host endianness.
getWord16host :: Get o () Word16
getWord16host = voidCastGet S.getWord16host
{-# INLINE getWord16host #-}

-- | Read a 4 byte 'Word32' in native host order and host endianness.
getWord32host :: Get o () Word32
getWord32host = voidCastGet S.getWord32host
{-# INLINE getWord32host #-}

-- | Read a 8 byte 'Word64' in native host order and host endianness.
getWord64host :: Get o () Word64
getWord64host = voidCastGet S.getWord64host
{-# INLINE getWord64host #-}

-- | Read an 'Int16' in big endian format.
getInt16be :: Get o () Int16
getInt16be = voidCastGet S.getInt16be
{-# INLINE getInt16be #-}

-- | Read an 'Int32' in big endian format.
getInt32be :: Get o () Int32
getInt32be = voidCastGet S.getInt32be
{-# INLINE getInt32be #-}

-- | Read an 'Int64' in big endian format.
getInt64be :: Get o () Int64
getInt64be = voidCastGet S.getInt64be
{-# INLINE getInt64be #-}

-- | Read an 'Int16' in little endian format.
getInt16le :: Get o () Int16
getInt16le = voidCastGet S.getInt16le
{-# INLINE getInt16le #-}

-- | Read an 'Int32' in little endian format.
getInt32le :: Get o () Int32
getInt32le = voidCastGet S.getInt32le
{-# INLINE getInt32le #-}

-- | Read an 'Int64' in little endian format.
getInt64le :: Get o () Int64
getInt64le = voidCastGet S.getInt64le
{-# INLINE getInt64le #-}

-- | Read a single native machine word. It works in the same way as 'getWordhost'.
getInthost :: Get o () Int
getInthost = voidCastGet S.getInthost
{-# INLINE getInthost #-}

-- | Read a 2 byte 'Int16' in native host order and host endianness.
getInt16host :: Get o () Int16
getInt16host = voidCastGet S.getInt16host
{-# INLINE getInt16host #-}

-- | Read a 4 byte 'Int32' in native host order and host endianness.
getInt32host :: Get o () Int32
getInt32host = voidCastGet S.getInt32host
{-# INLINE getInt32host #-}

-- | Read a 8 byte 'Int64' in native host order and host endianness.
getInt64host :: Get o () Int64
getInt64host = voidCastGet S.getInt64host
{-# INLINE getInt64host #-}

-- | Read a 'Float' in big endian IEEE-754 format.
getFloatbe :: Get o () Float
getFloatbe = voidCastGet S.getFloat32be
{-# INLINE getFloatbe #-}

-- | Read a 'Float' in little endian IEEE-754 format.
getFloatle :: Get o () Float
getFloatle = voidCastGet S.getFloat32le
{-# INLINE getFloatle #-}

-- | Read a 'Float' in IEEE-754 format and host endian.
getFloathost :: Get o () Float
getFloathost = wordToFloat <$> voidCastGet S.getWord32host
{-# INLINE getFloathost #-}

-- | Read a 'Double' in big endian IEEE-754 format.
getDoublebe :: Get o () Double
getDoublebe = voidCastGet S.getFloat64be
{-# INLINE getDoublebe #-}

-- | Read a 'Double' in little endian IEEE-754 format.
getDoublele :: Get o () Double
getDoublele = voidCastGet S.getFloat64le
{-# INLINE getDoublele #-}

-- | Read a 'Double' in IEEE-754 format and host endian.
getDoublehost :: Get o () Double
getDoublehost = wordToDouble <$> voidCastGet S.getWord64host
{-# INLINE getDoublehost #-}
