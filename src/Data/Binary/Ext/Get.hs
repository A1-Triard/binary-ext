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

-- | At the first look, Data.Binary.Ext.Get module is very similar with Data.Binary.Get.
-- The main differences between them are the following.
-- While the 'S.Get' from binary is a very custom monad,
-- the local 'Get' is 'ConduitM', which leads to easy integration in complicated format parsing.
-- The Data.Binary.Get module does not have a function to create custom 'S.Get' monad,
-- this module provides 'getC'.
-- Unlike 'isolate' from binary, local 'isolate' does not "cut" bytes counter.
-- While the binary's 'S.Get' is 'MonadFail', which leads to very ugly errors handling
-- in complicated cases, local 'Get' is 'MonadError'.

module Data.Binary.Ext.Get
  ( ByteOffset
  , Decoding
  , startDecoding
  , decodingBytesRead
  , decodingGot
  , decodingUngot
  , GetC
  , ByteChunk
  , Get
  , runGetC
  , getC
  , getChunk
  , ungetChunk
  , yieldChunk
  , yieldChunkOr
  , mapError
  , runGet
  , bytesRead
  , castGet
  , getNull
  , onError
  , withError
  , ifError
  , voidError
  , skip
  , isolate
  , getByteString
  , getLazyByteString
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

import Data.Binary.Ext.Get.GetC

-- | Run a decoder presented as a 'Get' monad.
-- Returns decoder result and consumed bytes count.
runGet :: Monad m => Get o e m a -> ConduitM S.ByteString o m (Either e a, ByteOffset)
runGet !g = (\(!r, !s) -> (r, decodingBytesRead s)) <$> runGetC (startDecoding 0) g
{-# INLINE runGet #-}

-- | Get the total number of bytes read to this point.
bytesRead :: Monad m => Get o e m ByteOffset
bytesRead = getC $ \ !x -> return (Right $ decodingBytesRead x, x)
{-# INLINE bytesRead #-}

-- | Run the given 'S.Get' monad from binary package
-- and convert result into 'Get'.
castGet :: Monad m => S.Get a -> Get o String m a
castGet !g =
  go (S.runGetIncremental g)
  where
    go (S.Done !t _ !r) = ungetChunk t >> return r
    go (S.Fail !t _ !e) = ungetChunk t >> throwError e
    go (S.Partial !c) = go =<< c <$> getChunk
{-# INLINE castGet #-}

-- | 'True' if there are no input elements left.
-- This function may remove empty leading chunks from the stream, but otherwise will not modify it.
getNull :: Monad m => Get o e m Bool
getNull =
  untilJust $ maybe
    (return $ Just True)
    (\i -> if SB.null i then return Nothing else ungetChunk i >> return (Just False))
    =<< getChunk
{-# INLINE getNull #-}

-- | 'onError' is 'mapError' with its arguments flipped.
onError :: Monad m => Get o e m a -> (e -> e') -> Get o e' m a
onError = flip mapError
{-# INLINE onError #-}

-- | Set decoder error. If the decoder fails, the given error will be used
-- as the error message.
withError :: Monad m => e -> Get o () m a -> Get o e m a
withError e = mapError (const e)
{-# INLINE withError #-}

-- | 'ifError' is 'withError' with its arguments flipped.
ifError :: Monad m => Get o () m a -> e -> Get o e m a
ifError = flip withError
{-# INLINE ifError #-}

-- | Map any error into '()'.
voidError :: Monad m => Get o e m a -> Get o () m a
voidError = mapError (const ())
{-# INLINE voidError #-}

-- | Skip ahead @n@ bytes. Fails if fewer than @n@ bytes are available.
skip :: Monad m => ByteOffset -> Get o () m ()
skip !n = do
  !consumed <- go 0
  if consumed < n
    then throwError ()
    else return ()
  where
    go consumed
      | consumed > n = error "Data.Binary.Ext.Get.skip"
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
  => ByteOffset -- ^ The number of bytes that must be consumed.
  -> e -- ^ The error if if fewer than @n@ bytes are available.
  -> (ByteOffset -> e) -- ^ The error if fewer than @n@ bytes were consumed.
  -> Get o e m a -- ^ The decoder to isolate.
  -> Get o e m a
isolate !n unexpected_eof f !g = do
  !o1 <- bytesRead
  !r <- go 0 =$= g
  !o2 <- bytesRead
  if o2 - o1 < n
    then throwError $ f $ o2 - o1
    else return r
  where
    go consumed
      | consumed > n = error "Data.Binary.Ext.Get.isolate"
      | consumed == n = return ()
      | otherwise = do
          !mi <- getChunk
          case mi of
            Nothing -> throwError unexpected_eof
            Just !i -> do
              let !gap = n - consumed
              if gap >= fromIntegral (SB.length i)
                then do
                  yieldChunk i
                  go $ consumed + fromIntegral (SB.length i)
                else do
                  let (!h, !t) = SB.splitAt (fromIntegral gap) i
                  yieldChunk h
                  ungetChunk t
{-# INLINE isolate #-}

-- | An efficient get method for strict 'S.ByteString's. Fails if fewer than @n@
-- bytes are left in the input. If @n <= 0@ then the empty string is returned.
getByteString :: Monad m => Int -> Get o () m S.ByteString
getByteString !n = do
  go SB.empty
  where
    go consumed
      | SB.length consumed >= n = do
        let (!h, !t) = SB.splitAt n consumed
        if SB.null t then return () else ungetChunk t
        return h
      | otherwise = do
        !mi <- getChunk
        case mi of
          Nothing -> throwError ()
          Just !i -> go $ consumed <> i
{-# INLINE getByteString #-}

-- | An efficient get method for lazy 'ByteString's. Fails if fewer than @n@
-- bytes are left in the input. If @n <= 0@ then the empty string is returned.
getLazyByteString :: Monad m => Int64 -> Get o () m ByteString
getLazyByteString n = do
  go B.empty
  where
    go consumed
      | B.length consumed >= n = do
        let (!h, !t) = B.splitAt n consumed
        if B.null t then return () else forM_ (reverse $ B.toChunks t) ungetChunk
        return h
      | otherwise = do
        !mi <- getChunk
        case mi of
          Nothing -> throwError ()
          Just !i -> go $ consumed <> B.fromStrict i
{-# INLINE getLazyByteString #-}

{-
    , getLazyByteStringNul
    , getRemainingLazyByteString
-}

voidCastGet :: Monad m => S.Get a -> Get o () m a
voidCastGet = voidError . castGet
{-# INLINE voidCastGet #-}

-- | Read a 'Word8' from the monad state.
getWord8 :: Monad m => Get o () m Word8
getWord8 = voidCastGet S.getWord8
{-# INLINE getWord8 #-}

-- | Read an 'Int8' from the monad state.
getInt8 :: Monad m => Get o () m Int8
getInt8 = voidCastGet S.getInt8
{-# INLINE getInt8 #-}

-- | Read a 'Word16' in big endian format.
getWord16be :: Monad m => Get o () m Word16
getWord16be = voidCastGet S.getWord16be
{-# INLINE getWord16be #-}

-- | Read a 'Word32' in big endian format.
getWord32be :: Monad m => Get o () m Word32
getWord32be = voidCastGet S.getWord32be
{-# INLINE getWord32be #-}

-- | Read a 'Word64' in big endian format.
getWord64be :: Monad m => Get o () m Word64
getWord64be = voidCastGet S.getWord64be
{-# INLINE getWord64be #-}

-- | Read a 'Word16' in little endian format.
getWord16le :: Monad m => Get o () m Word16
getWord16le = voidCastGet S.getWord16le
{-# INLINE getWord16le #-}

-- | Read a 'Word32' in little endian format.
getWord32le :: Monad m => Get o () m Word32
getWord32le = voidCastGet S.getWord32le
{-# INLINE getWord32le #-}

-- | Read a 'Word64' in little endian format.
getWord64le :: Monad m => Get o () m Word64
getWord64le = voidCastGet S.getWord64le
{-# INLINE getWord64le #-}

-- | Read a single native machine word. The word is read in
-- host order, host endian form, for the machine you're on. On a 64 bit
-- machine the Word is an 8 byte value, on a 32 bit machine, 4 bytes.
getWordhost :: Monad m => Get o () m Word
getWordhost = voidCastGet S.getWordhost
{-# INLINE getWordhost #-}

-- | Read a 2 byte 'Word16' in native host order and host endianness.
getWord16host :: Monad m => Get o () m Word16
getWord16host = voidCastGet S.getWord16host
{-# INLINE getWord16host #-}

-- | Read a 4 byte 'Word32' in native host order and host endianness.
getWord32host :: Monad m => Get o () m Word32
getWord32host = voidCastGet S.getWord32host
{-# INLINE getWord32host #-}

-- | Read a 8 byte 'Word64' in native host order and host endianness.
getWord64host :: Monad m => Get o () m Word64
getWord64host = voidCastGet S.getWord64host
{-# INLINE getWord64host #-}

-- | Read an 'Int16' in big endian format.
getInt16be :: Monad m => Get o () m Int16
getInt16be = voidCastGet S.getInt16be
{-# INLINE getInt16be #-}

-- | Read an 'Int32' in big endian format.
getInt32be :: Monad m => Get o () m Int32
getInt32be = voidCastGet S.getInt32be
{-# INLINE getInt32be #-}

-- | Read an 'Int64' in big endian format.
getInt64be :: Monad m => Get o () m Int64
getInt64be = voidCastGet S.getInt64be
{-# INLINE getInt64be #-}

-- | Read an 'Int16' in little endian format.
getInt16le :: Monad m => Get o () m Int16
getInt16le = voidCastGet S.getInt16le
{-# INLINE getInt16le #-}

-- | Read an 'Int32' in little endian format.
getInt32le :: Monad m => Get o () m Int32
getInt32le = voidCastGet S.getInt32le
{-# INLINE getInt32le #-}

-- | Read an 'Int64' in little endian format.
getInt64le :: Monad m => Get o () m Int64
getInt64le = voidCastGet S.getInt64le
{-# INLINE getInt64le #-}

-- | Read a single native machine word. It works in the same way as 'getWordhost'.
getInthost :: Monad m => Get o () m Int
getInthost = voidCastGet S.getInthost
{-# INLINE getInthost #-}

-- | Read a 2 byte 'Int16' in native host order and host endianness.
getInt16host :: Monad m => Get o () m Int16
getInt16host = voidCastGet S.getInt16host
{-# INLINE getInt16host #-}

-- | Read a 4 byte 'Int32' in native host order and host endianness.
getInt32host :: Monad m => Get o () m Int32
getInt32host = voidCastGet S.getInt32host
{-# INLINE getInt32host #-}

-- | Read a 8 byte 'Int64' in native host order and host endianness.
getInt64host :: Monad m => Get o () m Int64
getInt64host = voidCastGet S.getInt64host
{-# INLINE getInt64host #-}

-- | Read a 'Float' in big endian IEEE-754 format.
getFloatbe :: Monad m => Get o () m Float
getFloatbe = voidCastGet S.getFloat32be
{-# INLINE getFloatbe #-}

-- | Read a 'Float' in little endian IEEE-754 format.
getFloatle :: Monad m => Get o () m Float
getFloatle = voidCastGet S.getFloat32le
{-# INLINE getFloatle #-}

-- | Read a 'Float' in IEEE-754 format and host endian.
getFloathost :: Monad m => Get o () m Float
getFloathost = wordToFloat <$> voidCastGet S.getWord32host
{-# INLINE getFloathost #-}

-- | Read a 'Double' in big endian IEEE-754 format.
getDoublebe :: Monad m => Get o () m Double
getDoublebe = voidCastGet S.getFloat64be
{-# INLINE getDoublebe #-}

-- | Read a 'Double' in little endian IEEE-754 format.
getDoublele :: Monad m => Get o () m Double
getDoublele = voidCastGet S.getFloat64le
{-# INLINE getDoublele #-}

-- | Read a 'Double' in IEEE-754 format and host endian.
getDoublehost :: Monad m => Get o () m Double
getDoublehost = wordToDouble <$> voidCastGet S.getWord64host
{-# INLINE getDoublehost #-}
