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
  , TrackGet
  , trackGetBefore
  , trackBytesRead
  , trackGetAwait
  , trackGetLeftover
  , GetC
  , GetInp
  , Get
  , runGetC
  , getC
  , getInp
  , ungetInp
  , mapError
  , runGet
  , bytesRead
  , castGet
  , getNull
  , onError
  , withError
  , ifError
  , voidError
--  , skip
--  , isolate
  , getByteString
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
runGet g = (\(!r, !s) -> (r, trackBytesRead s)) <$> runGetC g (trackGetBefore 0)
{-# INLINE runGet #-}

-- | Get the total number of bytes read to this point.
bytesRead :: Monad m => Get o e m ByteOffset
bytesRead = getC $ \ !x -> return (Right $ trackBytesRead x, x)
{-# INLINE bytesRead #-}

-- | Run the given 'S.Get' monad from binary package
-- and convert result into 'Get'.
castGet :: Monad m => S.Get a -> Get o String m a
castGet g =
  go (S.runGetIncremental g)
  where
    go (S.Done !t _ !r) = ungetInp t >> return r
    go (S.Fail !t _ !e) = ungetInp t >> throwError e
    go (S.Partial !c) = go =<< c <$> getInp
{-# INLINE castGet #-}

-- | 'True' if there are no input elements left.
-- This function may remove empty leading chunks from the stream, but otherwise will not modify it.
getNull :: Monad m => Get o e m Bool
getNull =
  untilJust $ maybe
    (return $ Just True)
    (\i -> if SB.null i then return Nothing else ungetInp i >> return (Just False))
    =<< getInp
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

{-
skipM :: Monad m => ByteOffset -> ConduitM GetInp o m ByteOffset
skipM n =
  go 0
  where
    go consumed = do
      !mi <- await
      case mi of
        Nothing -> return consumed
        Just (GetInp !i) -> do
          let !next = consumed + fromIntegral (SB.length i)
          if next < n
            then go next
            else leftover (GetInp $ SB.drop (fromIntegral $ n - consumed) i) >> return n
{-# INLINE skipM #-}

-- | Skip ahead @n@ bytes. Fails if fewer than @n@ bytes are available.
skip :: Monad m => ByteOffset -> Get o () m ()
skip n = do
  !consumed <- skipM n
  markAsRead consumed
  if consumed < n
    then throwError ()
    else return ()
{-# INLINE skip #-}

isolateM :: Monad m => ByteOffset -> ConduitM GetInp GetInp m ByteOffset
isolateM n =
  go 0
  where
    go consumed = do
      !mi <- await
      case mi of
        Nothing -> return consumed
        Just (GetInp !i) -> do
          let (!h, !t) = SB.splitAt (fromIntegral $ n - consumed) i
          let !next = consumed + fromIntegral (SB.length h)
          if SB.null h then return () else yield (GetInp h)
          if next < n
            then go next
            else leftover (GetInp t) >> return n
{-# INLINE isolateM #-}

-- | Isolate a decoder to operate with a fixed number of bytes, and fail if
-- fewer bytes were consumed, or more bytes were attempted to be consumed.
-- Unlike 'Data.Binary.Get.isolate' from binary package,
-- offset from 'bytesRead' will NOT be relative to the start of 'isolate'.
isolate :: Monad m
  => ByteOffset -- ^ The number of bytes that must be consumed
  -> Get o e m a -- ^ The decoder to isolate
  -> (ByteOffset -> e) -- ^ The error if fewer bytes were consumed
  -> Get o e m a
isolate n g f = do
  (!consumed, !r) <- fuseBoth (isolateM n) g
  if consumed < n
    then throwError $ f consumed
    else return r
{-# INLINE isolate #-}
-}

-- | An efficient get method for strict 'ByteString's. Fails if fewer than @n@
-- bytes are left in the input. If @n <= 0@ then the empty string is returned.
getByteString :: Monad m => Int -> Get o () m S.ByteString
getByteString n = do
  go SB.empty
  where
    go consumed
      | SB.length consumed >= n = do
        let (!h, !t) = SB.splitAt n consumed
        if SB.null t then return () else ungetInp t
        return h
      | otherwise = do
        !mi <- getInp
        case mi of
          Nothing -> throwError ()
          Just !i -> go $ consumed <> i
{-# INLINE getByteString #-}

{-
    , getLazyByteString
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

-- | Read a 'Int8' from the monad state.
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
