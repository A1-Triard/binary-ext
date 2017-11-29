module Data.Binary.Ext.Get
  ( ByteOffset
  , Get
  , runGet
  , bytesGeten
  , markAsGeten
  , castGet
  , mapError
  , onError
  , withError
  , ifError
  , voidError
  , getWord8
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
  , isolateM
  , isolateN
  ) where

#include <haskell>

import Data.Binary.Get (ByteOffset)

type Get e m = ConduitM S.ByteString Void (ExceptT e (StateT ByteOffset m))

runGet :: Monad m => ByteOffset -> Get e m a -> ConduitM S.ByteString o m (Either e a, ByteOffset)
runGet bytes_read_before = mapOutput absurd . runStateC bytes_read_before . runExceptC

bytesGeten :: Monad m => Get e m ByteOffset
bytesGeten = lift $ lift get

markAsGeten :: Monad m => ByteOffset -> Get e m ()
markAsGeten n = lift $ lift $ modify' (+ n)

castGet :: Monad m => S.Get a -> Get String m a
castGet g =
  go (S.runGetIncremental g)
  where
    go (S.Done t o r) = leftover t >> markAsGeten (fromIntegral o) >> return r
    go (S.Fail t o e) = leftover t >> markAsGeten (fromIntegral o) >> throwError e
    go (S.Partial c) = go =<< c <$> await

-- | Convert decoder error. If the decoder fails, the given function will be applied
-- to the error message.
mapError :: Monad m => (e -> e') -> Get e m a -> Get e' m a
mapError f g = exceptC $ either (Left . f) Right <$> runExceptC g

-- | 'onError' is 'mapError' with its arguments flipped.
onError :: Monad m => Get e m a -> (e -> e') -> Get e' m a
onError = flip mapError

-- | Set decoder error. If the decoder fails, the given error will be used
-- as the error message.
withError :: Monad m => e -> Get () m a -> Get e m a
withError e = mapError (const e)

-- | 'ifError' is 'withError' with its arguments flipped.
ifError :: Monad m => Get () m a -> e -> Get e m a
ifError = flip withError

voidError :: Monad m => Get e m a -> Get () m a
voidError = mapError (const ())

{-
skipN :: Monad m => ByteOffset -> ConduitM S.ByteString o m ()
skipN n =
  go 0
  where
    go consumed = do
      mi <- await
-}

isolateM :: Monad m => ByteOffset -> ConduitM S.ByteString S.ByteString m ByteOffset
isolateM n0 =
  go 0
  where
    go consumed = do
      mi <- await
      case mi of
        Nothing -> return consumed
        Just i -> do
          let (!h, !t) = SB.splitAt (fromIntegral $ n0 - consumed) i
          let !n = consumed + fromIntegral (SB.length h)
          if SB.null h then return () else yield h
          if SB.null t then go n else leftover t >> return n

isolateN :: Monad m => ByteOffset -> Get e m a -> (ByteOffset -> e) -> Get e m a
isolateN n0 g f = do
  (!consumed, r) <- fuseBoth (isolateM n0) g
  if consumed < n0
    then throwError $ f consumed
    else return r

voidCastGet :: Monad m => S.Get a -> Get () m a
voidCastGet = voidError . castGet

getWord8 :: Monad m => Get () m Word8
getWord8 = voidCastGet S.getWord8

getWord16be :: Monad m => Get () m Word16
getWord16be = voidCastGet S.getWord16be

getWord32be :: Monad m => Get () m Word32
getWord32be = voidCastGet S.getWord32be

getWord64be :: Monad m => Get () m Word64
getWord64be = voidCastGet S.getWord64be

getWord16le :: Monad m => Get () m Word16
getWord16le = voidCastGet S.getWord16le

getWord32le :: Monad m => Get () m Word32
getWord32le = voidCastGet S.getWord32le

getWord64le :: Monad m => Get () m Word64
getWord64le = voidCastGet S.getWord64le

getWordhost :: Monad m => Get () m Word
getWordhost = voidCastGet S.getWordhost

getWord16host :: Monad m => Get () m Word16
getWord16host = voidCastGet S.getWord16host

getWord32host :: Monad m => Get () m Word32
getWord32host = voidCastGet S.getWord32host

getWord64host :: Monad m => Get () m Word64
getWord64host = voidCastGet S.getWord64host

getInthost :: Monad m => Get () m Int
getInthost = voidCastGet S.getInthost

getInt16host :: Monad m => Get () m Int16
getInt16host = voidCastGet S.getInt16host

getInt32host :: Monad m => Get () m Int32
getInt32host = voidCastGet S.getInt32host

getInt64host :: Monad m => Get () m Int64
getInt64host = voidCastGet S.getInt64host

getFloatbe :: Monad m => Get () m Float
getFloatbe = voidCastGet S.getFloat32be

getFloatle :: Monad m => Get () m Float
getFloatle = voidCastGet S.getFloat32le

getFloathost :: Monad m => Get () m Float
getFloathost = wordToFloat <$> voidCastGet S.getWord32host

getDoublebe :: Monad m => Get () m Double
getDoublebe = voidCastGet S.getFloat64be

getDoublele :: Monad m => Get () m Double
getDoublele = voidCastGet S.getFloat64le

getDoublehost :: Monad m => Get () m Double
getDoublehost = wordToDouble <$> voidCastGet S.getWord64host
