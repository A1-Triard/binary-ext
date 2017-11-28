module Data.Binary.Get.Ext
  ( ByteOffset
  , Get
  , runGet
  , bytesRead
  , markAsRead
  , fromGet
  , mapError
  , onError
  , withError
  , ifError
  , voidError
  , getWord8
  , getWord16be
  , isolateM
  , isolate
  ) where

#include <haskell>

type ByteOffset = Word64

type Get e m = ConduitM S.ByteString Void (ExceptT e (StateT ByteOffset m))

runGet :: Monad m => ByteOffset -> Get e m a -> ConduitM S.ByteString o m (Either e a, ByteOffset)
runGet bytes_read_before = mapOutput absurd . runStateC bytes_read_before . runExceptC

bytesRead :: Monad m => Get e m ByteOffset
bytesRead = lift $ lift get

markAsRead :: Monad m => ByteOffset -> Get e m ()
markAsRead n = lift $ lift $ modify' (+ n)

fromGet :: Monad m => SG.Get a -> Get String m a
fromGet g =
  go (SG.runGetIncremental g)
  where
    go (SG.Done t o r) = leftover t >> markAsRead (fromIntegral o) >> return r
    go (SG.Fail t o e) = leftover t >> markAsRead (fromIntegral o) >> throwError e
    go (SG.Partial c) = go =<< c <$> await

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

voidFromGet :: Monad m => SG.Get a -> Get () m a
voidFromGet = voidError . fromGet

getWord8 :: Monad m => Get () m Word8
getWord8 = voidFromGet SG.getWord8

getWord16be :: Monad m => Get () m Word16
getWord16be = voidFromGet SG.getWord16be

isolateM :: Monad m => ByteOffset -> ConduitM S.ByteString S.ByteString m ByteOffset
isolateM n0 =
  go 0
  where
    go r = do
      mi <- await
      case mi of
        Nothing -> return r
        Just i -> do
          let (!h, !t) = SB.splitAt (fromIntegral $ n0 - r) i
          let !rn = r + fromIntegral (SB.length h)
          if SB.null h then return () else yield h
          if SB.null t then go rn else leftover t >> return rn

isolate :: Monad m => ByteOffset -> Get e m a -> (ByteOffset -> e) -> Get e m a
isolate n0 g f = do
  (!consumed, r) <- fuseBoth (isolateM n0) g
  if consumed < n0
    then throwError $ f consumed
    else return r
