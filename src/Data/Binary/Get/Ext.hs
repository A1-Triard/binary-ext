module Data.Binary.Get.Ext
  ( ByteOffset
  , Get
  , runGet
  , bytesReaded
  , bytesReadedBefore
  , totalBytesReaded
  , markAsReaded
  , fromGet
  , mapError
  , onError
  , getWord8
  ) where

#include <haskell>

type ByteOffset = Word64

type Get e m = ConduitM S.ByteString Void (ExceptT e (ReaderT ByteOffset (StateT ByteOffset m)))

runGet :: Monad m => ByteOffset -> Get e m a -> ConduitM S.ByteString o m (Either e a, ByteOffset)
runGet bytes_readed_before = mapOutput absurd . runStateC 0 . runReaderC bytes_readed_before . runExceptC

bytesReaded :: Monad m => Get e m ByteOffset
bytesReaded = lift $ lift $ lift get

bytesReadedBefore :: Monad m => Get e m ByteOffset
bytesReadedBefore = lift $ lift ask

totalBytesReaded :: Monad m => Get e m ByteOffset
totalBytesReaded = (+) <$> bytesReadedBefore <*> bytesReaded

markAsReaded :: Monad m => ByteOffset -> Get e m ()
markAsReaded n = lift $ lift $ lift $ modify' (+ n)

fromGet :: Monad m => SG.Get a -> Get String m a
fromGet g =
  go (SG.runGetIncremental g)
  where
    go (SG.Done t o r) = leftover t >> markAsReaded (fromIntegral o) >> return r
    go (SG.Fail t o e) = leftover t >> markAsReaded (fromIntegral o) >> throwError e
    go (SG.Partial c) = go =<< c <$> await

mapError :: Monad m => (e -> e') -> Get e m a -> Get e' m a
mapError f g = exceptC $ either (Left . f) Right <$> runExceptC g

onError :: Monad m => Get () m a -> e -> Get e m a
onError g e = mapError (const e) g

getWord8 :: Monad m => Get () m Word8
getWord8 = mapError (const ()) $ fromGet SG.getWord8
