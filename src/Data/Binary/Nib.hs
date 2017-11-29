module Data.Binary.Nib
  ( ByteOffset
  , Nib
  , runNib
  , bytesNiben
  , markAsNiben
  , nibGet
  , mapError
  , onError
  , withError
  , ifError
  , voidError
  , nibWord8
  , nibWord16be
  , nibWord32be
  , nibWord64be
  , nibWord16le
  , nibWord32le
  , nibWord64le
  , nibWordhost
  , nibWord16host
  , nibWord32host
  , nibWord64host
  , nibInthost
  , nibInt16host
  , nibInt32host
  , nibInt64host
  , nibFloatbe
  , nibFloatle
  , nibFloathost
  , nibDoublebe
  , nibDoublele
  , nibDoublehost
  , isolateM
  , isolateN
  ) where

#include <haskell>

type Nib e m = ConduitM S.ByteString Void (ExceptT e (StateT ByteOffset m))

runNib :: Monad m => ByteOffset -> Nib e m a -> ConduitM S.ByteString o m (Either e a, ByteOffset)
runNib bytes_read_before = mapOutput absurd . runStateC bytes_read_before . runExceptC

bytesNiben :: Monad m => Nib e m ByteOffset
bytesNiben = lift $ lift get

markAsNiben :: Monad m => ByteOffset -> Nib e m ()
markAsNiben n = lift $ lift $ modify' (+ n)

nibGet :: Monad m => Get a -> Nib String m a
nibGet g =
  go (runGetIncremental g)
  where
    go (Done t o r) = leftover t >> markAsNiben (fromIntegral o) >> return r
    go (Fail t o e) = leftover t >> markAsNiben (fromIntegral o) >> throwError e
    go (Partial c) = go =<< c <$> await

-- | Convert decoder error. If the decoder fails, the given function will be applied
-- to the error message.
mapError :: Monad m => (e -> e') -> Nib e m a -> Nib e' m a
mapError f g = exceptC $ either (Left . f) Right <$> runExceptC g

-- | 'onError' is 'mapError' with its arguments flipped.
onError :: Monad m => Nib e m a -> (e -> e') -> Nib e' m a
onError = flip mapError

-- | Set decoder error. If the decoder fails, the given error will be used
-- as the error message.
withError :: Monad m => e -> Nib () m a -> Nib e m a
withError e = mapError (const e)

-- | 'ifError' is 'withError' with its arguments flipped.
ifError :: Monad m => Nib () m a -> e -> Nib e m a
ifError = flip withError

voidError :: Monad m => Nib e m a -> Nib () m a
voidError = mapError (const ())

voidNibGet :: Monad m => Get a -> Nib () m a
voidNibGet = voidError . nibGet

nibWord8 :: Monad m => Nib () m Word8
nibWord8 = voidNibGet getWord8

nibWord16be :: Monad m => Nib () m Word16
nibWord16be = voidNibGet getWord16be

nibWord32be :: Monad m => Nib () m Word32
nibWord32be = voidNibGet getWord32be

nibWord64be :: Monad m => Nib () m Word64
nibWord64be = voidNibGet getWord64be

nibWord16le :: Monad m => Nib () m Word16
nibWord16le = voidNibGet getWord16le

nibWord32le :: Monad m => Nib () m Word32
nibWord32le = voidNibGet getWord32le

nibWord64le :: Monad m => Nib () m Word64
nibWord64le = voidNibGet getWord64le

nibWordhost :: Monad m => Nib () m Word
nibWordhost = voidNibGet getWordhost

nibWord16host :: Monad m => Nib () m Word16
nibWord16host = voidNibGet getWord16host

nibWord32host :: Monad m => Nib () m Word32
nibWord32host = voidNibGet getWord32host

nibWord64host :: Monad m => Nib () m Word64
nibWord64host = voidNibGet getWord64host

nibInthost :: Monad m => Nib () m Int
nibInthost = voidNibGet getInthost

nibInt16host :: Monad m => Nib () m Int16
nibInt16host = voidNibGet getInt16host

nibInt32host :: Monad m => Nib () m Int32
nibInt32host = voidNibGet getInt32host

nibInt64host :: Monad m => Nib () m Int64
nibInt64host = voidNibGet getInt64host

nibFloatbe :: Monad m => Nib () m Float
nibFloatbe = voidNibGet getFloat32be

nibFloatle :: Monad m => Nib () m Float
nibFloatle = voidNibGet getFloat32le

nibFloathost :: Monad m => Nib () m Float
nibFloathost = wordToFloat <$> voidNibGet getWord32host

nibDoublebe :: Monad m => Nib () m Double
nibDoublebe = voidNibGet getFloat64be

nibDoublele :: Monad m => Nib () m Double
nibDoublele = voidNibGet getFloat64le

nibDoublehost :: Monad m => Nib () m Double
nibDoublehost = wordToDouble <$> voidNibGet getWord64host

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

isolateN :: Monad m => ByteOffset -> Nib e m a -> (ByteOffset -> e) -> Nib e m a
isolateN n0 g f = do
  (!consumed, r) <- fuseBoth (isolateM n0) g
  if consumed < n0
    then throwError $ f consumed
    else return r
