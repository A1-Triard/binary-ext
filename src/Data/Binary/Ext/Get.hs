module Data.Binary.Ext.Get
  ( ByteOffset
  , GetC
  , Get
  , runGet
  , bytesRead
  , markAsRead
  , castGet
  , mapError
  , onError
  , withError
  , ifError
  , voidError
  , select
  , skip
  , isolate
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

-- | An offset, counted in bytes.
type ByteOffset = Word64

newtype GetC e m a = C { runC :: ExceptT e (StateT ByteOffset m) a } deriving Generic

instance MonadTrans (GetC e) where
  lift = C . lift . lift
deriving instance Monad m => Monad (GetC e m)
deriving instance Functor m => Functor (GetC e m)
deriving instance MonadFix m => MonadFix (GetC e m)
deriving instance MonadFail m => MonadFail (GetC e m)
deriving instance (Functor m, Monad m) => Applicative (GetC e m)
deriving instance MonadIO m => MonadIO (GetC e m)
deriving instance (Functor m, Monad m, Monoid e) => Alternative (GetC e m)
deriving instance (Monad m, Monoid e) => MonadPlus (GetC e m)
deriving instance Monad m => MonadError e (GetC e m)

-- | A 'ConduitM' with internal transformers supposed to a binary deserialization.
type Get e m = ConduitM S.ByteString Void (GetC e m)

trackM :: Monad m => ConduitM S.ByteString S.ByteString (StateT [S.ByteString] m) ()
trackM =
  go
  where
  go = do
    mi <- await
    case mi of
      Nothing -> return ()
      Just i -> lift (modify' (\t -> i : t)) >> yield i

track :: Monad m => ConduitM S.ByteString o m a -> ConduitM S.ByteString o m (a, [S.ByteString])
track g = do
  ((_, r), consumed) <- runStateC [] $ fuseBothMaybe trackM $ stateC $ \x -> (\t -> (t, x)) <$> g
  return (r, consumed)

-- | Try run two alternative decoders. Returns result of succeed decoder.
-- If both decoders fail, return combined error.
select :: Monad m
  => Get e m a -- ^ First alternative decoder.
  -> Get e m a -- ^ Second alternative decoder.
  -> (e -> e -> e) -- ^ Function combining decoders errors into one error.
  -> Get e m a
select u v both = transPipe C $ exceptC $ do
  (r1, t1) <- track (runExceptC $ transPipe runC u)
  case r1 of
    Right a1 -> return $ Right a1
    Left e1 -> do
      mapM_ leftover t1
      (r2, t2) <- track (runExceptC $ transPipe runC v)
      case r2 of
        Right a2 -> return $ Right a2
        Left e2 -> do
          mapM_ leftover t2
          return $ Left $ both e1 e2

-- | Run a 'Get' monad, converting all internal transformers into a 'ConduitM' result.
runGet :: Monad m => ByteOffset -> Get e m a -> ConduitM S.ByteString o m (Either e a, ByteOffset)
runGet bytes_read_before = mapOutput absurd . runStateC bytes_read_before . runExceptC . transPipe runC

-- | Get the total number of bytes read to this point.
bytesRead :: Monad m => Get e m ByteOffset
bytesRead = lift $ C $ lift get

-- | Move 'bytesRead' counter forward by @n@ bytes.
markAsRead :: Monad m => ByteOffset -> Get e m ()
markAsRead n = lift $ C $ lift $ modify' (+ n)

-- | Run the given 'Data.Binary.Get.Get' monad from binary package
-- and convert result into 'Get'.
castGet :: Monad m => S.Get a -> Get String m a
castGet g =
  go (S.runGetIncremental g)
  where
    go (S.Done t !o !r) = leftover t >> markAsRead (fromIntegral o) >> return r
    go (S.Fail t !o !e) = leftover t >> markAsRead (fromIntegral o) >> throwError e
    go (S.Partial !c) = go =<< c <$> await

-- | Convert decoder error. If the decoder fails, the given function will be applied
-- to the error message.
mapError :: Monad m => (e -> e') -> Get e m a -> Get e' m a
mapError f g = transPipe C $ exceptC $ either (Left . f) Right <$> runExceptC (transPipe runC g)

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

-- | Map any error into '()'.
voidError :: Monad m => Get e m a -> Get () m a
voidError = mapError (const ())

skipM :: Monad m => ByteOffset -> ConduitM S.ByteString o m ByteOffset
skipM n =
  go 0
  where
    go consumed = do
      !mi <- await
      case mi of
        Nothing -> return consumed
        Just !i -> do
          let !next = consumed + fromIntegral (SB.length i)
          if next < n
            then go next
            else leftover (SB.drop (fromIntegral $ n - consumed) i) >> return n

-- | Skip ahead @n@ bytes. Fails if fewer than @n@ bytes are available.
skip :: Monad m => ByteOffset -> Get () m ()
skip n = do
  !consumed <- skipM n
  if consumed < n
    then throwError ()
    else return ()

isolateM :: Monad m => ByteOffset -> ConduitM S.ByteString S.ByteString m ByteOffset
isolateM n =
  go 0
  where
    go consumed = do
      !mi <- await
      case mi of
        Nothing -> return consumed
        Just !i -> do
          let (!h, !t) = SB.splitAt (fromIntegral $ n - consumed) i
          let !next = consumed + fromIntegral (SB.length h)
          if SB.null h then return () else yield h
          if next < n
            then go next
            else leftover t >> return n

-- | Isolate a decoder to operate with a fixed number of bytes, and fail if
-- fewer bytes were consumed, or more bytes were attempted to be consumed.
-- Unlike 'Data.Binary.Get.isolate' from binary package,
-- offset from 'bytesRead' will NOT be relative to the start of 'isolate'.
isolate :: Monad m
  => ByteOffset -- ^ The number of bytes that must be consumed
  -> Get e m a -- ^ The decoder to isolate
  -> (ByteOffset -> e) -- ^ The error if fewer bytes were consumed
  -> Get e m a
isolate n g f = do
  (!consumed, !r) <- fuseBoth (isolateM n) g
  if consumed < n
    then throwError $ f consumed
    else return r


{-
    , getByteString
    , getLazyByteString
    , getLazyByteStringNul
    , getRemainingLazyByteString
-}


voidCastGet :: Monad m => S.Get a -> Get () m a
voidCastGet = voidError . castGet

-- | Read a 'Word8' from the monad state.
getWord8 :: Monad m => Get () m Word8
getWord8 = voidCastGet S.getWord8

-- | Read a 'Int8' from the monad state.
getInt8 :: Monad m => Get () m Int8
getInt8 = voidCastGet S.getInt8

-- | Read a 'Word16' in big endian format.
getWord16be :: Monad m => Get () m Word16
getWord16be = voidCastGet S.getWord16be

-- | Read a 'Word32' in big endian format.
getWord32be :: Monad m => Get () m Word32
getWord32be = voidCastGet S.getWord32be

-- | Read a 'Word64' in big endian format.
getWord64be :: Monad m => Get () m Word64
getWord64be = voidCastGet S.getWord64be

-- | Read a 'Word16' in little endian format.
getWord16le :: Monad m => Get () m Word16
getWord16le = voidCastGet S.getWord16le

-- | Read a 'Word32' in little endian format.
getWord32le :: Monad m => Get () m Word32
getWord32le = voidCastGet S.getWord32le

-- | Read a 'Word64' in little endian format.
getWord64le :: Monad m => Get () m Word64
getWord64le = voidCastGet S.getWord64le

-- | Read a single native machine word. The word is read in
-- host order, host endian form, for the machine you're on. On a 64 bit
-- machine the Word is an 8 byte value, on a 32 bit machine, 4 bytes.
getWordhost :: Monad m => Get () m Word
getWordhost = voidCastGet S.getWordhost

-- | Read a 2 byte 'Word16' in native host order and host endianness.
getWord16host :: Monad m => Get () m Word16
getWord16host = voidCastGet S.getWord16host

-- | Read a 4 byte 'Word32' in native host order and host endianness.
getWord32host :: Monad m => Get () m Word32
getWord32host = voidCastGet S.getWord32host

-- | Read a 8 byte 'Word64' in native host order and host endianness.
getWord64host :: Monad m => Get () m Word64
getWord64host = voidCastGet S.getWord64host

-- | Read a single native machine word. It works in the same way as 'getWordhost'.
getInthost :: Monad m => Get () m Int
getInthost = voidCastGet S.getInthost

-- | Read a 2 byte 'Int16' in native host order and host endianness.
getInt16host :: Monad m => Get () m Int16
getInt16host = voidCastGet S.getInt16host

-- | Read a 4 byte 'Int32' in native host order and host endianness.
getInt32host :: Monad m => Get () m Int32
getInt32host = voidCastGet S.getInt32host

-- | Read a 8 byte 'Int64' in native host order and host endianness.
getInt64host :: Monad m => Get () m Int64
getInt64host = voidCastGet S.getInt64host

-- | Read a 'Float' in big endian IEEE-754 format.
getFloatbe :: Monad m => Get () m Float
getFloatbe = voidCastGet S.getFloat32be

-- | Read a 'Float' in little endian IEEE-754 format.
getFloatle :: Monad m => Get () m Float
getFloatle = voidCastGet S.getFloat32le

-- | Read a 'Float' in IEEE-754 format and host endian.
getFloathost :: Monad m => Get () m Float
getFloathost = wordToFloat <$> voidCastGet S.getWord32host

-- | Read a 'Double' in big endian IEEE-754 format.
getDoublebe :: Monad m => Get () m Double
getDoublebe = voidCastGet S.getFloat64be

-- | Read a 'Double' in little endian IEEE-754 format.
getDoublele :: Monad m => Get () m Double
getDoublele = voidCastGet S.getFloat64le

-- | Read a 'Double' in IEEE-754 format and host endian.
getDoublehost :: Monad m => Get () m Double
getDoublehost = wordToDouble <$> voidCastGet S.getWord64host
