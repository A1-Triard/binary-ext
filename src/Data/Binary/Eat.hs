module Data.Binary.Eat
  ( Eat
  , runEat
  , bytesEaten
  , markAsEaten
  , eatGet
  , mapError
  , onError
  , withError
  , ifError
  , voidError
  , eatWord8
  , eatWord16be
  , eatWord32be
  , eatWord64be
  , eatWord16le
  , eatWord32le
  , eatWord64le
  , eatWordhost
  , eatWord16host
  , eatWord32host
  , eatWord64host
  , eatInthost
  , eatInt16host
  , eatInt32host
  , eatInt64host
  , eatFloatbe
  , eatFloatle
  , eatFloathost
  , eatDoublebe
  , eatDoublele
  , eatDoublehost
  , isolateM
  , eatIsolate
  ) where

#include <haskell>

type Eat e m = ConduitM S.ByteString Void (ExceptT e (StateT ByteOffset m))

runEat :: Monad m => ByteOffset -> Eat e m a -> ConduitM S.ByteString o m (Either e a, ByteOffset)
runEat bytes_read_before = mapOutput absurd . runStateC bytes_read_before . runExceptC

bytesEaten :: Monad m => Eat e m ByteOffset
bytesEaten = lift $ lift get

markAsEaten :: Monad m => ByteOffset -> Eat e m ()
markAsEaten n = lift $ lift $ modify' (+ n)

eatGet :: Monad m => Get a -> Eat String m a
eatGet g =
  go (runGetIncremental g)
  where
    go (Done t o r) = leftover t >> markAsEaten (fromIntegral o) >> return r
    go (Fail t o e) = leftover t >> markAsEaten (fromIntegral o) >> throwError e
    go (Partial c) = go =<< c <$> await

-- | Convert decoder error. If the decoder fails, the given function will be applied
-- to the error message.
mapError :: Monad m => (e -> e') -> Eat e m a -> Eat e' m a
mapError f g = exceptC $ either (Left . f) Right <$> runExceptC g

-- | 'onError' is 'mapError' with its arguments flipped.
onError :: Monad m => Eat e m a -> (e -> e') -> Eat e' m a
onError = flip mapError

-- | Set decoder error. If the decoder fails, the given error will be used
-- as the error message.
withError :: Monad m => e -> Eat () m a -> Eat e m a
withError e = mapError (const e)

-- | 'ifError' is 'withError' with its arguments flipped.
ifError :: Monad m => Eat () m a -> e -> Eat e m a
ifError = flip withError

voidError :: Monad m => Eat e m a -> Eat () m a
voidError = mapError (const ())

voidEatGet :: Monad m => Get a -> Eat () m a
voidEatGet = voidError . eatGet

eatWord8 :: Monad m => Eat () m Word8
eatWord8 = voidEatGet getWord8

eatWord16be :: Monad m => Eat () m Word16
eatWord16be = voidEatGet getWord16be

eatWord32be :: Monad m => Eat () m Word32
eatWord32be = voidEatGet getWord32be

eatWord64be :: Monad m => Eat () m Word64
eatWord64be = voidEatGet getWord64be

eatWord16le :: Monad m => Eat () m Word16
eatWord16le = voidEatGet getWord16le

eatWord32le :: Monad m => Eat () m Word32
eatWord32le = voidEatGet getWord32le

eatWord64le :: Monad m => Eat () m Word64
eatWord64le = voidEatGet getWord64le

eatWordhost :: Monad m => Eat () m Word
eatWordhost = voidEatGet getWordhost

eatWord16host :: Monad m => Eat () m Word16
eatWord16host = voidEatGet getWord16host

eatWord32host :: Monad m => Eat () m Word32
eatWord32host = voidEatGet getWord32host

eatWord64host :: Monad m => Eat () m Word64
eatWord64host = voidEatGet getWord64host

eatInthost :: Monad m => Eat () m Int
eatInthost = voidEatGet getInthost

eatInt16host :: Monad m => Eat () m Int16
eatInt16host = voidEatGet getInt16host

eatInt32host :: Monad m => Eat () m Int32
eatInt32host = voidEatGet getInt32host

eatInt64host :: Monad m => Eat () m Int64
eatInt64host = voidEatGet getInt64host

eatFloatbe :: Monad m => Eat () m Float
eatFloatbe = voidEatGet getFloat32be

eatFloatle :: Monad m => Eat () m Float
eatFloatle = voidEatGet getFloat32le

eatFloathost :: Monad m => Eat () m Float
eatFloathost = wordToFloat <$> voidEatGet getWord32host

eatDoublebe :: Monad m => Eat () m Double
eatDoublebe = voidEatGet getFloat64be

eatDoublele :: Monad m => Eat () m Double
eatDoublele = voidEatGet getFloat64le

eatDoublehost :: Monad m => Eat () m Double
eatDoublehost = wordToDouble <$> voidEatGet getWord64host

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

eatIsolate :: Monad m => ByteOffset -> Eat e m a -> (ByteOffset -> e) -> Eat e m a
eatIsolate n0 g f = do
  (!consumed, r) <- fuseBoth (isolateM n0) g
  if consumed < n0
    then throwError $ f consumed
    else return r
