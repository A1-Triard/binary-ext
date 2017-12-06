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

module Data.Binary.Ext.Get.GetC
  ( ByteOffset
  , Decoding
  , startDecoding
  , decodingBytesRead
  , decodingGot
  , decodingUngot
  , GetC
  , GetInp
  , Get
  , runGetC
  , getC
  , getInp
  , ungetInp
  , yieldInp
  , yieldInpOr
  , mapError
  ) where

#include <haskell>

-- | An offset, counted in bytes.
type ByteOffset = Word64

-- | 'GetC' monad state.
data Decoding = Decoding { decodingBytesRead :: ByteOffset, tracking :: Maybe [S.ByteString] } deriving Show

dropBytes :: ByteOffset -> [S.ByteString] -> [S.ByteString]
dropBytes 0 !x = x
dropBytes _ [] = error "Data.Binary.Ext.Get.dropBytes"
dropBytes !n !(h : t)
  | fromIntegral (SB.length h) <= n = dropBytes (n - fromIntegral (SB.length h)) t
  | otherwise = SB.take (SB.length h - fromIntegral n) h : t
{-# INLINE dropBytes #-}

-- | Construct 'GetC' initial state.
startDecoding :: ByteOffset -> Decoding
startDecoding !bytes_read_before = Decoding { decodingBytesRead = bytes_read_before, tracking = Nothing }
{-# INLINE startDecoding #-}

-- | Modify 'GetC' state: mark byte string @inp@ as read.
-- See 'getC' for usage example.
decodingGot :: S.ByteString -> Decoding -> Decoding
decodingGot !inp !s = Decoding
  { decodingBytesRead = decodingBytesRead s + fromIntegral (SB.length inp)
  , tracking = (inp :) <$> tracking s
  }
{-# INLINE decodingGot #-}

-- | Modify 'GetC' state: mark last read @bytes_count@ as unread.
-- See 'getC' for usage example.
decodingUngot :: ByteOffset -> Decoding -> Decoding
decodingUngot !bytes_count !s = Decoding
  { decodingBytesRead = decodingBytesRead s - fromIntegral bytes_count
  , tracking = dropBytes bytes_count <$> tracking s
  }
{-# INLINE decodingUngot #-}

-- | Internal transformers for 'Get'.
newtype GetC
  e -- ^ Error type.
  m -- ^ Host monad type.
  a -- ^ Decoder result type.
  = C { runC :: ExceptT e (StateT Decoding m) a }

instance MonadTrans (GetC e) where
  lift = C . lift . lift
  {-# INLINE lift #-}
deriving instance Monad m => Monad (GetC e m)
deriving instance Functor m => Functor (GetC e m)
deriving instance MonadFix m => MonadFix (GetC e m)
deriving instance MonadFail m => MonadFail (GetC e m)
deriving instance (Functor m, Monad m) => Applicative (GetC e m)
deriving instance MonadIO m => MonadIO (GetC e m)
deriving instance (Functor m, Monad m, Monoid e) => Alternative (GetC e m)
deriving instance (Monad m, Monoid e) => MonadPlus (GetC e m)
deriving instance Monad m => MonadError e (GetC e m)

instance MonadTransControl (GetC e) where
  type StT (GetC e) a = StT (StateT Decoding) (StT (ExceptT e) a)
  liftWith = defaultLiftWith2 C runC
  restoreT = defaultRestoreT2 C

instance MonadBase b m => MonadBase b (GetC e m) where
  liftBase = liftBaseDefault

instance MonadBaseControl b m => MonadBaseControl b (GetC e m) where
  type StM (GetC e m) a = ComposeSt (GetC e) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

-- | A wrapped 'S.ByteString'.
-- There is no direct conversion between 'S.ByteString' and 'GetInp'.
-- Use 'getInt' instead of 'await' and 'ungetInp' instead of 'leftover'
-- to get unwrapped 'S.ByteString' instead of 'GetInp'.
newtype GetInp = GetInp { bs :: S.ByteString }
type instance Element GetInp = Word8
deriving instance Show GetInp
deriving instance Semigroup GetInp
deriving instance Monoid GetInp
deriving instance MonoFunctor GetInp

-- | A 'ConduitM' with internal transformers supposed to a binary deserialization.
type Get o e m = ConduitM GetInp o (GetC e m)

instance MonadBase b m => MonadBase b (ConduitM GetInp o (GetC e m)) where
  liftBase = liftBaseDefault

instance (Monoid e, Monad m) => Alternative (Get o e m) where
  empty = throwError mempty
  {-# INLINE empty #-}
  a <|> b = catchError (transaction a) $ \ !ea -> catchError (transaction b) $ \ !eb -> throwError (ea `mappend` eb)
  {-# INLINE (<|>) #-}

transaction :: Monad m => Get o e m a -> Get o e m a
transaction !g = getC $ \ !c -> do
  (!r, !f) <- runGetC (Decoding { decodingBytesRead = decodingBytesRead c, tracking = Just [] }) g
  let !tracking_f = fromMaybe (error "Data.Binary.Ext.Get.track") $ tracking f
  if isRight r
    then  return (r, Decoding { decodingBytesRead = decodingBytesRead f, tracking = (tracking_f ++) <$> tracking c })
    else forM_ tracking_f leftover >> return (r, c)
{-# INLINE transaction #-}

-- | Run a 'Get' monad, unwrapping all internal transformers in a reversible way.
-- 'getC' . 'flip' runGetC = 'id'
runGetC :: Monad m => Decoding -> Get o e m a -> ConduitM S.ByteString o m (Either e a, Decoding)
runGetC !decoding = runStateC decoding . runExceptC . transPipe runC . mapInput GetInp (Just . bs)
{-# INLINE runGetC #-}

-- | Custom 'Get'.
-- getC . 'flip' 'runGetC' = 'id'
-- Example:
-- > customGet :: Get
-- > customGet = getC $ flip runStateC $ do
-- >   !i <- await
-- >   lift $ modify' $ decodingGot i
-- >   leftover i
-- >   decodingUngot
-- >
-- >
-- >
-- >
-- >
-- >
getC :: Monad m => (Decoding -> ConduitM S.ByteString o m (Either e a, Decoding)) -> Get o e m a
getC = mapInput bs (Just . GetInp) . transPipe C . exceptC . stateC
{-# INLINE getC #-}

-- | Wait for a single input value from upstream. If no data is available, returns 'Nothing'.
-- Once await returns 'Nothing', subsequent calls will also return 'Nothing'.
-- getInp is 'await' with injected inner 'decodingGot'.
getInp :: Monad m => Get o e m (Maybe S.ByteString)
getInp = do
  !mi <- await
  case mi of
    Nothing -> return Nothing
    Just (GetInp !i) -> do
      lift $ C $ lift $ modify' $ decodingGot i
      return $ Just i
{-# INLINE getInp #-}

-- | Provide a single piece of leftover input to be consumed by the next component in the current monadic binding.
-- Note: it is highly encouraged to only return leftover values from input already consumed from upstream.
-- ungetInp is 'leftover' with injected inner 'decodingUngot'.
ungetInp :: Monad m => S.ByteString -> Get o e m ()
ungetInp !i = do
  leftover $ GetInp i
  lift $ C $ lift $ modify' $ decodingUngot $ fromIntegral $ SB.length i
{-# INLINE ungetInp #-}

-- | Send a value downstream to the next component to consume. If the downstream component terminates,
-- this call will never return control. If you would like to register a cleanup function, please use 'yieldInpOr' instead.
-- yieldInp is 'yield' with injected conversion from 'GetInp' to 'S.ByteString'.
yieldInp :: Monad m => S.ByteString -> ConduitM i GetInp m ()
yieldInp = yield . GetInp
{-# INLINE yieldInp #-}

-- | Similar to 'yieldInp', but additionally takes a finalizer to be run if the downstream component terminates.
-- yieldInpOr is 'yieldOr' with injected conversion from 'GetInp' to 'S.ByteString'.
yieldInpOr :: Monad m
  => S.ByteString
  -> m () -- ^ Finalizer.
  -> ConduitM i GetInp m ()
yieldInpOr !o = yieldOr (GetInp o)
{-# INLINE yieldInpOr #-}

-- | Convert decoder error. If the decoder fails, the given function will be applied
-- to the error message.
mapError :: Monad m => (e -> e') -> Get o e m a -> Get o e' m a
mapError !f !g = transPipe C $ exceptC $ either (Left . f) Right <$> runExceptC (transPipe runC g)
{-# INLINE mapError #-}
