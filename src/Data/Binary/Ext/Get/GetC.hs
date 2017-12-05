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
  ) where

#include <haskell>

-- | An offset, counted in bytes.
type ByteOffset = Word64

-- | 'GetC' monad state.
data TrackGet = TrackGet { trackBytesRead :: ByteOffset, tracking :: Maybe ByteString } deriving Show

-- | Construct 'GetC' initial state.
trackGetBefore :: ByteOffset -> TrackGet
trackGetBefore bytes_read_before = TrackGet { trackBytesRead = bytes_read_before, tracking = Nothing }
{-# INLINE trackGetBefore #-}

-- | Modify 'GetC' state: mark byte string @inp@ as read.
-- See 'getC' for usage example.
trackGetAwait :: S.ByteString -> TrackGet -> TrackGet
trackGetAwait inp s = TrackGet
  { trackBytesRead = trackBytesRead s + fromIntegral (SB.length inp)
  , tracking = B.append (B.fromStrict inp) <$> tracking s
  }
{-# INLINE trackGetAwait #-}

-- | Modify 'GetC' state: mark last read @bytes_count@ as unread.
-- See 'getC' for usage example.
trackGetLeftover :: Int64 -> TrackGet -> TrackGet
trackGetLeftover bytes_count s = TrackGet
  { trackBytesRead = trackBytesRead s - fromIntegral bytes_count
  , tracking = B.drop bytes_count <$> tracking s
  }
{-# INLINE trackGetLeftover #-}

-- | Internal transformers for 'Get'.
newtype GetC
  e -- ^ Error type.
  m -- ^ Host monad type.
  a -- ^ Decoder result type.
  = C { runC :: ExceptT e (StateT TrackGet m) a }

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
  type StT (GetC e) a = StT (StateT TrackGet) (StT (ExceptT e) a)
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
  a <|> b = catchError (track a) $ \ !ea -> catchError (track b) $ \ !eb -> throwError (ea `mappend` eb)
  {-# INLINE (<|>) #-}

track :: Monad m => Get o e m a -> Get o e m a
track g = getC $ \ !c -> do
  (!r, !f) <- runGetC g $ TrackGet
    { trackBytesRead = trackBytesRead c
    , tracking = Just B.empty
    }
  let !tracking_f = fromMaybe (error "Data.Binary.Ext.Get.track") $ tracking f
  if isRight r
    then  return (r, TrackGet { trackBytesRead = trackBytesRead f, tracking = B.append tracking_f <$> tracking c })
    else forM_ (B.toChunks tracking_f) leftover >> return (r, c)
{-# INLINE track #-}

-- | Run a 'Get' monad, unwrapping all internal transformers in a reversible way.
-- 'getC' . runGetC = 'id'
runGetC :: Monad m => Get o e m a -> TrackGet -> ConduitM S.ByteString o m (Either e a, TrackGet)
runGetC g state_before = runStateC state_before $ runExceptC $ transPipe runC $ mapInput GetInp (Just . bs) g
{-# INLINE runGetC #-}

-- | Custom 'Get'.
-- getC . 'runGetC' = 'id'
getC :: Monad m => (TrackGet -> ConduitM S.ByteString o m (Either e a, TrackGet)) -> Get o e m a
getC = mapInput bs (Just . GetInp) . transPipe C . exceptC . stateC
{-# INLINE getC #-}

-- | Wait for a single input value from upstream. If no data is available, returns 'Nothing'.
-- Once await returns 'Nothing', subsequent calls will also return 'Nothing'.
-- getInp is 'await' with injected inner 'trackGetAwait'.
getInp :: Monad m => Get o e m (Maybe S.ByteString)
getInp = do
  !mi <- await
  case mi of
    Nothing -> return Nothing
    Just (GetInp !i) -> do
      lift $ C $ lift $ modify' $ trackGetAwait i
      return $ Just i
{-# INLINE getInp #-}

-- | Provide a single piece of leftover input to be consumed by the next component in the current monadic binding.
-- Note: it is highly encouraged to only return leftover values from input already consumed from upstream.
-- ungetInp is 'leftover' with injected inner 'trackGetLeftover'.
ungetInp :: Monad m => S.ByteString -> Get o e m ()
ungetInp i = do
  lift $ C $ lift $ modify' $ trackGetLeftover $ fromIntegral $ SB.length i
  leftover $ GetInp i
{-# INLINE ungetInp #-}

-- | Convert decoder error. If the decoder fails, the given function will be applied
-- to the error message.
mapError :: Monad m => (e -> e') -> Get o e m a -> Get o e' m a
mapError f g = transPipe C $ exceptC $ either (Left . f) Right <$> runExceptC (transPipe runC g)
{-# INLINE mapError #-}
