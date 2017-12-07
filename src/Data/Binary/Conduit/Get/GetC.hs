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

-- | This module provides the 'GetC' monad transformer,
-- and all functions, which could not be defined using 'GetC' public interface only.

module Data.Binary.Conduit.Get.GetC
  ( Decoding
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
  , mapError
  ) where

#include <haskell>

import Data.Binary.Conduit.Base

-- | 'GetC' monad state.
data Decoding = Decoding
  { decodingBytesRead :: !Word64 -- ^ Get the total number of bytes read to this point.
  , tracking :: !(Maybe [S.ByteString])
  } deriving Show

dropBytes :: Word64 -> [S.ByteString] -> [S.ByteString]
dropBytes 0 !x = x
dropBytes _ [] = error "Data.Binary.Conduit.Get.dropBytes"
dropBytes !n !(h : t)
  | fromIntegral (SB.length h) <= n = dropBytes (n - fromIntegral (SB.length h)) t
  | otherwise = SB.take (SB.length h - fromIntegral n) h : t
{-# INLINE dropBytes #-}

-- | Construct 'GetC' initial state.
startDecoding :: Word64 -> Decoding
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
decodingUngot :: Word64 -> Decoding -> Decoding
decodingUngot !bytes_count !s = Decoding
  { decodingBytesRead = decodingBytesRead s - fromIntegral bytes_count
  , tracking = dropBytes bytes_count <$> tracking s
  }
{-# INLINE decodingUngot #-}

-- | Internal transformers for 'Get' with error type @e@, host monad @m@ and decoder result @a@.
newtype GetC e m a = C { runC :: ExceptT e (StateT Decoding m) a }

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
  {-# INLINE liftWith #-}
  restoreT = defaultRestoreT2 C
  {-# INLINE restoreT #-}

instance MonadBase b m => MonadBase b (GetC e m) where
  liftBase = liftBaseDefault
  {-# INLINE liftBase #-}

instance MonadBaseControl b m => MonadBaseControl b (GetC e m) where
  type StM (GetC e m) a = ComposeSt (GetC e) m a
  liftBaseWith = defaultLiftBaseWith
  {-# INLINE liftBaseWith #-}
  restoreM = defaultRestoreM
  {-# INLINE restoreM #-}

-- | A 'ConduitM' with internal transformers supposed to a binary deserialization.
type Get o e m = ConduitM ByteChunk o (GetC e m)

instance MonadBase b m => MonadBase b (Get o e m) where
  liftBase = liftBaseDefault
  {-# INLINE liftBase #-}

instance (Monoid e, Monad m) => Alternative (Get o e m) where
  empty = throwError mempty
  {-# INLINE empty #-}
  a <|> b = catchError (transaction a) $ \ !ea -> catchError (transaction b) $ \ !eb -> throwError (ea `mappend` eb)
  {-# INLINE (<|>) #-}

transaction :: Monad m => Get o e m a -> Get o e m a
transaction !g = getC $ \ !c -> do
  (!r, !f) <- runGetC (Decoding { decodingBytesRead = decodingBytesRead c, tracking = Just [] }) g
  let !tracking_f = fromMaybe (error "Data.Binary.Conduit.Get.GetC.transaction") $ tracking f
  if isRight r
    then  return (r, Decoding { decodingBytesRead = decodingBytesRead f, tracking = (tracking_f ++) <$> tracking c })
    else forM_ tracking_f leftover >> return (r, c)
{-# INLINE transaction #-}

-- | Run a 'Get' monad, unwrapping all internal transformers in a reversible way.
-- @'getC' . 'flip' runGetC = 'id'@
runGetC :: Monad m => Decoding -> Get o e m a -> ConduitM S.ByteString o m (Either e a, Decoding)
runGetC !decoding = runStateC decoding . runExceptC . transPipe runC . mapInput ByteChunk (Just . bs)
{-# INLINE runGetC #-}

-- | Custom 'Get'.
-- @getC . 'flip' 'runGetC' = 'id'@
-- Example:
-- > skipUntilZero :: Monad m => Get o e m ()
-- > skipUntilZero = getC $ flip runStateC $ do
-- >   untilM_ (return ()) $ do
-- >     !m_inp <- await
-- >     case m_inp of
-- >       Nothing -> return True
-- >       Just !inp -> do
-- >         lift $ modify' $ decodingGot inp
-- >         case SB.elemIndex 0 inp of
-- >           Nothing -> return ()
-- >           Just !i -> do
-- >             let (!h, !t) = SB.splitAt i inp
-- >             leftover t
-- >             lift $ modify' $ decodingUngot $ SB.length t
-- Please note, the above code is just a sample, and this particular function can be defined in easy way without getC.
getC :: Monad m => (Decoding -> ConduitM S.ByteString o m (Either e a, Decoding)) -> Get o e m a
getC = mapInput bs (Just . ByteChunk) . transPipe C . exceptC . stateC
{-# INLINE getC #-}

-- | Wait for a single input value from upstream. If no data is available, returns 'Nothing'.
-- Once await returns 'Nothing', subsequent calls will also return 'Nothing'.
-- getChunk is 'await' with injected inner 'decodingGot'.
getChunk :: Monad m => Get o e m (Maybe S.ByteString)
getChunk = do
  !mi <- await
  case mi of
    Nothing -> return Nothing
    Just (ByteChunk !i) -> do
      lift $ C $ lift $ modify' $ decodingGot i
      return $ Just i
{-# INLINE getChunk #-}

-- | Provide a single piece of leftover input to be consumed by the next component in the current monadic binding.
-- Note: it is highly encouraged to only return leftover values from input already consumed from upstream.
-- ungetChunk is 'leftover' with injected inner 'decodingUngot'.
ungetChunk :: Monad m => S.ByteString -> Get o e m ()
ungetChunk !i = do
  leftover $ ByteChunk i
  lift $ C $ lift $ modify' $ decodingUngot $ fromIntegral $ SB.length i
{-# INLINE ungetChunk #-}

-- | Convert decoder error. If the decoder fails, the given function will be applied
-- to the error message.
mapError :: Monad m => (e -> e') -> Get o e m a -> Get o e' m a
mapError f !g = transPipe C $ exceptC $ either (Left . f) Right <$> runExceptC (transPipe runC g)
{-# INLINE mapError #-}
