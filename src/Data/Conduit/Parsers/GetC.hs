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

module Data.Conduit.Parsers.GetC
  ( DecodingState (..)
  , Decoding
  , startDecoding
  , continueDecoding
  , decodingRead
  , GetC
  , GetM
  , runGetC
  , getC
  , trackP
  , tryP
  , maybeG
  , runMaybeG
  , exceptG
  , runExceptG
  , catchExceptG
  , readerG
  , runReaderG
  , stateLG
  , runStateLG
  , evalStateLG
  , execStateLG
  , stateG
  , runStateG
  , evalStateG
  , execStateG
  , writerLG
  , runWriterLG
  , execWriterLG
  , writerG
  , runWriterG
  , execWriterG
  , rwsLG
  , runRWSLG
  , evalRWSLG
  , execRWSLG
  , rwsG
  , runRWSG
  , evalRWSG
  , execRWSG
  ) where

import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.Base
import Control.Monad.Error.Class
import Control.Monad.Error.Map
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.RWS.Strict
import qualified Control.Monad.Trans.RWS.Lazy as L
import Control.Monad.Trans.State.Strict
import qualified Control.Monad.Trans.State.Lazy as L
import Control.Monad.Trans.Writer.Strict
import qualified Control.Monad.Trans.Writer.Lazy as L
import Data.Conduit
import Data.Conduit.Lift
import Data.Maybe hiding (fromJust)

class DecodingState s where
  type DecodingToken s :: *
  decoded :: DecodingToken s -> s -> s

-- | 'GetC' monad state.
data Decoding s i = Decoding
  { decodingRead :: !s -- ^ Get the total number of bytes read to this point.
  , tracking :: !(Maybe [i])
  }

-- | Construct 'GetC' initial state.
startDecoding :: s -> Decoding s i
startDecoding !bytes_read_before = Decoding { decodingRead = bytes_read_before, tracking = Nothing }
{-# INLINE startDecoding #-}

continueDecoding :: s -> [i] -> Decoding s i -> Decoding s i
continueDecoding new delta old = Decoding { decodingRead = new, tracking = (delta ++) <$> tracking old }
{-# INLINE continueDecoding #-}

instance (DecodingState s, DecodingToken s ~ i) => DecodingState (Decoding s i) where
  type DecodingToken (Decoding s i) = DecodingToken s
  decoded !inp !s = Decoding
    { decodingRead = decoded inp (decodingRead s)
    , tracking = (inp :) <$> tracking s
    }
  {-# INLINE decoded #-}

-- | Internal transformers for 'Get' with error type @e@, host monad @m@ and decoder result @a@.
newtype GetC s i e m a = C { runC :: ExceptT e (StateT (Decoding s i) m) a }

instance MonadTrans (GetC s i e) where
  lift = C . lift . lift
  {-# INLINE lift #-}
deriving instance Monad m => Monad (GetC s i e m)
deriving instance Functor m => Functor (GetC s i e m)
deriving instance MonadFix m => MonadFix (GetC s i e m)
deriving instance MonadFail m => MonadFail (GetC s i e m)
deriving instance (Functor m, Monad m) => Applicative (GetC s i e m)
deriving instance MonadIO m => MonadIO (GetC s i e m)
deriving instance (Functor m, Monad m, Monoid e) => Alternative (GetC s i e m)
deriving instance (Monad m, Monoid e) => MonadPlus (GetC s i e m)
deriving instance Monad m => MonadError e (GetC s i e m)

instance MonadTransControl (GetC s i e) where
  type StT (GetC s i e) a = StT (StateT (Decoding s i)) (StT (ExceptT e) a)
  liftWith = defaultLiftWith2 C runC
  {-# INLINE liftWith #-}
  restoreT = defaultRestoreT2 C
  {-# INLINE restoreT #-}

instance MonadBase b m => MonadBase b (GetC s i e m) where
  liftBase = liftBaseDefault
  {-# INLINE liftBase #-}

instance MonadBaseControl b m => MonadBaseControl b (GetC s i e m) where
  type StM (GetC s i e m) a = ComposeSt (GetC s i e) m a
  liftBaseWith = defaultLiftBaseWith
  {-# INLINE liftBaseWith #-}
  restoreM = defaultRestoreM
  {-# INLINE restoreM #-}

instance Monad m => MonadMapError e (GetC s i e m) e' (GetC s i e' m) where
  mapError f = C . mapError f . runC

-- | A 'ConduitM' with internal transformers supposed to a binary deserialization.
type GetM s i o e m = ConduitM i o (GetC s i e m)

instance (Monoid e, Monad m) => Alternative (GetM s i o e m) where
  empty = throwError mempty
  {-# INLINE empty #-}
  a <|> b = catchError (tryP a) $ \ !ea -> catchError (tryP b) $ \ !eb -> throwError (ea `mappend` eb)
  {-# INLINE (<|>) #-}

instance (Monoid e, Monad m) => MonadPlus (GetM s i o e m) where
  mzero = empty
  {-# INLINE mzero #-}
  mplus a b = a <|> b
  {-# INLINE mplus #-}

-- | Leftover consumed input on error.
tryP :: Monad m => GetM s i o e m a -> GetM s i o e m a
tryP !g = getC $ \ !c -> do
  (!t, !d) <- runGetC (startDecoding $ decodingRead c) $ trackP g
  case t of
    Right (!f, !r) -> return (Right r, continueDecoding (decodingRead d) f c)
    Left (!f, !e) -> forM_ f leftover >> return (Left e, c)
{-# INLINE tryP #-}

-- | Run a decoder, storing input stream.
trackP :: Monad m => GetM s i o e m a -> GetM s i o ([i], e) m ([i], a)
trackP !g = getC $ \ !c -> do
  (!r, !f) <- runGetC (Decoding { decodingRead = decodingRead c, tracking = Just [] }) g
  let !tracking_f = fromMaybe (error "Data.Conduit.Parsers.GetC.track") $ tracking f
  return (either (Left . (tracking_f,)) (Right . (tracking_f,)) r, Decoding { decodingRead = decodingRead f, tracking = (tracking_f ++) <$> tracking c })
{-# INLINE trackP #-}

-- | Run a 'Get' monad, unwrapping all internal transformers in a reversible way.
-- @'getC' . 'flip' runGetC = 'id'@
runGetC :: Monad m => Decoding s i -> GetM s i o e m a -> ConduitM i o m (Either e a, Decoding s i)
runGetC !decoding = runStateC decoding . runExceptC . transPipe runC
{-# INLINE runGetC #-}

-- | Custom 'Get'.
-- @getC . 'flip' 'runGetC' = 'id'@
-- Example:
-- > skipUntilZero :: Get e Bool
-- > skipUntilZero = getC $ flip runStateC $ untilJust $ do
-- >   !m_inp <- await
-- >   case m_inp of
-- >     Nothing -> return $ Just $ Right False
-- >     Just !inp -> do
-- >       case SB.elemIndex 0 inp of
-- >         Nothing -> do
-- >           lift $ modify' $ decoded inp
-- >           return Nothing
-- >         Just !i -> do
-- >           let (!h, !t) = SB.splitAt i inp
-- >           leftover t
-- >           lift $ modify' $ decoded h
-- >           return $ Just $ Right True
getC :: Monad m => (Decoding s i -> ConduitM i o m (Either e a, Decoding s i)) -> GetM s i o e m a
getC = transPipe C . exceptC . stateC
{-# INLINE getC #-}

-- | Wrap the base monad in `ExceptT`, pushing `Either` to a monad transformers stack.
exceptG :: Monad m => GetM s i o e' m (Either e a) -> GetM s i o e' (ExceptT (e, Decoding s i) m) a
exceptG g =
  getC $ \ !x -> exceptC $ ee <$> runGetC x g
  where
  ee :: (Either e' (Either e a), Decoding s i) -> Either (e, Decoding s i) (Either e' a, Decoding s i)
  ee (Right (Right a), b) = Right (Right a, b)
  ee (Right (Left x), b) = Left (x, b)
  ee (Left x, b) = Right (Left x, b)
{-# INLINE exceptG #-}

-- | Run `ExceptT` in the base monad, pulling `Either` from a monad transformers stack.
runExceptG :: Monad m => GetM s i o e' (ExceptT (e, Decoding s i) m) a -> GetM s i o e' m (Either e a)
runExceptG g =
  getC $ \ !x -> (ee <$>) $ runExceptC $ runGetC x g
  where
  ee :: Either (e, Decoding s i) (Either e' a, Decoding s i) -> (Either e' (Either e a), Decoding s i)
  ee (Right (Right a, b)) = (Right (Right a), b)
  ee (Right (Left x, b)) = (Left x, b)
  ee (Left (x, b)) = (Right (Left x), b)
{-# INLINE runExceptG #-}

-- | Catch an error in the base monad.
catchExceptG :: Monad m => GetM s i o e' (ExceptT (e, Decoding s i) m) r -> (e -> GetM s i o e' (ExceptT (e, Decoding s i) m) r) -> GetM s i o e' (ExceptT (e, Decoding s i) m) r
catchExceptG g c =
  getC $ \ !x -> catchExceptC (runGetC x g) (\(e, b) -> runGetC b (c e))
{-# INLINE catchExceptG #-}

-- | Wrap the base monad in `ExceptT`, pushing `Maybe` to a monad transformers stack.
maybeG :: Monad m => GetM s i o e m (Maybe a) -> GetM s i o e (ExceptT (Decoding s i) m) a
maybeG g =
  getC $ \ !x -> exceptC $ em <$> runGetC x g
  where
  em :: (Either e (Maybe a), Decoding s i) -> Either (Decoding s i) (Either e a, Decoding s i)
  em (Right (Just a), b) = Right (Right a, b)
  em (Right Nothing, b) = Left b
  em (Left e, b) = Right (Left e, b)
{-# INLINE maybeG #-}

-- | Run `ExceptT` in the base monad, pulling `Maybe` from a monad transformers stack.
runMaybeG :: Monad m => GetM s i o e (ExceptT (Decoding s i) m) a -> GetM s i o e m (Maybe a)
runMaybeG g =
  getC $ \ !x -> (me <$>) $ runExceptC $ runGetC x g
  where
  me :: Either (Decoding s i) (Either e a, Decoding s i) -> (Either e (Maybe a), Decoding s i)
  me (Right (Right a, b)) = (Right (Just a), b)
  me (Right (Left e, b)) = (Left e, b)
  me (Left b) = (Right Nothing, b)
{-# INLINE runMaybeG #-}

-- | Wrap the base monad in `ReaderT`.
readerG :: Monad m => (r -> GetM s i o e m a) -> GetM s i o e (ReaderT r m) a
readerG g = getC $ \ !x -> readerC $ \r -> runGetC x (g r)
{-# INLINE readerG #-}

-- | Run `ReaderT` in the base monad.
runReaderG :: Monad m => r -> GetM s i o e (ReaderT r m) a -> GetM s i o e m a
runReaderG r g = getC $ \ !x -> runReaderC r $ runGetC x g
{-# INLINE runReaderG #-}

-- | Wrap the base monad in `L.StateT`.
stateLG :: Monad m => (t -> GetM s i o e m (a, t)) -> GetM s i o e (L.StateT t m) a
stateLG g =
  getC $ \ !x -> stateLC $ \t -> st <$> runGetC x (g t)
  where
  st :: (Either e (a, t), Decoding s i) -> ((Either e a, Decoding s i), t)
  st (Right (a, t), b) = ((Right a, b), t)
  st (Left e, b) = ((Left e, b), error "stateLG")
{-# INLINE stateLG #-}

-- | Run `L.StateT` in the base monad.
runStateLG :: Monad m => t -> GetM s i o e (L.StateT t m) a -> GetM s i o e m (a, t)
runStateLG t g =
  getC $ \ !x -> (ts <$>) $ runStateLC t $ runGetC x g
  where
  ts :: ((Either e a, Decoding s i), t) -> (Either e (a, t), Decoding s i)
  ts ((Right a, b), r) = (Right (a, r), b)
  ts ((Left e, b), _) = (Left e, b)
{-# INLINE runStateLG #-}

-- | Evaluate `L.StateT` in the base monad.
evalStateLG :: Monad m => t -> GetM s i o e (L.StateT t m) a -> GetM s i o e m a
evalStateLG t = (fst <$>) . runStateLG t
{-# INLINE evalStateLG #-}

-- | Execute `L.StateT` in the base monad.
execStateLG :: Monad m => t -> GetM s i o e (L.StateT t m) a -> GetM s i o e m t
execStateLG t = (snd <$>) . runStateLG t
{-# INLINE execStateLG #-}

-- | Wrap the base monad in `StateT`.
stateG :: Monad m => (t -> GetM s i o e m (a, t)) -> GetM s i o e (StateT t m) a
stateG g =
  getC $ \ !x -> stateC $ \t -> st <$> runGetC x (g t)
  where
  st :: (Either e (a, t), Decoding s i) -> ((Either e a, Decoding s i), t)
  st (Right (a, t), b) = ((Right a, b), t)
  st (Left e, b) = ((Left e, b), error "stateLG")
{-# INLINE stateG #-}

-- | Run `StateT` in the base monad.
runStateG :: Monad m => t -> GetM s i o e (StateT t m) a -> GetM s i o e m (a, t)
runStateG t g =
  getC $ \ !x -> (ts <$>) $ runStateC t $ runGetC x g
  where
  ts :: ((Either e a, Decoding s i), t) -> (Either e (a, t), Decoding s i)
  ts ((Right a, b), r) = (Right (a, r), b)
  ts ((Left e, b), _) = (Left e, b)
{-# INLINE runStateG #-}

-- | Evaluate `StateT` in the base monad.
evalStateG :: Monad m => t -> GetM s i o e (StateT t m) a -> GetM s i o e m a
evalStateG t = (fst <$>) . runStateG t
{-# INLINE evalStateG #-}

-- | Execute `StateT` in the base monad.
execStateG :: Monad m => t -> GetM s i o e (StateT t m) a -> GetM s i o e m t
execStateG t = (snd <$>) . runStateG t
{-# INLINE execStateG #-}

-- | Wrap the base monad in `L.WriterT`.
writerLG :: (Monad m, Monoid t) => GetM s i o e m (a, t) -> GetM s i o e (L.WriterT t m) a
writerLG g =
  getC $ \ !x -> writerLC $ st <$> runGetC x g
  where
  st :: (Either e (a, t), Decoding s i) -> ((Either e a, Decoding s i), t)
  st (Right (a, t), b) = ((Right a, b), t)
  st (Left e, b) = ((Left e, b), error "writerLG")
{-# INLINE writerLG #-}

-- | Run `L.WriterT` in the base monad.
runWriterLG :: (Monad m, Monoid t) => GetM s i o e (L.WriterT t m) a -> GetM s i o e m (a, t)
runWriterLG g =
  getC $ \ !x -> (ts <$>) $ runWriterLC $ runGetC x g
  where
  ts :: ((Either e a, Decoding s i), t) -> (Either e (a, t), Decoding s i)
  ts ((Right a, b), r) = (Right (a, r), b)
  ts ((Left e, b), _) = (Left e, b)
{-# INLINE runWriterLG #-}

-- | Execute `L.WriterT` in the base monad.
execWriterLG :: (Monad m, Monoid t) => GetM s i o e (L.WriterT t m) a -> GetM s i o e m t
execWriterLG = (snd <$>) . runWriterLG
{-# INLINE execWriterLG #-}

-- | Wrap the base monad in `WriterT`.
writerG :: (Monad m, Monoid t) => GetM s i o e m (a, t) -> GetM s i o e (WriterT t m) a
writerG g =
  getC $ \ !x -> writerC $ st <$> runGetC x g
  where
  st :: (Either e (a, t), Decoding s i) -> ((Either e a, Decoding s i), t)
  st (Right (a, t), b) = ((Right a, b), t)
  st (Left e, b) = ((Left e, b), error "writerG")
{-# INLINE writerG #-}

-- | Run `WriterT` in the base monad.
runWriterG :: (Monad m, Monoid t) => GetM s i o e (WriterT t m) a -> GetM s i o e m (a, t)
runWriterG g =
  getC $ \ !x -> (ts <$>) $ runWriterC $ runGetC x g
  where
  ts :: ((Either e a, Decoding s i), t) -> (Either e (a, t), Decoding s i)
  ts ((Right a, b), r) = (Right (a, r), b)
  ts ((Left e, b), _) = (Left e, b)
{-# INLINE runWriterG #-}

-- | Execute `WriterT` in the base monad.
execWriterG :: (Monad m, Monoid t) => GetM s i o e (WriterT t m) a -> GetM s i o e m t
execWriterG = (snd <$>) . runWriterG
{-# INLINE execWriterG #-}

-- | Wrap the base monad in `L.RWST`.
rwsLG :: (Monad m, Monoid w) => (r -> t -> GetM s i o e m (a, t, w)) -> GetM s i o e (L.RWST r w t m) a
rwsLG g =
  getC $ \ !x -> rwsLC $ \r t -> st <$> runGetC x (g r t)
  where
  st :: (Either e (a, t, w), Decoding s i) -> ((Either e a, Decoding s i), t, w)
  st (Right (a, t, w), b) = ((Right a, b), t, w)
  st (Left e, b) = ((Left e, b), error "rwsLG.s", error "rwsLG.w")
{-# INLINE rwsLG #-}

-- | Run `L.RWST` in the base monad.
runRWSLG :: (Monad m, Monoid w) => r -> t -> GetM s i o e (L.RWST r w t m) a -> GetM s i o e m (a, t, w)
runRWSLG r t g =
  getC $ \ !x -> (ts <$>) $ runRWSLC r t $ runGetC x g
  where
  ts :: ((Either e a, Decoding s i), t, w) -> (Either e (a, t, w), Decoding s i)
  ts ((Right a, b), x, w) = (Right (a, x, w), b)
  ts ((Left e, b), _, _) = (Left e, b)
{-# INLINE runRWSLG #-}

-- | Evaluate `L.RWST` in the base monad.
evalRWSLG :: (Monad m, Monoid w) => r -> t -> GetM s i o e (L.RWST r w t m) a -> GetM s i o e m (a, w)
evalRWSLG r t =
  (res <$>) . runRWSLG r t
  where
  res (a, _, b) = (a, b)
{-# INLINE evalRWSLG #-}

-- | Execute `L.RWST` in the base monad.
execRWSLG :: (Monad m, Monoid w) => r -> t -> GetM s i o e (L.RWST r w t m) a -> GetM s i o e m (t, w)
execRWSLG r t =
  (res <$>) . runRWSLG r t
  where
  res (_, a, b) = (a, b)
{-# INLINE execRWSLG #-}

-- | Wrap the base monad in `RWST`.
rwsG :: (Monad m, Monoid w) => (r -> t -> GetM s i o e m (a, t, w)) -> GetM s i o e (RWST r w t m) a
rwsG g =
  getC $ \ !x -> rwsC $ \r t -> st <$> runGetC x (g r t)
  where
  st :: (Either e (a, t, w), Decoding s i) -> ((Either e a, Decoding s i), t, w)
  st (Right (a, t, w), b) = ((Right a, b), t, w)
  st (Left e, b) = ((Left e, b), error "rwsG.s", error "rwsG.w")
{-# INLINE rwsG #-}

-- | Run `RWST` in the base monad.
runRWSG :: (Monad m, Monoid w) => r -> t -> GetM s i o e (RWST r w t m) a -> GetM s i o e m (a, t, w)
runRWSG r t g =
  getC $ \ !x -> (ts <$>) $ runRWSC r t $ runGetC x g
  where
  ts :: ((Either e a, Decoding s i), t, w) -> (Either e (a, t, w), Decoding s i)
  ts ((Right a, b), x, w) = (Right (a, x, w), b)
  ts ((Left e, b), _, _) = (Left e, b)
{-# INLINE runRWSG #-}

-- | Evaluate `RWST` in the base monad.
evalRWSG :: (Monad m, Monoid w) => r -> t -> GetM s i o e (RWST r w t m) a -> GetM s i o e m (a, w)
evalRWSG r t =
  (res <$>) . runRWSG r t
  where
  res (a, _, b) = (a, b)
{-# INLINE evalRWSG #-}

-- | Execute `RWST` in the base monad.
execRWSG :: (Monad m, Monoid w) => r -> t -> GetM s i o e (RWST r w t m) a -> GetM s i o e m (t, w)
execRWSG r t =
  (res <$>) . runRWSG r t
  where
  res (_, a, b) = (a, b)
{-# INLINE execRWSG #-}
