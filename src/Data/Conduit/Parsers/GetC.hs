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
import Control.Monad.Trans.State.Strict
import Data.Conduit
import Data.Conduit.Lift
import Data.Maybe hiding (fromJust)

--import Control.Monad.IO.Class
--import Control.Monad.Trans.Class
--import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
--import Control.Monad.Trans.Reader
--import Control.Monad.Trans.State.Strict
--import Control.Monad.Trans.Writer

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

tryP :: Monad m => GetM s i o e m a -> GetM s i o e m a
tryP !g = getC $ \ !c -> do
  (!t, !d) <- runGetC (startDecoding $ decodingRead c) $ trackP g
  case t of
    Right (!f, !r) -> return (Right r, continueDecoding (decodingRead d) f c)
    Left (!f, !e) -> forM_ f leftover >> return (Left e, c)
{-# INLINE tryP #-}

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

maybeG :: Monad m => GetM s i o e m (Maybe a) -> GetM s i o e (MaybeT m) a
maybeG g = getC $ \ !x -> maybeC $ em <$> runGetC x g
  where
  em :: (Either e (Maybe a), b) -> Maybe (Either e a, b)
  em (Right (Just a), b) = Just (Right a, b)
  em (Right Nothing, _) = Nothing
  em (Left e, b) = Just (Left e, b)
{-# INLINE maybeG #-}

runMaybeG :: Monad m => GetM s i o e (MaybeT m) a -> GetM s i o e m (Maybe a)
runMaybeG g = getC $ \ !x -> (me x <$>) $ runMaybeC $ runGetC x g
  where
  me :: b -> Maybe (Either e a, b) -> (Either e (Maybe a), b)
  me _ (Just (Right a, b)) = (Right (Just a), b)
  me _ (Just (Left e, b)) = (Left e, b)
  me b Nothing = (Right Nothing, b)
{-# INLINE runMaybeG #-}
