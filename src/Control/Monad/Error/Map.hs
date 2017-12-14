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

module Control.Monad.Error.Map
  ( MonadMapError (..)
  , (?=>>)
  , (?>>)
  ) where

import Control.Monad.Error.Class
import Control.Monad.Trans.Except
import Data.Conduit
import Data.Maybe
import Data.Void

class (MonadError e m_e, MonadError e' m_e') => MonadMapError e m_e e' m_e' | m_e -> e, m_e' -> e', m_e e' -> m_e', m_e' e -> m_e where
  mapError :: (e -> e') -> m_e a -> m_e' a

instance MonadMapError e (Either e) e' (Either e') where
  mapError f = either (Left . f) Right
  {-# INLINE mapError #-}

instance Monad m => MonadMapError e (ExceptT e m) e' (ExceptT e' m) where
  mapError f = ExceptT . (mapError f <$>) . runExceptT
  {-# INLINE mapError #-}

instance MonadMapError e m_e e' m_e' => MonadMapError e (ConduitM i o m_e) e' (ConduitM i o m_e') where
  mapError f = transPipe (mapError f)
  {-# INLINE mapError #-}

infixl 1 ?=>>
(?=>>) ::
  ( MonadMapError e m_e (Either e e') m_Either_e_e'
  , MonadMapError Void m_Void (Either e e') m_Either_e_e'
  , MonadMapError (Either e e') m_Either_e_e' e' m_e'
  ) => m_e a -> (e -> m_Void e') -> m_e' a
(?=>>) action mapper =
  mapError (either (error "?=>>") id)
  $ catchError (mapError Left action)
  $ ((throwError . Right) =<<) . mapError absurd . mapper . either id (error "?=>>")
{-# INLINE (?=>>) #-}

infixl 1 ?>>
(?>>) ::
  ( MonadMapError () m_Unit (Maybe e) m_Maybe_e
  , MonadMapError Void m_Void (Maybe e) m_Maybe_e
  , MonadMapError (Maybe e) m_Maybe_e e m_e
  ) => m_Unit a -> m_Void e -> m_e a
(?>>) action mapper =
  mapError (fromMaybe (error "?>>"))
  $ catchError (mapError (const Nothing) action)
  $ const $ (throwError . Just) =<< mapError absurd mapper
{-# INLINE (?>>) #-}
