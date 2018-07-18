--
-- Copyright 2017, 2018 Warlock <internalmike@gmail.com>
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
-- | This module provides generic functions set useful for errortype-parameterized monads and monad transformers,
-- such as 'Data.Conduit.Parsers.Binary.Get.Get'.
-- Example:
--
-- > pSign :: Parser (Maybe Char) Bool
-- > pSign = do
-- >   c <- pChar ?>> return Nothing
-- >   case c of
-- >     '+' -> return False
-- >     '-' -> return True
-- >     x -> throwError (Just x)
-- >
-- > pSignedNumber :: Parser String Int
-- > pSignedNumber = do
-- >   is_negative <- fromMaybe False <$> option'' pSign
-- >   value <- foldl1 (\ !a !b -> a * 10 + b) <$> many1'' pDigit ?>> return "digit or sign expected"
-- >   return $ if is_negative then -value else value


module Control.Monad.Error.Map
  ( MonadMapError (..)
  , (?=>>)
  , (?>>)
  , option''
  , many''
  , many1''
  , manyTill''
  , sepBy''
  , sepBy1''
  , skipMany''
  , skipMany1''
  ) where

import Prelude hiding (head, tail, init, last, minimum)
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Trans.Except
import Data.Conduit
import Data.List.NonEmpty (NonEmpty (..))
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

instance MonadMapError e m_e e' m_e' => MonadMapError e (ConduitT i o m_e) e' (ConduitT i o m_e') where
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

-- | @option'' p@ tries to apply action @p@.
-- If @p@ fails, it returns @Nothing@.
-- @option'' p@ never fails.
--
-- @pPriority  = (digitToInt . fromMaybe 0) <$> option'' pDigit@
option'' ::
  ( MonadPlus m_Unit
  , MonadMapError e m_e () m_Unit
  , MonadMapError () m_Unit e' m_e'
  ) => m_e a -> m_e' (Maybe a)
option'' p = mapError (error "Control.Monad.Error.Map.option''") $ mapError (const ()) (Just <$> p) `mplus` return Nothing
{-# INLINE option'' #-}

-- | A version of 'liftM2' that is strict in the result of its first action.
liftM2' :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2' f a b = do
  !x <- a
  y <- b
  return (f x y)
{-# INLINE liftM2' #-}

-- | @many'' p@ applies the action @p@ zero or more times. Returns a list of the returned values of @p@.
-- @many'' p@ never fails.
--
-- @pWord  = many'' pLetter@
many'' ::
  ( MonadPlus m_Unit
  , MonadMapError e m_e () m_Unit
  , MonadMapError () m_Unit e' m_e'
  ) => m_e a -> m_e' [a]
many'' p =
  mapError (error "Control.Monad.Error.Map.many''") many_p
  where
    many_p = some_p `mplus` return []
    some_p = liftM2' (:) (mapError (const ()) p) many_p
{-# INLINE many'' #-}

-- | @many1'' p@ applies the action @p@ one or more times. Returns a non-empty list of the returned values of @p@.
--
-- @pWord  = many1'' pLetter@
many1'' ::
  ( MonadPlus m_Unit
  , MonadMapError e m_e () m_Unit
  , MonadMapError () m_Unit e m_e
  ) => m_e a -> m_e (NonEmpty a)
many1'' p = do
  !h <- p
  !t <- many'' p
  return $ h :| t
{-# INLINE many1'' #-}

-- | @manyTill'' p@ end applies action @p@ zero or more times until action end succeeds,
-- and returns the list of values returned by @p@. This can be used to scan comments:
--
-- @pSimpleComment = pStringIs "<!--" *> manyTill'' pChar (pStringIs "-->")@
-- (Note the overlapping parsers anyChar and string "-->". While this will work, it is not very efficient, as it will cause a lot of backtracking.)
manyTill'' ::
  ( MonadPlus m_Unit
  , MonadMapError e' m_e' () m_Unit
  , MonadMapError () m_Unit e m_e
  ) => m_e a -> m_e' b -> m_e [a]
manyTill'' p end =
  reverse <$> go []
  where
  go !r = do
    !n <- option'' end
    case n of
      Just _ -> return r
      Nothing -> do
        !c <- p
        go (c : r)
{-# INLINE manyTill'' #-}

-- | @sepBy'' p sep@ applies zero or more occurrences of @p@, separated by @sep@. Returns a list of the values returned by @p@.
-- @sepBy'' p@ never fails.
--
-- @commaSep p  = p `sepBy''` (pCharIs ',')@
sepBy'' ::
  ( MonadPlus m_Unit
  , MonadMapError e m_e () m_Unit
  , MonadMapError () m_Unit e'' m_e''
  , MonadMapError e' m_e' () m_Unit
  , MonadMapError () m_Unit () m_Unit
  ) => m_e a -> m_e' s -> m_e'' [a]
sepBy'' p sep = do
  !h <- option'' p
  case h of
    Nothing -> return []
    Just c -> do
      !t <- many'' (mapError (const ()) sep >> mapError (const ()) p)
      return $ c : t
{-# INLINE sepBy'' #-}

-- | @sepBy1'' p sep@ applies one or more occurrences of @p@, separated by @sep@.
-- Returns a non-empty list of the values returned by @p@.
--
-- @commaSep p  = p `sepBy1''` (pCharIs ',')@
sepBy1'' ::
  ( MonadPlus m_Unit
  , MonadMapError e m_e () m_Unit
  , MonadMapError () m_Unit e m_e
  , MonadMapError e' m_e' () m_Unit
  , MonadMapError () m_Unit () m_Unit
  ) => m_e a -> m_e' s -> m_e (NonEmpty a)
sepBy1'' p sep = do
  !h <- p
  !t <- many'' (mapError (const ()) sep >> mapError (const ()) p)
  return $ h :| t
{-# INLINE sepBy1'' #-}

-- | @skipMany'' p@ applies the action @p@ zero or more times, and drops result.
skipMany'' ::
  ( MonadPlus m_Unit
  , MonadMapError e m_e () m_Unit
  , MonadMapError () m_Unit e' m_e'
  ) => m_e a -> m_e' ()
skipMany'' p =
  go
  where
  go = do
    n <- option'' p
    case n of
      Nothing -> return ()
      Just _ -> go
{-# INLINE skipMany'' #-}

-- | @skipMany1'' p@ applies the action @p@ one or more times, and drops result.
skipMany1'' ::
  ( MonadPlus m_Unit
  , MonadMapError e m_e () m_Unit
  , MonadMapError () m_Unit e m_e
  ) => m_e a -> m_e ()
skipMany1'' p = p >> skipMany'' p
{-# INLINE skipMany1'' #-}
