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

-- | This module provides the 'PutS' monad,
-- and all functions, which could not be defined using 'PutS' public interface only.

module Data.Conduit.Parsers.PutS
  ( EncodingState (..)
  , VoidEncodingState (..)
  , Encoding
  , encodingWrote
  , runEncoding
  , startEncoding
  , PutS
  , runPutS
  , putS
  , PutM
  ) where

import Control.Monad.Fix
import Control.Monad.Trans.State.Strict
import Data.Conduit hiding (ConduitM)
import Data.String
import qualified Data.Text as S (Text)

-- | Collects encoding process feedback.
class EncodingState s where
  type EncodingToken s :: *
  encoded :: EncodingToken s -> s -> s

-- | Trivial encoding state.
data VoidEncodingState = VoidEncodingState

instance EncodingState VoidEncodingState where
  type EncodingToken VoidEncodingState = ()
  encoded () = id
  {-# INLINE encoded #-}

-- | 'PutS' monad state.
data Encoding s m = Encoding
  { encodingWrote :: !s -- ^ Get the feedback of the encoding process that is currently collected.
  , runEncoding :: !(m ()) -- ^ Get the 'ConduitT'.
  }

instance (EncodingState s, Monad m) => EncodingState (Encoding s m) where
  type EncodingToken (Encoding s m) = (m (), EncodingToken s)
  encoded (!producer, !bytes_count) !s = Encoding
    { encodingWrote = encoded bytes_count (encodingWrote s)
    , runEncoding = runEncoding s >> producer
    }
  {-# INLINE encoded #-}

-- | Construct 'PutS' initial state.
startEncoding :: Applicative m => s -> Encoding s m
startEncoding !bytes_wrote_before = Encoding
  { encodingWrote = bytes_wrote_before
  , runEncoding = pure ()
  }
{-# INLINE startEncoding #-}

-- | A serializer with conduit @m@ and result @a@ (usually @()@).
newtype PutS s m a = S { runS :: State (Encoding s m) a }

deriving instance Monad (PutS s m)
deriving instance Functor (PutS s m)
deriving instance MonadFix (PutS s m)
deriving instance Applicative (PutS s m)

instance Monad m => Semigroup (PutS s m ()) where
  a <> b = a >> b
  {-# INLINE (<>) #-}

-- | A wrapped 'ConduitT' supposed to a binary or text serialization.
type PutM s i o m a = PutS s (ConduitT i o m) a

instance (EncodingState s, EncodingToken s ~ (), Monad m) => IsString (PutM s i S.Text m ()) where
  fromString x = putS $ \ !t -> ((), encoded (yield (fromString x), ()) t)
  {-# INLINE fromString #-}

-- | Run a serializer represented by 'PutM' monad, unwrapping all wrappers in a reversible way.
--
-- @'putS' . runPutS = 'id'@
runPutS :: PutS s m a -> Encoding s m -> (a, Encoding s m)
runPutS = runState . runS
{-# INLINE runPutS #-}

-- | Custom 'PutM'.
--
-- @putS . 'runPutS' = 'id'@
putS :: (Encoding s m -> (a, Encoding s m)) -> PutS s m a
putS = S . state
{-# INLINE putS #-}
