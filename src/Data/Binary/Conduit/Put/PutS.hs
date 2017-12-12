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

-- | This module provides the 'PutS' functor,
-- and all functions, which could not be defined using 'PutS' public interface only.

module Data.Binary.Conduit.Put.PutS
  ( EncodingState (..)
  , EncodingBytesWrote (..)
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
import Data.Conduit
import Data.Word

class EncodingState s where
  type EncodingToken s :: *
  encoded :: EncodingToken s -> s -> s

class EncodingBytesWrote s where
  encodingBytesWrote :: s -> Word64

-- | 'PutS' functor state.
data Encoding s m = Encoding
  { encodingWrote :: !s -- ^ Get the total number of bytes wrote to this point.
  , runEncoding :: !(m ()) -- ^ Get the 'Producer'.
  }

instance (EncodingState s, Monad m) => EncodingState (Encoding s m) where
  type EncodingToken (Encoding s m) = (m (), EncodingToken s)
  encoded (!producer, !bytes_count) !s = Encoding
    { encodingWrote = encoded bytes_count (encodingWrote s)
    , runEncoding = runEncoding s >> producer
    }
  {-# INLINE encoded #-}

instance (EncodingBytesWrote s) => EncodingBytesWrote (Encoding s m) where
  encodingBytesWrote = encodingBytesWrote . encodingWrote
  {-# INLINE encodingBytesWrote #-}

-- | Construct 'PutS' initial state.
startEncoding :: Applicative m => s -> Encoding s m
startEncoding !bytes_wrote_before = Encoding
  { encodingWrote = bytes_wrote_before
  , runEncoding = pure ()
  }
{-# INLINE startEncoding #-}

-- | Wrappers for 'Put' with inner monad @m@ and result @a@ (usually @()@).
newtype PutS s m a = S { runS :: State (Encoding s m) a }

deriving instance Monad (PutS s m)
deriving instance Functor (PutS s m)
deriving instance MonadFix (PutS s m)
deriving instance Applicative (PutS s m)

-- | A 'ConduitM' with wrappers supposed to a binary serialization.
type PutM s i o m a = PutS s (ConduitM i o m) a

-- | Run a 'Put' monad, unwrapping all wrappers in a reversible way.
-- @'putS' . runPutS = 'id'@
runPutS :: PutS s m a -> Encoding s m -> (a, Encoding s m)
runPutS = runState . runS
{-# INLINE runPutS #-}

-- | Custom 'Put'.
-- @putS . 'runPutS' = 'id'@
putS :: (Encoding s m -> (a, Encoding s m)) -> PutS s m a
putS = S . state
{-# INLINE putS #-}
