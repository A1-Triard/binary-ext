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
  ( Encoding
  , encodingBytesWrote
  , runEncoding
  , startEncoding
  , encoded
  , PutS
  , runPutS
  , putS
  , PutM
  ) where

import Control.Monad.Fix
import Control.Monad.Trans.State.Strict
import qualified Data.ByteString as S (ByteString)
import Data.Conduit
import Data.Word

-- | 'PutS' functor state.
data Encoding m = Encoding
  { encodingBytesWrote :: !Word64 -- ^ Get the total number of bytes wrote to this point.
  , runEncoding :: !(m ()) -- ^ Get the 'Producer'.
  }

-- | Construct 'PutS' initial state.
startEncoding :: Applicative m => Word64 -> m () -> Encoding m
startEncoding !bytes_wrote_before !produced_before = Encoding
  { encodingBytesWrote = bytes_wrote_before
  , runEncoding = produced_before
  }
{-# INLINE startEncoding #-}

-- | Modify 'PutS' state: mark byte string @inp@ as wrote.
encoded :: Monad m => m () -> Word64 -> Encoding m -> Encoding m
encoded !producer !bytes_count !s = Encoding
  { encodingBytesWrote = encodingBytesWrote s + bytes_count
  , runEncoding = runEncoding s >> producer
  }
{-# INLINE encoded #-}

-- | Wrappers for 'Put' with host monad @m@ and monad result @a@ (usually @()@).
newtype PutS m a = S { runS :: State (Encoding m) a }

deriving instance Monad (PutS m)
deriving instance Functor (PutS m)
deriving instance MonadFix (PutS m)
deriving instance Applicative (PutS m)

-- | A 'ConduitM' with wrappers supposed to a binary serialization.
type PutM i m a = PutS (ConduitM i S.ByteString m) a

-- | Run a 'Put' monad, unwrapping all wrappers in a reversible way.
-- @'putS' . runPutS = 'id'@
runPutS :: PutS m a -> Encoding m -> (a, Encoding m)
runPutS = runState . runS
{-# INLINE runPutS #-}

-- | Custom 'Put'.
-- @putS . 'runPutS' = 'id'@
putS :: (Encoding m -> (a, Encoding m)) -> PutS m a
putS = S . state
{-# INLINE putS #-}
