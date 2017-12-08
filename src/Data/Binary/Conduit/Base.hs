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

-- | This module is a common part of @Data.Binary.Conduit.Get.GetC@ and @Data.Binary.Conduit.Put.PutC@,

module Data.Binary.Conduit.Base
  ( ByteChunk (..)
  ) where

import Control.Applicative
import Control.Error.Util
import Control.Monad hiding (fail)
import Control.Monad.Base
import Control.Monad.Error.Class
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import qualified Data.ByteString as S (ByteString)
import qualified Data.ByteString as SB hiding (ByteString, head, last, init, tail)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B hiding (ByteString, head, last, init, tail)
import Data.Conduit
import Data.Conduit.Lift
import Data.Maybe hiding (fromJust)
import Data.MonoTraversable
import Data.Semigroup hiding (Option)
import Data.Word

-- | A wrapped 'S.ByteString'.
-- There is no direct conversion between 'S.ByteString' and 'ByteChunk'.
-- Use 'getInt' instead of 'await' and 'ungetChunk' instead of 'leftover'
-- to get unwrapped 'S.ByteString' instead of 'ByteChunk'.
newtype ByteChunk = ByteChunk { bs :: S.ByteString }
type instance Element ByteChunk = Word8
deriving instance Show ByteChunk
deriving instance Semigroup ByteChunk
deriving instance Monoid ByteChunk
deriving instance MonoFunctor ByteChunk
