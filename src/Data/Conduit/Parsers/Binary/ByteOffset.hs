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

-- | At the first look, Data.Binary.Conduit.Get module is very similar with Data.Binary.Get.
-- The main differences between them are the following.
-- While the 'S.Get' from binary is a very custom monad,
-- the local 'Get' is 'ConduitM', which leads to easy integration in complicated format parsing.
-- The Data.Binary.Get module does not have a function to create custom 'S.Get' monad,
-- this module provides 'getC'.
-- Unlike 'isolate' from binary, local 'isolate' does not "cut" bytes counter.
-- While the binary's 'S.Get' is 'MonadFail', which leads to very ugly errors handling
-- in complicated cases, local 'Get' is 'MonadError'.

module Data.Conduit.Parsers.Binary.ByteOffset
  ( ByteOffset (..)
  ) where

import qualified Data.ByteString as S (ByteString)
import qualified Data.ByteString as SB hiding (ByteString, head, last, init, tail)
import Data.Word
import Data.Conduit.Parsers
import Data.Conduit.Parsers.Binary
import Data.Conduit.Parsers.GetC
import Data.Conduit.Parsers.PutS

newtype ByteOffset = ByteOffset Word64 deriving Show

instance DecodingState ByteOffset where
  type DecodingToken ByteOffset = S.ByteString
  decoded !i (ByteOffset !s) = ByteOffset (s + fromIntegral (SB.length i))
  {-# INLINE decoded #-}

instance DecodingElemsRead ByteOffset where
  decodingElemsRead (ByteOffset !s) = s
  {-# INLINE decodingElemsRead #-}

instance EncodingState ByteOffset where
  type EncodingToken ByteOffset = Word64
  encoded !w (ByteOffset !s) = ByteOffset (s + w)
  {-# INLINE encoded #-}

instance EncodingBytesWrote ByteOffset where
  encodingBytesWrote (ByteOffset !s) = s
  {-# INLINE encodingBytesWrote #-}
