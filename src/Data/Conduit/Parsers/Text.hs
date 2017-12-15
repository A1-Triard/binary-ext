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

module Data.Conduit.Parsers.Text
  ( DecodingLinesRead (..)
  , DecodingColumnsRead (..)
  , DecodingTextRead
  ) where

import Data.Word
import Data.Conduit.Parsers
import Data.Conduit.Parsers.GetC

class DecodingLinesRead s where
  decodingLinesRead :: s -> Word64

class DecodingColumnsRead s where
  decodingColumnsRead :: s -> Word64

class (DecodingElemsRead s, DecodingLinesRead s, DecodingColumnsRead s) => DecodingTextRead s where

instance (DecodingElemsRead s, DecodingLinesRead s, DecodingColumnsRead s) => DecodingTextRead s where

instance (DecodingState s, DecodingLinesRead s) => DecodingLinesRead (Decoding s i) where
  decodingLinesRead = decodingLinesRead . decodingRead
  {-# INLINE decodingLinesRead #-}

instance (DecodingState s, DecodingColumnsRead s) => DecodingColumnsRead (Decoding s i) where
  decodingColumnsRead = decodingColumnsRead . decodingRead
  {-# INLINE decodingColumnsRead #-}
