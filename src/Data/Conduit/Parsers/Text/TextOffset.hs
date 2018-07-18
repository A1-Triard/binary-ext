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
-- | Default text parser state.

module Data.Conduit.Parsers.Text.TextOffset
  ( TextOffset (..)
  ) where

import qualified Data.Text as S (Text)
import qualified Data.Text as ST hiding (Text, head, last, tail, init)
import Data.Word
import Data.Conduit.Parsers
import Data.Conduit.Parsers.GetC
import Data.Conduit.Parsers.Text

data TextOffset = TextOffset Word64 Word64 Word64 deriving Show

instance DecodingState TextOffset where
  type DecodingToken TextOffset = S.Text
  decoded !i (TextOffset !o !l !c) =
    let newlines = reverse $ drop 1 $ ST.split (== '\n') i in
    TextOffset (o + fromIntegral (ST.length i)) (l + fromIntegral (length newlines)) $ case newlines of
      [] -> c + fromIntegral (ST.length i)
      (x : _) -> fromIntegral (ST.length x)
  {-# INLINE decoded #-}

instance DecodingElemsRead TextOffset where
  decodingElemsRead (TextOffset !o _ _) = o
  {-# INLINE decodingElemsRead #-}

instance DecodingLinesRead TextOffset where
  decodingLinesRead (TextOffset _ !l _) = l
  {-# INLINE decodingLinesRead #-}

instance DecodingColumnsRead TextOffset where
  decodingColumnsRead (TextOffset _ _ !c) = c
  {-# INLINE decodingColumnsRead #-}
