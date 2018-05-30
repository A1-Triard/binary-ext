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

module Data.Conduit.Parsers.Text.Parser.Spec
  ( tests
  ) where

import Data.Conduit
import qualified Data.Conduit.Combinators as N
import Data.Functor.Identity
import qualified Data.Text as S (Text)
import Test.HUnit.Base hiding (Label)
import Data.Conduit.Parsers.Binary.Get hiding (runGet)
import Data.Conduit.Parsers.Text.Parser

tests :: Test
tests = TestList
  [ TestCase testLinesRead
  ]

testLinesRead :: Assertion
testLinesRead = do
  let !r = runIdentity $ N.yieldMany testInput1 `connect` runParser parser1
  assertEqual "" (Right ('t', 'x')) r

parser1 :: Parser () (Char, Char)
parser1 = do
  0 <- linesRead
  0 <- columnsRead
  c1 <- pChar
  0 <- linesRead
  1 <- columnsRead
  skipEndOfLine
  1 <- linesRead
  0 <- columnsRead
  skipCharIs 'a'
  1 <- linesRead
  1 <- columnsRead
  skipCharIs 'u'
  1 <- linesRead
  2 <- columnsRead
  skipEndOfLine
  2 <- linesRead
  0 <- columnsRead
  c2 <- pCharIsNot 'b'
  2 <- linesRead
  1 <- columnsRead
  endOfInput
  return (c1, c2)

testInput1 :: [S.Text]
testInput1 =
  [ "t\n"
  , "au\nx"
  , ""
  ]
