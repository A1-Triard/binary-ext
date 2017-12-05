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

module Data.Binary.Get.Ext.Spec
  ( tests
  ) where

#define TESTS
#include <haskell>

import Data.Binary.Ext.Get

tests :: Test
tests = TestList
  [ TestCase getBytes1
  , TestCase getBytes2
  , TestCase testSkip
  , TestCase eofError
  , TestCase eofOrNotEof
  ]

testInput1 :: [S.ByteString]
testInput1 =
  [ "\x12\x13\x14"
  , "\x15\x18\xF3"
  , ""
  ]

testInput2 :: [S.ByteString]
testInput2 =
  [ "\x12\x13\x14"
  , "\x15\x18\xF3"
  , "\0"
  ]

testInput3 :: [S.ByteString]
testInput3 =
  [ "\x12\x13"
  , "\x15\x18\xF3"
  , "\0"
  ]

testInput4 :: [S.ByteString]
testInput4 =
  [ "AB"
  , "C"
  ]

ensureEof :: Monad m => e -> Get o e m ()
ensureEof e = do
  eof <- getNull
  if eof then return () else throwError e

get1 :: Monad m => Get Word16 Bool m ()
get1 = do
  yield =<< getWord16le `ifError` False
  yield =<< getWord16le `ifError` False
  yield =<< getWord16be `ifError` False
  ensureEof True

get2 :: Monad m => Get o () m ByteOffset
get2 = do
  skip 3
  bytesRead

getTailBytes :: Monad m => Get o () m S.ByteString
getTailBytes = do
  r <- getByteString 3
  ensureEof ()
  return r

getBytes1 :: Assertion
getBytes1 = do
  let ((!e, !c), !r) = runIdentity $ N.yieldMany testInput1 $$ (runGet get1 `fuseBoth` N.sinkList)
  assertEqual "" (Right ()) e
  assertEqual "" [0x13 `shiftL` 8 .|. 0x12, 0x15 `shiftL` 8 .|. 0x14, 0x18 `shiftL` 8 .|. 0xF3] r
  assertEqual "" 6 c

getBytes2 :: Assertion
getBytes2 = do
  let ((!e, !c), !r) = runIdentity $ N.yieldMany testInput2 $$ (runGet get1 `fuseBoth` N.sinkList)
  assertEqual "" (Left True) e
  assertEqual "" [0x13 `shiftL` 8 .|. 0x12, 0x15 `shiftL` 8 .|. 0x14, 0x18 `shiftL` 8 .|. 0xF3] r
  assertEqual "" 6 c

testSkip :: Assertion
testSkip = do
  let (!e, !c) = runIdentity $ N.yieldMany testInput3 $$ runGet get2
  assertEqual "" (Right 3) e
  assertEqual "" 3 c

eofError :: Assertion
eofError = do
  let (!e, !c) = runIdentity $ N.yieldMany testInput4 $$ runGet getInt64host
  assertEqual "" (Left ()) e
  assertEqual "" 0 c

eofOrNotEof :: Assertion
eofOrNotEof = do
  let (!e, !c) = runIdentity $ N.yieldMany testInput4 $$ runGet (Right <$> getInt64host <|> Left <$> getTailBytes)
  assertEqual "" (Right $ Left "ABC") e
  assertEqual "" 3 c
