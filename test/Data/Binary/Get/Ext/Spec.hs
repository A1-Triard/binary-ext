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
  [ TestCase getBytes
  ]

testInput1 :: [String]
testInput1 =
  [ "\x12\x13\x14"
  , "\x15\x18\xF3"
  ]

get1 :: Monad m => Get Word16 () m ()
get1 = do
  yield =<< getWord16le
  yield =<< getWord16le
  yield =<< getWord16be

getBytes :: Assertion
getBytes = do
  let ((!e, !c), !r) = runIdentity $ N.yieldMany (SC.pack <$> testInput1) $$ (runGet 0 get1 `fuseBoth` N.sinkList)
  assertEqual "" (Right ()) e
  assertEqual "" [0x13 `shiftL` 8 .|. 0x12, 0x15 `shiftL` 8 .|. 0x14, 0x18 `shiftL` 8 .|. 0xF3] r
  assertEqual "" 6 c
