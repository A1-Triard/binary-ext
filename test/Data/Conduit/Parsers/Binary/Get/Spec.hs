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

module Data.Conduit.Parsers.Binary.Get.Spec
  ( tests
  ) where

import Control.Applicative
import Control.Monad.Error.Class
import Control.Monad.Loops
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Bits
import qualified Data.ByteString as S (ByteString)
import qualified Data.ByteString as SB hiding (ByteString, head, last, init, tail)
import Data.ByteString.Lazy (ByteString)
import Data.Char
import Data.Conduit
import qualified Data.Conduit.Combinators as N
import Data.Conduit.Lift
import Data.Functor.Identity
import Data.Semigroup hiding (Option)
import Data.Void
import Data.Word
import Test.HUnit.Base hiding (Label)
import qualified Data.Conduit.Parsers.Binary.ByteOffset as G
import Data.Conduit.Parsers.Binary.Get hiding (runGet)
import qualified Data.Conduit.Parsers.Binary.Get as G (runGet)
import qualified Data.Conduit.Parsers.GetC as G

tests :: Test
tests = TestList
  [ TestCase getBytes1
  , TestCase getBytes2
  , TestCase testSkip
  , TestCase eofError
  , TestCase eofOrNotEof
  , TestCase testIsolateOverAlternativeIsolateNotEnough
  , TestCase testIsolateOverAlternativeIsolateExactly
  , TestCase testIsolateOverAlternativeIsolateEnough
  , TestCase testIsolateOverAlternativeIsolateEnoughButEof
  , TestCase testIsolateIsolateEnoughButEof
  , TestCase testIsolateIsolateEnoughButEofEarly
  , TestCase testAlternativeRollback
  , TestCase testRecords
  , TestCase testLeftoversInIsolate
  , TestCase testSkipUntilZero
  , TestCase testErrorMap
  ]

runGet :: Monad m => GetM G.ByteOffset i o e m a -> ConduitM i o m (Either e a, Word64)
runGet !g = (\(!r, !s) -> (r, G.decodingBytesRead s)) <$> G.runGetC (G.startDecoding $ G.ByteOffset 0) g

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

testInput5 :: [S.ByteString]
testInput5 =
  [ "AB"
  , "CDE"
  ]

testInput6 :: [S.ByteString]
testInput6 =
  [ "\x01\x02\x03\x04\x05\x06"
  , "\x07\x08\x09\x0A\x0B\x0C"
  ]

testInput7 :: [S.ByteString]
testInput7 =
  [ "A"
  , "B"
  , "C"
  ]

ensureEof :: e -> Get e ()
ensureEof e = do
  eof <- N.nullE
  if eof then return () else throwError e

get1 :: (DefaultDecodingState s, Monad m) => GetM s S.ByteString Word16 Bool m ()
get1 = do
  yield =<< mapError (const False) getWord16le
  yield =<< mapError (const False) getWord16le
  yield =<< mapError (const False) getWord16be
  ensureEof True

get2 :: Get () Word64
get2 = do
  skip 3
  bytesRead

getTailBytes :: Get () S.ByteString
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
  assertEqual "" 3 c

eofOrNotEof :: Assertion
eofOrNotEof = do
  let (!e, !c) = runIdentity $ N.yieldMany testInput4 $$ runGet (Right <$> getInt64host <|> Left <$> getTailBytes)
  assertEqual "" (Right $ Left "ABC") e
  assertEqual "" 3 c

testIsolateOverAlternativeIsolateNotEnough :: Assertion
testIsolateOverAlternativeIsolateNotEnough = do
  let
    (!e, !c) = runIdentity $ N.yieldMany testInput5
      $$ runGet (isolate 2 $ Right <$> getInt32le <|> Left <$> getWord8)
  assertEqual "" (Left $ Left $ Just 1) e
  assertEqual "" 1 c

testIsolateOverAlternativeIsolateExactly :: Assertion
testIsolateOverAlternativeIsolateExactly = do
  let
    (!e, !c) = runIdentity $ N.yieldMany testInput5
      $$ runGet (isolate 1 $ Right <$> getInt32le <|> Left <$> getWord8)
  assertEqual "" (Right $ Left $ fromIntegral $ ord 'A') e
  assertEqual "" 1 c

testIsolateOverAlternativeIsolateEnough :: Assertion
testIsolateOverAlternativeIsolateEnough = do
  let
    (!e, !c) = runIdentity $ N.yieldMany testInput5
      $$ runGet (isolate 4 $ Right <$> getInt32le <|> Left <$> getWord8)
  assertEqual "" (Right $ Right 1145258561) e
  assertEqual "" 4 c

testIsolateOverAlternativeIsolateEnoughButEof :: Assertion
testIsolateOverAlternativeIsolateEnoughButEof = do
  let
    (!e, !c) = runIdentity $ N.yieldMany testInput4
      $$ runGet (isolate 4 $ Right <$> getInt32le <|> Left <$> getWord8)
  assertEqual "" (Left $ Left Nothing) e
  assertEqual "" 0 c

testIsolateIsolateEnoughButEof :: Assertion
testIsolateIsolateEnoughButEof = do
  let
    (!e, !c) = runIdentity $ N.yieldMany testInput4
      $$ runGet (isolate 4 getWord8)
  assertEqual "" (Left $ Left $ Just 1) e
  assertEqual "" 1 c

testIsolateIsolateEnoughButEofEarly :: Assertion
testIsolateIsolateEnoughButEofEarly = do
  let
    (!e, !c) = runIdentity $ N.yieldMany testInput7
      $$ runGet (isolate 4 $ getWord8 >> getWord8 >> getWord8 >> getWord8)
  assertEqual "" (Left $ Left Nothing) e
  assertEqual "" 0 c

testAlternativeRollback :: Assertion
testAlternativeRollback = do
  let (!e, !c) = runIdentity $ N.yieldMany testInput6 $$ runGet ((skip 9 >> throwError ()) <|> getWord64le)
  assertEqual "" (Right $ 0x01 .|. 0x02 `shiftL` 8 .|. 0x03 `shiftL` 16 .|. 0x04 `shiftL` 24 .|. 0x05 `shiftL` 32 .|. 0x06 `shiftL` 40 .|. 0x07 `shiftL` 48 .|. 0x08 `shiftL` 56) e
  assertEqual "" 8 c

recordBody :: Get () [Word64]
recordBody = whileM (not <$> N.nullE) $ mapError (const ()) $ isolate 8 getWord64le

record :: Word64 -> Get (Either (Maybe Word64) ()) [Word64]
record z = isolate z recordBody

records :: (DefaultDecodingState s, Monad m) => GetM s S.ByteString [Word64] (Either (Maybe Word64) ()) m ()
records = do
  yield =<< record 24
  yield =<< record 16
  yield =<< record 8

recordsInput :: [S.ByteString]
recordsInput =
  [ "0123456701234567"
  , "01234567012345670123456701234567"
  ]

testRecords :: Assertion
testRecords = do
  let ((!e, !c), !r) = runIdentity $ N.yieldMany recordsInput $$ (runGet records `fuseBoth` N.sinkList)
  assertEqual (show c) (Right ()) e
  assertEqual "" [[3978425819141910832, 3978425819141910832, 3978425819141910832], [3978425819141910832, 3978425819141910832], [3978425819141910832]] r
  assertEqual "" 48 c

takeE :: Monad m => Int -> ConduitM S.ByteString o m S.ByteString
takeE !n =
  go SB.empty 0
  where
  go consumed !consumed_length
    | consumed_length >= n = return consumed
    | otherwise = do
      !mi <- await
      case mi of
        Nothing -> error "takeE"
        Just !i -> do
          let !gap = n - consumed_length
          if gap >= SB.length i
            then do
              go (consumed <> i) (consumed_length + fromIntegral (SB.length i))
            else do
              let (!got, !rest) = SB.splitAt gap i
              leftover rest
              return (consumed <> got)

testLeftoversInIsolate :: Assertion
testLeftoversInIsolate = do
  let !i = isolate 4 $ (leftover =<< takeE 4) >> skip 2
  let
    !g = do
      catchError i $ const $ return ()
      !r <- mapError Right $ getByteString 2
      ensureEof $ Right ()
      return r
  let (!e, !c) = runIdentity $ yield "ABCD" $$ runGet g
  assertEqual "" (Right "CD") e
  assertEqual "" 4 c

skipUntilZero :: Get e Bool
skipUntilZero = G.getC $ flip runStateC $ untilJust $ do
  !m_inp <- await
  case m_inp of
    Nothing -> return $ Just $ Right False
    Just !inp -> do
      case SB.elemIndex 0 inp of
        Nothing -> do
          lift $ modify' $ G.decoded inp
          return Nothing
        Just !i -> do
          let (!h, !t) = SB.splitAt i inp
          leftover t
          lift $ modify' $ G.decoded h
          return $ Just $ Right True

testZeroInput1 :: [S.ByteString]
testZeroInput1 =
  [ "0123"
  , "45\0zx"
  , "8"
  ]

testSkipUntilZero :: Assertion
testSkipUntilZero = do
  let (!r, !c) = runIdentity $ N.yieldMany testZeroInput1 $$ runGet (skipUntilZero >> getRemainingLazyByteString)
  assertEqual "" ((Right "\0zx8") :: Either Void ByteString) r
  assertEqual "" 10 c

w32 :: Get String Word32
w32 = getWord32le ?>> ("Unexpected EOF at " ++) <$> show <$> bytesRead

testErrorMap :: Assertion
testErrorMap = do
  let !r = runIdentity $ N.yieldMany [] $$ G.runGet w32
  assertEqual "" (Left "Unexpected EOF at 0") r
