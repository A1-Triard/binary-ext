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

-- | Despite binary's 'S.Put' is fully-functional construction (unlike 'S.Get'),
-- we decided to provide this module for symmetry with 'Data.Binary.Conduit.Get'.

module Data.Conduit.Parsers.Text.Gen
  ( PutM
  , TextGen
  , runTextGen
  , genString
  , genLazyString
  , genShow
  , genDigit
  , genHexDigit
  , genHexByte
  , genEnum
  ) where

import Data.Bits
import Data.Char
import Data.Conduit
import qualified Data.Text as S (Text)
import qualified Data.Text as ST hiding (Text, head, last, tail, init)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T hiding (Text, head, last, tail, init)
import Data.Word
import Data.Conduit.Parsers.PutS
import Debug.Trace

class (EncodingState s, EncodingToken s ~ ()) => DefaultTextGenState s where

instance (EncodingState s, EncodingToken s ~ ()) => DefaultTextGenState s where

-- | The shortening of 'PutM' for the most common use case.
type TextGen = forall s i m. (DefaultTextGenState s, Monad m) => PutM s i S.Text m ()

-- | Run an encoder presented as a 'Put' monad.
-- Returns 'Producer'.
runTextGen :: PutM VoidEncodingState i o m () -> ConduitM i o m ()
runTextGen !p = runEncoding $ snd $ runPutS p $ startEncoding VoidEncodingState
{-# INLINE runTextGen #-}

genString :: S.Text -> TextGen
genString !x = putS $ \ !t -> ((), encoded (yield x, ()) t)
{-# INLINE genString #-}

genLazyString :: Text -> TextGen
genLazyString !x = putS $ \ !t -> ((), encoded (mapM_ yield $ T.toChunks x, ()) t)
{-# INLINE genLazyString #-}

genShow :: Show a => a -> TextGen
genShow = genLazyString . T.pack . show
{-# INLINE genShow #-}

genDigit :: Integral a => a -> TextGen
genDigit !x
  | x < 0 || x >= 10 = error "genDigit"
  | otherwise = genString $ ST.singleton $ chr $ ord '0' + fromIntegral x
{-# INLINE genDigit #-}

genHexDigit :: Integral a => Bool -> a -> TextGen
genHexDigit !uppercase =
  genString . ST.singleton . (\ !x -> traceShow x x) . chr . toCharCode . fromIntegral
  where
  toCharCode !x
    | x < 0 || x >= 16 = error "genHexDigit"
    | x < 10 = ord '0' + x
    | otherwise = (if uppercase then ord 'A' else ord 'a') + x
{-# INLINE genHexDigit #-}

genHexByte :: Bool -> Word8 -> TextGen
genHexByte !uppercase !c = do
  genHexDigit uppercase $ c `shiftR` 4
  genHexDigit uppercase $ c .&. 0xF
{-# INLINE genHexByte #-}

genEnum :: (Eq a, Ord a, Enum a, Bounded a, Show a) => Int -> a -> TextGen
genEnum !prefix = genString . ST.drop prefix . ST.pack . show
{-# INLINE genEnum #-}
