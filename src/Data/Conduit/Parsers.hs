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

module Data.Conduit.Parsers
  ( choice
  , count
  , option''
  , many''
  , many1''
  , manyTill''
  , sepBy''
  , sepBy1''
  , skipMany''
  , skipMany1''
  , eitherP
  , Chunk (..)
  , DecodingElemsRead (..)
  , elemsRead
  , endOfInput
  , skip
  , isolate
  , matchP
  , tryP
  ) where

import Control.Monad.Error.Class
import Data.Attoparsec.Text hiding (skip, endOfInput, match, try, option)
import qualified Data.ByteString as S (ByteString)
import qualified Data.ByteString as SB hiding (ByteString, head, last, init, tail)
import Data.Conduit
import qualified Data.Conduit.Combinators as N
import Data.Conduit.Lift
import Data.MonoTraversable
import qualified Data.Text as S (Text)
import qualified Data.Text as ST hiding (Text, head, last, tail, init)
import Data.Word
import Control.Monad.Error.Map
import Data.Conduit.Parsers.GetC

class MonoFoldable c => Chunk c where
  osplitAt :: Int -> c -> (c, c)

instance Chunk S.ByteString where
  osplitAt = SB.splitAt
  {-# INLINE osplitAt #-}

instance Chunk S.Text where
  osplitAt = ST.splitAt
  {-# INLINE osplitAt #-}

class DecodingElemsRead s where
  decodingElemsRead :: s -> Word64

instance (DecodingState s, DecodingElemsRead s) => DecodingElemsRead (Decoding s i) where
  decodingElemsRead = decodingElemsRead . decodingRead
  {-# INLINE decodingElemsRead #-}

-- | Get the total number of bytes read to this point.
elemsRead :: (DecodingState s, DecodingElemsRead s, Monad m) => GetT s i o e m Word64
elemsRead = getC $ \ !x -> return (Right $ decodingElemsRead x, x)
{-# INLINE elemsRead #-}

-- | Skip ahead @n@ bytes. Fails if fewer than @n@ bytes are available.
skip :: (DecodingState s, Chunk (DecodingToken s), Monad m) => Word64 -> GetT s (DecodingToken s) o () m ()
skip !n = getC $
  go 0
  where
  go !consumed !decoding
    | consumed > n = error "Data.Binary.Conduit.Get.skip"
    | consumed == n = return (Right (), decoding)
    | otherwise = do
      !mi <- await
      case mi of
        Nothing -> return (Left (), decoding)
        Just !i -> do
          let !gap = n - consumed
          if gap >= fromIntegral (olength i)
            then do
              go (consumed + fromIntegral (olength i)) (decoded i decoding)
            else do
              let (!got, !rest) = osplitAt (fromIntegral gap) i
              leftover rest
              return (Right (), decoded got decoding)
{-# INLINE skip #-}

-- | Isolate a decoder to operate with a fixed number of bytes, and fail if
-- fewer bytes were consumed, or if fewer bytes are left in the input.
-- Unlike 'S.isolate' from binary package,
-- offset from 'bytesRead' will NOT be relative to the start of @isolate@.
isolate :: (DecodingState s, Chunk (DecodingToken s), DecodingElemsRead s, Monad m)
  => Word64 -- ^ The number of bytes that must be consumed.
  -> GetT s (DecodingToken s) o e m a -- ^ The decoder to isolate.
  -> GetT s (DecodingToken s) o (Either (Maybe Word64) e) m a
isolate !n !g = do
  !o1 <- elemsRead
  !r <- getC $ flip runStateC $ runExceptC $ fuseLeftovers id (go 0) (exceptC $ stateC $ flip runGetC $ mapError Right g)
  !o2 <- elemsRead
  if o2 - o1 < n
    then throwError $ Left $ Just $ o2 - o1
    else return r
  where
  go consumed
    | consumed > n = error "Data.Binary.Conduit.Get.isolate"
    | consumed == n = return ()
    | otherwise = do
      !i <- maybe (throwError $ Left Nothing) return =<< await
      let !gap = n - consumed
      if gap >= fromIntegral (olength i)
        then do
          yield i
          go $ consumed + fromIntegral (olength i)
        else do
          let (!h, !t) = osplitAt (fromIntegral gap) i
          leftover t
          yield h
{-# INLINE isolate #-}

endOfInput :: (DecodingState s, MonoFoldable (DecodingToken s), Monad m) => GetT s (DecodingToken s) o () m ()
endOfInput = do
  end <- N.nullE
  if end then return () else throwError ()
{-# INLINE endOfInput #-}

matchP :: (DecodingState s, Monoid (DecodingToken s), Monad m) => GetT s (DecodingToken s) o e m a -> GetT s (DecodingToken s) o e m (DecodingToken s, a)
matchP !p = (\(!t, !r) -> (foldl (flip mappend) mempty t, r)) <$> mapError snd (trackP p)
{-# INLINE matchP #-}
