module Data.Conduit.Parsers
  ( choice
  , count
  , option
  , many'
  , many1
  , many1'
  , manyTill
  , manyTill'
  , sepBy
  , sepBy'
  , sepBy1
  , sepBy1'
  , skipMany
  , skipMany1
  , eitherP
  , Chunk (..)
  , skip
  ) where

import Data.Attoparsec.Text hiding (skip)
import qualified Data.ByteString as S (ByteString)
import qualified Data.ByteString as SB hiding (ByteString, head, last, init, tail)
import Data.Conduit
import Data.MonoTraversable
import qualified Data.Text as S (Text)
import qualified Data.Text as ST hiding (Text, head, last, tail, init)
import Data.Word
import Data.Conduit.Parsers.GetC

class MonoFoldable c => Chunk c where
  osplitAt :: Int -> c -> (c, c)

instance Chunk S.ByteString where
  osplitAt = SB.splitAt

instance Chunk S.Text where
  osplitAt = ST.splitAt

-- | Skip ahead @n@ bytes. Fails if fewer than @n@ bytes are available.
skip :: (DecodingState s, Chunk (DecodingToken s), Monad m) => Word64 -> GetM s (DecodingToken s) o () m ()
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

--match :: Parser e a -> Parser e (S.Text, a)
--match ::
