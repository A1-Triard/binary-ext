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
  , DecodingElemsRead (..)
  , elemsRead
  , skip
  , isolate
  ) where

import Control.Monad.Error.Class
import Data.Attoparsec.Text hiding (skip)
import qualified Data.ByteString as S (ByteString)
import qualified Data.ByteString as SB hiding (ByteString, head, last, init, tail)
import Data.Conduit
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
elemsRead :: (DecodingState s, DecodingElemsRead s, Monad m) => GetM s i o e m Word64
elemsRead = getC $ \ !x -> return (Right $ decodingElemsRead x, x)
{-# INLINE elemsRead #-}

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

-- | Isolate a decoder to operate with a fixed number of bytes, and fail if
-- fewer bytes were consumed, or if fewer bytes are left in the input.
-- Unlike 'S.isolate' from binary package,
-- offset from 'bytesRead' will NOT be relative to the start of @isolate@.
isolate :: (DecodingState s, Chunk (DecodingToken s), DecodingElemsRead s, Monad m)
  => Word64 -- ^ The number of bytes that must be consumed.
  -> GetM s (DecodingToken s) o e m a -- ^ The decoder to isolate.
  -> GetM s (DecodingToken s) o (Either (Maybe Word64) e) m a
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

--match :: Parser e a -> Parser e (S.Text, a)
--match ::
