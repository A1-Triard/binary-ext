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

module Data.Conduit.Parsers.Text.Parser
  ( MonadMapError (..)
  , (?=>>)
  , (?>>)
  , DefaultParsingState
  , GetT
  , Parser
  , runParser
  , charsRead
  , linesRead
  , columnsRead
  , castParser
  , pCharIs
  , skipCharIs
  , pChar
  , pCharIsNot
  , satisfy
  , satisfyWith
  , skip1
  , peekChar
  , peekChar'
  , pDigit
  , pHexDigit
  , pHexByte
  , pLetter
  , pSpace
  , inClass
  , notInClass
  , pStringIs
  , skipStringIs
  , pAsciiIgnoringCaseIs
  , skipSpace
  , skipWhile
  , scan
  , runScanner
  , pString
  , pStringWhile
  , pStringWhile1
  , pStringTill
  , pRemainingString
  , pRemainingLazyString
  , skipEndOfLine
  , isEndOfLine
  , isHorizontalSpace
  , pDecimal
  , pHexadecimal
  , pSignedDecimal
  , pSignedHexadecimal
  , pDouble
  , pRational
  , pScientific
  , choice
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
  , matchP
  , tryP
  , pEnum
  , endOfInput
  ) where

import Prelude hiding (head, take, takeWhile)
import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text (inClass, notInClass, isEndOfLine, isHorizontalSpace)
import qualified Data.Attoparsec.Text as T (Parser)
import qualified Data.Attoparsec.Text as TP (parse, IResult (..))
import qualified Data.Attoparsec.Text as Tp hiding (parse, parseOnly, Parser, Result, IResult, Done, Partial, Fail, inClass, notInClass, isEndOfLine, isHorizontalSpace)
import Data.Bits
import Data.Char
import Data.Conduit
import qualified Data.Conduit.Combinators as N
import Data.List.NonEmpty hiding (take, takeWhile)
import Data.NonNull hiding (head)
import Data.Scientific (Scientific)
import Data.Text.Lazy (Text)
import qualified Data.Text as S (Text)
import qualified Data.Text as ST hiding (Text, head, last, tail, init)
import Data.Word
import Control.Monad.Error.Map
import Data.Conduit.Parsers
import Data.Conduit.Parsers.Text
import Data.Conduit.Parsers.Text.TextOffset
import Data.Conduit.Parsers.GetC

class (DecodingState s, DecodingToken s ~ S.Text, DecodingTextRead s) => DefaultParsingState s where

instance (DecodingState s, DecodingToken s ~ S.Text, DecodingTextRead s) => DefaultParsingState s where

-- | The shortening of 'GetT' for the most common use case.
type Parser e a = forall s o m. (DefaultParsingState s, Monad m) => GetT s S.Text o e m a

-- | Run a decoder presented as a 'Get' monad.
-- Returns decoder result and consumed bytes count.
runParser :: Monad m => GetT TextOffset i o e m a -> ConduitT i o m (Either e a)
runParser !g = fst <$> runGetC (startDecoding $ TextOffset 0 0 0) g
{-# INLINE runParser #-}

-- | Get the total number of bytes read to this point.
charsRead :: (DecodingState s, DecodingElemsRead s, Monad m) => GetT s i o e m Word64
charsRead = elemsRead
{-# INLINE charsRead #-}

-- | Get the total number of bytes read to this point.
linesRead :: (DecodingState s, DecodingLinesRead s, Monad m) => GetT s i o e m Word64
linesRead = getC $ \ !x -> return (Right $ decodingLinesRead x, x)
{-# INLINE linesRead #-}

-- | Get the total number of bytes read to this point.
columnsRead :: (DecodingState s, DecodingColumnsRead s, Monad m) => GetT s i o e m Word64
columnsRead = getC $ \ !x -> return (Right $ decodingColumnsRead x, x)
{-# INLINE columnsRead #-}

-- | Run the given 'S.Get' monad from binary package
-- and convert result into 'Get'.
castParser :: (DecodingState s, DecodingToken s ~ S.Text, Monad m) => T.Parser a -> GetT s S.Text o (NonEmpty String) m a
castParser !g = getC $
  go (TP.Partial $ TP.parse g) ST.empty
  where
  go (TP.Done !rest !result) !chunk !decoding =
    if ST.null rest
      then return (Right result, decoded chunk decoding)
      else leftover rest >> return (Right result, decoded (ST.take (ST.length chunk - ST.length rest) chunk) decoding)
  go (TP.Fail _ !err_context !err) !chunk !decoding = return (Left (err :| err_context), decoded chunk decoding)
  go (TP.Partial !continue) !chunk !decoding = do
    next <- maybe ST.empty toNullable <$> N.awaitNonNull
    go (continue next) next (decoded chunk decoding)
{-# INLINE castParser #-}

voidError :: Monad m => GetT s i o e m a -> GetT s i o () m a
voidError = mapError (const ())
{-# INLINE voidError #-}

anyError :: Monad m => GetT s i o e' m a -> GetT s i o e m a
anyError = mapError (const $ error "Data.Conduit.Parsers.Text.Parser.anyError")
{-# INLINE anyError #-}

skipCharIs :: Char -> Parser () ()
skipCharIs = void . pCharIs
{-# INLINE skipCharIs #-}

pCharIs :: Char -> Parser () Char
pCharIs = voidError . castParser . Tp.char
{-# INLINE pCharIs #-}

pChar :: Parser () Char
pChar = voidError $ castParser Tp.anyChar
{-# INLINE pChar #-}

pCharIsNot :: Char -> Parser () Char
pCharIsNot = voidError . castParser . Tp.notChar
{-# INLINE pCharIsNot #-}

satisfy :: (Char -> Bool) -> Parser () Char
satisfy = voidError . castParser . Tp.satisfy
{-# INLINE satisfy #-}

satisfyWith :: (Char -> a) -> (a -> Bool) -> Parser () a
satisfyWith tr = voidError . castParser . Tp.satisfyWith tr
{-# INLINE satisfyWith #-}

skip1 :: (Char -> Bool) -> Parser () ()
skip1 = voidError . castParser . Tp.skip
{-# INLINE skip1 #-}

peekChar :: Parser e (Maybe Char)
peekChar = anyError $ castParser Tp.peekChar
{-# INLINE peekChar #-}

peekChar' :: Parser e Char
peekChar' = anyError $ castParser Tp.peekChar'
{-# INLINE peekChar' #-}

pDigit :: Integral a => Parser () a
pDigit = voidError $ (\ !x -> fromIntegral $ ord x - ord '0') <$> castParser Tp.digit
{-# INLINE pDigit #-}

pHexDigit :: Integral a => Parser () a
pHexDigit =
  (fromIntegral . digitValue) <$> satisfy isHexDigit
  where
  digitValue x
    | x >= 'a' = 10 + (ord x - ord 'a')
    | x >= 'A' = 10 + (ord x - ord 'A')
    | otherwise = ord x - ord '0'
{-# INLINE pHexDigit #-}

pHexByte :: Parser () Word8
pHexByte = do
  !h <- pHexDigit
  !l <- pHexDigit
  return $ h `shiftL` 4 .|. l
{-# INLINE pHexByte #-}

pLetter :: Parser () Char
pLetter = voidError $ castParser Tp.letter
{-# INLINE pLetter #-}

pSpace :: Parser () Char
pSpace = voidError $ castParser Tp.space
{-# INLINE pSpace #-}

skipStringIs :: S.Text -> Parser () ()
skipStringIs = void . pStringIs
{-# INLINE skipStringIs #-}

pStringIs :: S.Text -> Parser () S.Text
pStringIs = voidError . castParser . Tp.string
{-# INLINE pStringIs #-}

pAsciiIgnoringCaseIs :: S.Text -> Parser () S.Text
pAsciiIgnoringCaseIs = voidError . castParser . Tp.asciiCI
{-# INLINE pAsciiIgnoringCaseIs #-}

skipSpace :: Parser () ()
skipSpace = voidError $ castParser Tp.skipSpace
{-# INLINE skipSpace #-}

skipWhile :: (Char -> Bool) -> Parser e ()
skipWhile = anyError . castParser . Tp.skipWhile
{-# INLINE skipWhile #-}

scan :: s -> (s -> Char -> Maybe s) -> Parser e S.Text
scan s = anyError . castParser . Tp.scan s
{-# INLINE scan #-}

runScanner :: s -> (s -> Char -> Maybe s) -> Parser e (S.Text, s)
runScanner s = anyError . castParser . Tp.runScanner s
{-# INLINE runScanner #-}

pString :: Int -> Parser () S.Text
pString = voidError . castParser . Tp.take
{-# INLINE pString #-}

pStringWhile :: (Char -> Bool) -> Parser e S.Text
pStringWhile = anyError . castParser . Tp.takeWhile
{-# INLINE pStringWhile #-}

pStringWhile1 :: (Char -> Bool) -> Parser () S.Text
pStringWhile1 = voidError . castParser . Tp.takeWhile
{-# INLINE pStringWhile1 #-}

pStringTill :: (Char -> Bool) -> Parser e S.Text
pStringTill = anyError . castParser . Tp.takeTill
{-# INLINE pStringTill #-}

pRemainingString :: Parser e S.Text
pRemainingString = anyError $ castParser Tp.takeText
{-# INLINE pRemainingString #-}

pRemainingLazyString :: Parser e Text
pRemainingLazyString = anyError $ castParser Tp.takeLazyText
{-# INLINE pRemainingLazyString #-}

skipEndOfLine :: Parser () ()
skipEndOfLine = voidError $ castParser Tp.endOfLine
{-# INLINE skipEndOfLine #-}

pDecimal :: Integral a => Parser () a
pDecimal = voidError $ castParser Tp.decimal
{-# INLINE pDecimal #-}

pHexadecimal :: (Integral a, Bits a) => Parser () a
pHexadecimal = voidError $ castParser Tp.hexadecimal
{-# INLINE pHexadecimal #-}

pSignedDecimal :: Integral a => Parser () a
pSignedDecimal = voidError $ castParser $ Tp.signed Tp.decimal
{-# INLINE pSignedDecimal #-}

pSignedHexadecimal :: (Integral a, Bits a) => Parser () a
pSignedHexadecimal = voidError $ castParser $ Tp.signed Tp.hexadecimal
{-# INLINE pSignedHexadecimal #-}

pDouble :: Parser () Double
pDouble = voidError $ castParser Tp.double
{-# INLINE pDouble #-}

pRational :: Fractional a => Parser () a
pRational = voidError $ castParser Tp.rational
{-# INLINE pRational #-}

pScientific :: Parser () Scientific
pScientific = voidError $ castParser Tp.scientific
{-# INLINE pScientific #-}

pEnum :: (Eq a, Ord a, Enum a, Bounded a, Show a) => Int -> Parser () a
pEnum !prefix = do
  foldl1 (<|>) [pStringIs (ST.drop prefix $ ST.pack $ show t) >> return t | t <- [minBound .. maxBound]]
{-# INLINE pEnum #-}
