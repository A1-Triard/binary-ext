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

-- | At the first look, Data.Binary.Conduit.Get module is very similar with Data.Binary.Get.
-- The main differences between them are the following.
-- While the 'S.Get' from binary is a very custom monad,
-- the local 'Get' is 'ConduitM', which leads to easy integration in complicated format parsing.
-- The Data.Binary.Get module does not have a function to create custom 'S.Get' monad,
-- this module provides 'getC'.
-- Unlike 'isolate' from binary, local 'isolate' does not "cut" bytes counter.
-- While the binary's 'S.Get' is 'MonadFail', which leads to very ugly errors handling
-- in complicated cases, local 'Get' is 'MonadError'.

module Data.Conduit.Parsers.Text.Parser
  ( MonadMapError (..)
  , (?=>>)
  , (?>>)
  , DefaultParsingState
  , GetM
  , Parser
  , runParser
  , charsRead
  , linesRead
  , columnsRead
  , castParser
  , char
  , anyChar
  , notChar
  , satisfy
  , satisfyWith
  , skip1
  , peekChar
  , peekChar'
  , digit
  , letter
  , space
  , inClass
  , notInClass
  , string
  , asciiCI
  , skipSpace
  , skipWhile
  , scan
  , runScanner
  , take
  , takeWhile
  , takeWhile1
  , takeTill
  , takeText
  , takeLazyText
  , endOfLine
  , isEndOfLine
  , isHorizontalSpace
  , decimal
  , hexadecimal
  , signedDecimal
  , signedHexadecimal
  , double
  , rational
  , scientific
  , choice
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
  ) where

import Prelude hiding (head, take, takeWhile)
import Data.Attoparsec.Text (inClass, notInClass, isEndOfLine, isHorizontalSpace)
import qualified Data.Attoparsec.Text as T (Parser)
import qualified Data.Attoparsec.Text as TP (parse, IResult (..))
import qualified Data.Attoparsec.Text as Tp hiding (parse, parseOnly, Parser, Result, IResult, Done, Partial, Fail, inClass, notInClass, isEndOfLine, isHorizontalSpace)
import Data.Bits
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

-- | The shortening of 'GetM' for the most common use case.
type Parser e a = forall s o m. (DefaultParsingState s, Monad m) => GetM s S.Text o e m a

-- | Run a decoder presented as a 'Get' monad.
-- Returns decoder result and consumed bytes count.
runParser :: Monad m => GetM TextOffset i o e m a -> ConduitM i o m (Either e a)
runParser !g = fst <$> runGetC (startDecoding $ TextOffset 0 0 0) g
{-# INLINE runParser #-}

-- | Get the total number of bytes read to this point.
charsRead :: (DecodingState s, DecodingElemsRead s, Monad m) => GetM s i o e m Word64
charsRead = elemsRead
{-# INLINE charsRead #-}

-- | Get the total number of bytes read to this point.
linesRead :: (DecodingState s, DecodingLinesRead s, Monad m) => GetM s i o e m Word64
linesRead = getC $ \ !x -> return (Right $ decodingLinesRead x, x)
{-# INLINE linesRead #-}

-- | Get the total number of bytes read to this point.
columnsRead :: (DecodingState s, DecodingColumnsRead s, Monad m) => GetM s i o e m Word64
columnsRead = getC $ \ !x -> return (Right $ decodingColumnsRead x, x)
{-# INLINE columnsRead #-}

-- | Run the given 'S.Get' monad from binary package
-- and convert result into 'Get'.
castParser :: (DecodingState s, DecodingToken s ~ S.Text, Monad m) => T.Parser a -> GetM s S.Text o (NonEmpty String) m a
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

voidError :: Monad m => GetM s i o e m a -> GetM s i o () m a
voidError = mapError (const ())
{-# INLINE voidError #-}

anyError :: Monad m => GetM s i o e' m a -> GetM s i o e m a
anyError = mapError (const $ error "Data.Conduit.Parsers.Text.Parser.anyError")

isNotEnoughInput :: String -> Bool
isNotEnoughInput ('n' : _) = True -- "not enough input"
isNotEnoughInput _ = False -- "string", "stringCI", "takeWhile1", "satisfy", "satisfyWith", "skip", "takeWith"
{-# INLINE isNotEnoughInput #-}

boolError :: Monad m => GetM s i o (NonEmpty String) m a -> GetM s i o Bool m a
boolError = mapError (isNotEnoughInput . head)
{-# INLINE boolError #-}

char :: Char -> Parser Bool Char
char = boolError . castParser . Tp.char
{-# INLINE char #-}

anyChar :: Parser () Char
anyChar = voidError $ castParser Tp.anyChar
{-# INLINE anyChar #-}

notChar :: Char -> Parser Bool Char
notChar = boolError . castParser . Tp.notChar
{-# INLINE notChar #-}

satisfy :: (Char -> Bool) -> Parser Bool Char
satisfy = boolError . castParser . Tp.satisfy
{-# INLINE satisfy #-}

satisfyWith :: (Char -> a) -> (a -> Bool) -> Parser Bool a
satisfyWith tr = boolError . castParser . Tp.satisfyWith tr
{-# INLINE satisfyWith #-}

skip1 :: (Char -> Bool) -> Parser Bool ()
skip1 = boolError . castParser . Tp.skip
{-# INLINE skip1 #-}

peekChar :: Parser e (Maybe Char)
peekChar = anyError $ castParser Tp.peekChar
{-# INLINE peekChar #-}

peekChar' :: Parser e Char
peekChar' = anyError $ castParser Tp.peekChar'
{-# INLINE peekChar' #-}

digit :: Parser Bool Char
digit = boolError $ castParser Tp.digit
{-# INLINE digit #-}

letter :: Parser Bool Char
letter = boolError $ castParser Tp.letter
{-# INLINE letter #-}

space :: Parser Bool Char
space = boolError $ castParser Tp.space
{-# INLINE space #-}

string :: S.Text -> Parser Bool S.Text
string = boolError . castParser . Tp.string
{-# INLINE string #-}

asciiCI :: S.Text -> Parser Bool S.Text
asciiCI = boolError . castParser . Tp.asciiCI
{-# INLINE asciiCI #-}

skipSpace :: Parser Bool ()
skipSpace = boolError $ castParser Tp.skipSpace
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

take :: Int -> Parser () S.Text
take = voidError . castParser . Tp.take
{-# INLINE take #-}

takeWhile :: (Char -> Bool) -> Parser e S.Text
takeWhile = anyError . castParser . Tp.takeWhile
{-# INLINE takeWhile #-}

takeWhile1 :: (Char -> Bool) -> Parser () S.Text
takeWhile1 = voidError . castParser . Tp.takeWhile
{-# INLINE takeWhile1 #-}

takeTill :: (Char -> Bool) -> Parser e S.Text
takeTill = anyError . castParser . Tp.takeTill
{-# INLINE takeTill #-}

takeText :: Parser e S.Text
takeText = anyError $ castParser Tp.takeText
{-# INLINE takeText #-}

takeLazyText :: Parser e Text
takeLazyText = anyError $ castParser Tp.takeLazyText
{-# INLINE takeLazyText #-}

endOfLine :: Parser Bool ()
endOfLine = boolError $ castParser Tp.endOfLine
{-# INLINE endOfLine #-}

decimal :: Integral a => Parser Bool a
decimal = boolError $ castParser Tp.decimal
{-# INLINE decimal #-}

hexadecimal :: (Integral a, Bits a) => Parser Bool a
hexadecimal = boolError $ castParser Tp.hexadecimal
{-# INLINE hexadecimal #-}

signedDecimal :: Integral a => Parser Bool a
signedDecimal = boolError $ castParser $ Tp.signed Tp.decimal
{-# INLINE signedDecimal #-}

signedHexadecimal :: (Integral a, Bits a) => Parser Bool a
signedHexadecimal = boolError $ castParser $ Tp.signed Tp.hexadecimal
{-# INLINE signedHexadecimal #-}

double :: Parser Bool Double
double = boolError $ castParser Tp.double
{-# INLINE double #-}

rational :: Fractional a => Parser Bool a
rational = boolError $ castParser Tp.rational
{-# INLINE rational #-}

scientific :: Parser Bool Scientific
scientific = boolError $ castParser Tp.scientific
{-# INLINE scientific #-}
