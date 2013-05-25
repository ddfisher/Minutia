{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (ByteString, append)
import qualified Data.ByteString.Char8 as B8

import Data.Attoparsec as P
import Data.Attoparsec.Char8 (char8, endOfLine)
import qualified Data.Attoparsec.Char8 as P8
import qualified Data.Attoparsec.Combinator as PC

import Data.List
import Data.Traversable
import Control.Applicative hiding (many)
import Data.Word (Word8)
import Data.Char

-- requestLine :: Parser Request
-- requestLine = do
--   method <- P.takeWhile1 isToken <* char8 ' '
--   cheesyUri <- P8.takeWhile1 (/=' ') <* char8 ' '
--   ver <- version <* endOfLine
--   return (Request method cheesyUri ver)

whiteSpace = P.takeWhile P8.isSpace_w8

version :: Parser (Int, Int)
version = string "HTTP/" *> ((,) <$> (P8.decimal <* char8 '.') <*> P8.decimal)

statusLine = ((,,) <$> version) <*> (char8 ' ' *> P8.decimal) <*> (char8 ' ' *> P.takeTill P8.isEndOfLine <* endOfLine)


media_range = (anyType <|> anySubtype <|> specificSubtype)
    where anyType = liftA2 (,) (string "*" <* string "/") (string "*")
          anySubtype = liftA2 (,) (token <* string "/") (string "*")
          specificSubtype = liftA2 (,) (token <* string "/") token

token :: Parser ByteString
token = P8.takeWhile P8.isAlpha_ascii

spacedStrings :: [ByteString] -> Parser ByteString
spacedStrings strs = foldl1 append <$> sequenceA (intersperse whiteSpace parseStrs)
    where parseStrs = map string strs

accept_params :: Parser (Double, [(ByteString, Maybe ByteString)])
accept_params = (,) <$> (spacedStrings [";", "q", "="] *> P8.double) <*> many accept_extension

accept_extension = ((,) <$> (string ";" *> token)) <*> option Nothing (string "=" *> (Just <$> token))


data TestAppl a = TestAppl String a
    deriving Show

getString (TestAppl str _) = str

instance Functor TestAppl where
    f `fmap` (TestAppl str v) = TestAppl str (f v)

instance Applicative TestAppl where
    pure v = TestAppl "" v
    (TestAppl str1 f) <*> (TestAppl str2 v) = TestAppl (str1 ++ " : " ++  str2) (f v)
