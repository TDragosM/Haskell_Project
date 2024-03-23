module Bencode.Parser where

import Bencode.Value
import qualified Data.List as L
import Parsec (Parser, andThen, orElse, pMap, pThen, between)
import qualified Parsec as P
import Result

-- | Parse a bencode value
--
-- >>> P.runParser value "i10e"
-- Success (BencodeInt 10, "")
--
-- >>> P.runParser value "3:abc"
-- Success (BencodeString "abc", "")
--
-- >>> P.runParser value "l3:abc4:abcde"
-- Success (BencodeList [BencodeString "abc",BencodeString "abcd"], "")
--
-- >>> P.runParser value "d3:abci10ee"
-- Success (BencodeDict [("abc",BencodeInt 10)], "")
value :: Parser BencodeValue
value =
  (pMap BencodeString string)
    `orElse` (pMap BencodeInt int)
    `orElse` (pMap BencodeList list)
    `orElse` (pMap BencodeDict dict)

-- | Parse a bencode integer
--
-- >>> P.runParser int "i10e"
-- Success (10, "")
-- | Parse a bencode integer
--
-- >>> P.runParser int "i10e"
-- Success (10, "")
--
--Exercise 2.6.5 1.5p + 0.5p
--Implement the int function which parses bencode integers. A bencode integer starts
--with the character i , followed by digits representing the number in decimal, and ends
--with the character e .
--Examples:
--The number 10, would be represented in bencode as i10e .

int :: Parser Int
int = P.between (P.char 'i') (P.char 'e') P.number



-- | Parse a bencode string
--
-- >>> P.runParser string "3:abc"
-- Success ("abc", "")
--
--Implement the string function which parses bencode strings. A bencode string starts
--with a number that represents the number of characters that will follow for the string,
--the character : and the characters for the string.
--Examples:
--The string “Haskell” would be represented in bencode as 7:Haskell .
--Hints:
--You should consider using (some of) the following functions from the Parser module:
--with (-- | Chain two parsers, feeding both the result and the remaining input from the first parser to the second parser.)
--, take(-- | Parser that consumes a fixed number of characters.)


string :: Parser String
string = P.with P.number (\n -> P.char ':' `pThen` P.take n)

-- | Parse a bencode list
--
-- >>> P.runParser list "li1ei2ee"
-- Success ([BencodeInt 1,BencodeInt 2], "")
--
-- >>> P.runParser list "l1:a1:be"
-- Success ([BencodeString "a",BencodeString "b"], "")
--Implement the list function which parses bencode lists. A bencode list starts with
--the character l , followed by an arbitrary number of bencode values and ends with the
--character e .
--Examples:
--The Haskell list [1, 2] would be represented in bencode as li1ei2ee . We can also represent the heterogeneous list (i.e. which contains elements of different types) that would be
--invalid in Haskell and Elm, but would be valid in Python or JavaScript ["a", 1, "hello"]
--as l1:ai1e5:helloe .

list :: Parser [BencodeValue]
list = P.between (P.char 'l') (P.char 'e') (P.many value)

-- | Parse a bencode dict
--
-- >>> P.runParser dict "d1:ai1e1:bi2ee"
-- Success ([(BencodeString "a", BencodeInt 1),(BencodeString "b",BencodeInt 2)], "")
--Implement the dict function which parses for bencode dictionaries. A bencode dictionary (dict) starts with the character d , followed by an arbitrary number of (string,
--bencode value) pairs and ends with the character e .
--Examples:
--The JSON dict {"a": 1} would be represented in bencode as d1:ai1ee and
--the JSON dict {"name": "John", "age": 35} would be represented in bencode as
--d4:name4:John3:agei35ee .
-- andThen | Chain two parses, running the second parser with the remaining input from the first parser

dict :: Parser [BencodeKW]
dict = P.between (P.char 'd') (P.char 'e') (P.many (P.andThen (string) (value)))

-- | Convenience wrapper for `value`
--
-- >>> parse "i10e"
-- Success (BencodeInt 10)
parse :: String -> Result P.ParseError BencodeValue
parse input = fst <$> P.runParser value input
