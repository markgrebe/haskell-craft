module HaskellCraft.Parser where

import Text.Parsec
import Control.Applicative hiding ((<|>), many)

groupInts c = sepBy integer c

groupFloats c = sepBy float c

groupEvents = sepBy (groupInts (char ',')) (char '|')

plus   = char '+' *> number

minus  = (:) <$> char '-' <*> number

number = many1 digit

intPart = plus <|> minus <|> number

integer = rd <$> intPart
  where rd = read :: String -> Int

float = fmap rd $ (++) <$> intPart <*> decimal
  where rd      = read :: String -> Double
        decimal = option "" $ (:) <$> char '.' <*> number

parseCSVInts :: String -> Either ParseError [Int]
parseCSVInts input = parse (groupInts (char ',')) "(unknown)" input

parseBSVInts :: String -> Either ParseError [Int]
parseBSVInts input = parse (groupInts (char '|')) "(unknown)" input

parseCSVFloats :: String -> Either ParseError [Double]
parseCSVFloats input = parse (groupFloats (char ',')) "(unknown)" input

parseEvents :: String -> Either ParseError [[Int]]
parseEvents input = parse groupEvents "(unknown)" input

parseOneInt :: String -> Maybe Int
parseOneInt s = case parseCSVInts s of
    Right [i] -> Just i
    _         -> Nothing

parseTwoCSVInts :: String -> Maybe (Int, Int)
parseTwoCSVInts s = case parseCSVInts s of
    Right [a,b] -> Just (a,b)
    _           -> Nothing

parseThreeCSVInts :: String -> Maybe (Int, Int, Int)
parseThreeCSVInts s = case parseCSVInts s of
    Right [a,b,c] -> Just (a,b,c)
    _             -> Nothing

parseThreeCSVFloats :: String -> Maybe (Double, Double, Double)
parseThreeCSVFloats s = case parseCSVFloats s of
    Right [a,b,c] -> Just (a,b,c)
    _             -> Nothing

parseIntList :: String -> Maybe [Int]
parseIntList s = case parseCSVInts s of
    Right is -> Just is
    _        -> Nothing

--parseEventList :: String -> Maybe [(Int,Int,Int,Int,Int)]
--parseEventList s = case parseEvents s of
--    Right is -> Just is
--    _        -> Nothing

