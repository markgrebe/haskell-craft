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

parseOneInt :: String -> Int
parseOneInt s = case parseCSVInts s of
    Right [i] -> i
    _         -> 0

parseTwoCSVInts :: String -> (Int, Int)
parseTwoCSVInts s = case parseCSVInts s of
    Right [a,b] -> (a,b)
    _           -> (0,0)

parseThreeCSVInts :: String -> (Int, Int, Int)
parseThreeCSVInts s = case parseCSVInts s of
    Right [a,b,c] -> (a,b,c)
    _             -> (0,0,0)

parseThreeCSVFloats :: String -> (Double, Double, Double)
parseThreeCSVFloats s = case parseCSVFloats s of
    Right [a,b,c] -> (a,b,c)
    _             -> (0.0,0.0,0.0)

parseIntList :: String -> [Int]
parseIntList s = case parseBSVInts s of
    Right is -> is
    _        -> []

parseEventList :: String -> [(Int,Int,Int,Int,Int)]
parseEventList s = case parseEvents s of
    Right as -> toEvents as
    _        -> []
  where
    toEvents :: [[Int]] -> [(Int,Int,Int,Int,Int)]
    toEvents [] = []
    toEvents es =  (toEvent (head es)) ++ (toEvents (tail es))

    toEvent :: [Int] -> [(Int,Int,Int,Int,Int)]
    toEvent a = case a of
        [a,b,c,d,e] -> [(a,b,c,d,e)]
        _           -> []







