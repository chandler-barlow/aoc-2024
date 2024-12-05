{-# LANGUAGE OverloadedStrings #-}
module Day.Three where

import Data.Attoparsec.Text 
  (string
  , digit
  , char
  , Parser
  , anyChar
  , many'
  , many1'
  , parseOnly
  )
import Data.Either (fromRight)
import Data.Functor (($>))
import Control.Monad (void)
import Control.Applicative ((<|>))
import qualified Data.Text as T
import Data.Maybe (fromMaybe, mapMaybe, catMaybes)

path = "/home/cbarlow/aoc-2024/inputs/Three/input.txt"


data Tok = DO | DONT | MUL (Int, Int) deriving Show

mul :: Parser (Maybe (Int, Int))
mul = do
  _ <- string "mul(" 
  x <- many1' digit
  _ <- char ','
  y <- many1' digit
  _ <- char ')'
  pure $ Just (read x, read y)

allMuls :: String -> [(Int, Int)]
allMuls = fromRight [] . parseOnly (f >>= pure . catMaybes) . T.pack
  where f = many' $ mul <|> (anyChar $> Nothing)

partOne :: String -> Int
partOne = sum . map (uncurry (*)) . allMuls

doTok = string "do()" $> (Just DO)

dontTok = string "don't()" $> (Just DONT)

mulTok = (fmap MUL) <$> mul

allToks :: String -> [Tok]
allToks = fromRight [] . parseOnly (f >>= pure . catMaybes) . T.pack
  where f = many' $ doTok <|> dontTok <|> mulTok <|> (anyChar $> Nothing)

goods :: [Tok] -> [(Int, Int)]
goods [] = []
goods (x:xs) = 
  case x of 
    DO -> goods xs
    DONT -> bads xs
    MUL v -> v : goods xs

bads :: [Tok] -> [(Int, Int)]
bads [] = []
bads (x:xs) = 
    case x of
      DO -> goods xs
      DONT -> bads xs
      MUL _ -> bads xs 

partTwo :: String -> Int
partTwo = sum . map (uncurry (*)) . goods . allToks
