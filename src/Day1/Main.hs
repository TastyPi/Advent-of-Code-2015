{-# LANGUAGE GeneralizedNewtypeDeriving, TypeApplications #-}
module Day1.Main where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (Parser, char, parseOnly)
import Data.ByteString (hGetContents)
import Data.Functor
import System.IO (IOMode(ReadMode), withFile)

up :: Num a => Parser a
up = char '(' $> 1

down :: Num a => Parser a
down = char ')' $> -1

instruction :: Num a => Parser a
instruction = up <|> down

instructions :: Num a => Parser [a]
instructions = many instruction

withInput :: Num a => (String -> IO b) -> ([a] -> IO b) -> IO b
withInput onError onSuccess = withFile "data/Day1/input.txt" ReadMode $ \handle -> do
  contents <- hGetContents handle
  let result = parseOnly instructions contents
  either onError onSuccess result

main :: IO ()
main = withInput @Int print $ \input -> do
  putStrLn $ "Part 1: " ++ show (sum input)
  putStrLn $ "Part 2: " ++ show (length $ takeWhile (>=0) $ scanl (+) 0 input)
