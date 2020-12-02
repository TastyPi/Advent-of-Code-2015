{-# LANGUAGE GeneralizedNewtypeDeriving, TypeApplications #-}
module Day1.Main where

import Control.Monad
import Data.Functor
import System.IO
import Text.Parsec
import Text.Parsec.Text.Lazy

withInput :: (String -> IO a) -> IO a
withInput f = withFile "data/Day1/input.txt" ReadMode $ hGetContents >=> f

newtype Instruction a = Delta a
  deriving Show

instance Num a => Semigroup (Instruction a) where
  (Delta x) <> (Delta y) = Delta $ x + y

instance Num a => Monoid (Instruction a) where
  mempty = Delta 0

mkInstruction :: a -> Instruction a
mkInstruction = Delta

up :: Num a => Parser (Instruction a)
up = char '(' $> mkInstruction 1

down :: Num a => Parser (Instruction a)
down = char ')' $> mkInstruction (-1)

instruction :: Num a => Parser (Instruction a)
instruction = up <|> down

instructions :: Num a => Parser (Instruction a)
instructions = fmap mconcat $ many instruction

main :: IO ()
main = parseFromFile (instructions @Int) "data/Day1/input.txt" >>= print
