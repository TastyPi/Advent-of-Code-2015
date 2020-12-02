{-# LANGUAGE GeneralizedNewtypeDeriving, TypeApplications #-}
module Day1.Main where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8 (char)
import Data.Attoparsec.ByteString.Lazy
import Data.ByteString.Lazy
import Data.Functor
import System.IO (IOMode(ReadMode), withFile)

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

instructions :: Num a => Parser [Instruction a]
instructions = many instruction

withInput :: (ByteString -> IO a) -> IO a
withInput f = withFile "data/Day1/input.txt" ReadMode $ hGetContents >=> f

main :: IO ()
main = withInput $ print . fmap mconcat . parse (instructions @Int)
