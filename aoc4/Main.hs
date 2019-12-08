{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Arrow
import           Control.Lens
import           Data.List
import qualified Data.Sequence              as S
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           Data.Time.Clock
import           Data.Time.Clock.System
import           Data.Void
import           Paths_aoc4
import           Polysemy
import           Polysemy.Error
import           Polysemy.State
import           Polysemy.Trace
import           System.Directory           (doesFileExist)
import           System.Environment         (getArgs)
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type Parser = Parsec Void Text
type Input = (Int, Int)

parser :: Parser Input
parser = (,) <$> (decimal <* char '-') <*> decimal

digits :: Int -> [Int]
digits n = map (\c -> read [c]) $ show n

part1Cond :: [Int] -> Bool
part1Cond d = or (zipWith (==) d (tail d))

part2Cond :: [Int] -> Bool
part2Cond d = any ((==2) . length) (group d)

increasing :: [Int] -> Bool
increasing d = and (zipWith (<=) d (tail d))

c :: Input -> ([Int] -> Bool) -> Int
c input@(low, high) extra = length $ filter valid [low..high] where
  valid n = increasing d && extra d where
    d = digits n

challenges :: Input -> IO ()
challenges input = do
  print $ c input part1Cond
  print $ c input part2Cond
  return ()

main :: IO ()
main = do
  inputFile <- getArgs >>= \case
    [] -> do
      dataFile <- getDataFileName "input"
      exists <- doesFileExist dataFile
      return $ if exists then dataFile else "input"
    [file] -> return file
  contents <- TIO.readFile inputFile
  case parse parser "input" contents of
    Left err    -> print err
    Right input -> challenges input
