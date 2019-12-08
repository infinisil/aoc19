{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Lens
import           Data.List
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Ord
import qualified Data.Sequence              as S
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           Data.Time.Clock
import           Data.Time.Clock.System
import           Data.Void
import           Paths_aoc3
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
data Direction = R | D | L | U deriving (Show, Eq)
type WireSegment = (Direction, Int)
type WirePath = [WireSegment]
type Input = (WirePath, WirePath)

parser :: Parser Input
parser = (,) <$> parseWirePath <*> parseWirePath where
  parseWirePath :: Parser WirePath
  parseWirePath = parseWireSegment `sepBy` char ',' <* newline

  parseWireSegment :: Parser WireSegment
  parseWireSegment = (,) <$> parseDirection <*> decimal

  parseDirection :: Parser Direction
  parseDirection = oneOf "RDLU" >>= \case
    'R' -> return R
    'D' -> return D
    'L' -> return L
    'U' -> return U

type Coords = (Int, Int)

manhattanDist :: Coords -> Int
manhattanDist (x, y) = abs x + abs y

move :: Direction -> (Int, Int) -> (Int, Int)
move R (x, y) = (x + 1, y)
move D (x, y) = (x, y - 1)
move L (x, y) = (x - 1, y)
move U (x, y) = (x, y + 1)

pathCoords :: WirePath -> Coords -> Int -> Map Coords Int
pathCoords [] _ _ = Map.empty
pathCoords ((dir, 0):rest) start steps = pathCoords rest start steps
pathCoords ((dir, count):rest) start steps = Map.insert newStart newSteps $ pathCoords ((dir, count - 1):rest) newStart newSteps where
  newStart = move dir start
  newSteps = steps + 1

pathCoordsFromOrigin :: WirePath -> Map Coords Int
pathCoordsFromOrigin wire = pathCoords wire (0, 0) 0

intersections :: Input -> Map Coords (Int, Int)
intersections (wire1, wire2) = Map.intersectionWith (,) wire1Coords wire2Coords where
  wire1Coords = pathCoordsFromOrigin wire1
  wire2Coords = pathCoordsFromOrigin wire2

nearestPoint :: Foldable f => f Coords -> Coords
nearestPoint = minimumBy (comparing manhattanDist)

part1 :: Input -> Int
part1 input = manhattanDist $ nearestPoint (Map.keysSet $ intersections input)

part2 :: Input -> Int
part2 input = minimum $  Map.map (uncurry (+)) $ intersections input

challenges :: Input -> IO ()
challenges input = do
  print $ part1 input
  print $ part2 input
  return ()


time :: IO a -> IO a
time io = do
  start <- getSystemTime
  result <- io
  stop <- getSystemTime
  print $ systemToUTCTime stop `diffUTCTime` systemToUTCTime start
  return result

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
