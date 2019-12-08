{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Lens               hiding (noneOf)
import           Data.List
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe
import qualified Data.Sequence              as S
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           Data.Time.Clock
import           Data.Time.Clock.System
import           Data.Void
import           Paths_aoc6
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
type Orbit = (String, String)
type Input = [Orbit]

parser :: Parser Input
parser = many (parseOrbit <* newline) <* eof where
  parseOrbit :: Parser Orbit
  parseOrbit = (,) <$> (some (noneOf ")\n") <* char ')') <*> some (noneOf ")\n")

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

loeb :: Functor f => f (f a -> a) -> f a
loeb x = go where go = fmap ($ go) x

toAdj :: Input -> Map String (Set String)
toAdj []                  = Map.empty
toAdj ((center,obj):rest) = Map.insertWith Set.union obj (Set.singleton center) $ toAdj rest

toFullAdj :: Input -> Map String (Set String)
toFullAdj []                  = Map.empty
toFullAdj ((center,obj):rest) =
  Map.insertWith Set.union obj (Set.singleton center) $
  Map.insertWith Set.union center (Set.singleton obj) $ toFullAdj rest

closed :: Map String (Set String) -> Map String (Set String)
closed orbits = x where
  x :: Map String (Set String)
  x = Map.fromSet (\obj ->
                     let y = Map.findWithDefault Set.empty obj orbits
                      in Set.unions (y : map (\v -> Map.findWithDefault Set.empty v x) (Set.toAscList y))
                     ) (Map.keysSet orbits)

part1 :: Input -> Int
part1 input = sum $ map Set.size $ Map.elems $ closed $ toAdj input

search :: Map String (Set String) -> String -> String -> Set String -> Maybe Int
search orbits start goal visited
  | start == goal = Just (-2)
  | otherwise =
    let next = Map.findWithDefault Set.empty start orbits Set.\\ visited
        values = mapMaybe (\new -> (+1) <$> search orbits new goal (Set.insert new visited)) (Set.toAscList next)
    in if null values then Nothing
    else Just $ minimum values


challenges :: Input -> IO ()
challenges input = do
  print $ part1 input
  print $ fromJust $ search (toFullAdj input) "YOU" "SAN" Set.empty

  return ()

