{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
module Main where

import           Control.Lens
import qualified Data.Sequence              as S
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           Data.Time.Clock
import           Data.Time.Clock.System
import           Data.Void
import           Paths_aoc2
import           Polysemy
import           Polysemy.Embed
import           Polysemy.Error
import           Polysemy.State
import           Polysemy.Trace
import           System.Directory           (doesFileExist)
import           System.Environment         (getArgs)
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type Parser = Parsec Void Text
type Input = [Int]

data MachineState = MachineState
  { _instruction :: Int
  , _tape        :: S.Seq Int
  }

makeLenses ''MachineState

data MachineError = UnknownOpCode
                  | TooHighIndex
                  deriving (Show, Eq)

parser :: Parser Input
parser = decimal `sepBy` char ','

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
    Right input -> runM $ traceToIO $ challenges input


challenges :: Members '[Trace] r => Input -> Sem r ()
challenges input = do
  result1 <- either show show <$> runError (evalState (MachineState 0 (S.fromList input)) (part1 12 2))
  trace result1
  part2 input >>= trace . show


valueAt :: Members '[Error MachineError, State MachineState] r => Int -> Sem r Int
valueAt i = gets (^? tape . element i) >>= \case
  Nothing -> throw TooHighIndex
  Just value -> return value

readAdvance :: Members '[Error MachineError, State MachineState] r => Sem r Int
readAdvance = do
  instr <- gets (^. instruction)
  value <- valueAt instr
  modify (instruction +~ 1)
  return value

part1 :: Members '[Error MachineError, State MachineState] r => Int -> Int -> Sem r Int
part1 noun verb = do
  modify (tape . element 1 .~ noun)
  modify (tape . element 2 .~ verb)
  step
  valueAt 0

part2 :: Input -> Sem r Int
part2 input = do
  results <- sequence
    [ (100 * noun + verb,)
      <$> part2Check input noun verb
    | noun <- [0..99]
    , verb <- [0..99]
    ]
  return $ fst $ head $ filter snd results

part2Check :: Input -> Int -> Int -> Sem r Bool
part2Check input noun verb = do
  result <- runError (evalState (MachineState 0 (S.fromList input)) (part1 noun verb))
  return $ result == Right 19690720

--part2 :: Sem r (Int, Int)
--part2 = do
--  let range = [0..99]
--  results <- sequence
--    [ ((noun, verb),) <$> part1 noun verb
--    | noun <- range
--    , verb <- range
--    ]
--  return $ fst $ head $ filter ((==19690720) . snd) results
--  where


step :: Members '[Error MachineError, State MachineState] r => Sem r ()
step = readAdvance >>= \case
  1 -> do
    ai <- readAdvance
    a <- valueAt ai
    bi <- readAdvance
    b <- valueAt bi
    ci <- readAdvance
    modify (tape . element ci .~ a + b)
    step
  2 -> do
    ai <- readAdvance
    a <- valueAt ai
    bi <- readAdvance
    b <- valueAt bi
    ci <- readAdvance
    modify (tape . element ci .~ a * b)
    step
  99 -> return ()
  _ -> throw UnknownOpCode

