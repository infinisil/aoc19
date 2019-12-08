{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
module Main where

import           Control.Lens
import           Control.Monad
import qualified Data.Sequence              as S
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           Data.Time.Clock
import           Data.Time.Clock.System
import           Data.Void
import           Paths_aoc5
import           Polysemy
import           Polysemy.Embed
import           Polysemy.Error
import           Polysemy.Input
import           Polysemy.Output
import           Polysemy.State
import           Polysemy.Trace
import           System.Directory           (doesFileExist)
import           System.Environment         (getArgs)
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type Parser = Parsec Void Text
type In = [Int]

data MachineState = MachineState
  { _instruction :: Int
  , _tape        :: S.Seq Int
  }

makeLenses ''MachineState

data MachineError = UnknownOpCode
                  | TooHighIndex
                  deriving (Show, Eq)

parser :: Parser In
parser = signed (return ()) decimal `sepBy` char ','

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

part1 :: Member Trace r => In -> Sem r ()
part1 input = do
  result <- runError $ runOutputList $ runInputConst 1 $ evalState (MachineState 0 (S.fromList input)) step
  case result of
    Left err           -> trace $ show err
    Right (outputs, _) -> if all (==0) (init outputs)
      then trace $ show (last outputs)
      else trace $ "Some tests failed, output is: " ++ show outputs

part2 :: Member Trace r => In -> Sem r ()
part2 input = do
  result <- runError $ runOutputList $ runInputConst 5 $ evalState (MachineState 0 (S.fromList input)) step
  case result of
    Left err            -> trace $ show err
    Right ([output], _) -> trace $ show output

challenges :: Members '[Trace] r => In -> Sem r ()
challenges input = do
  part1 input
  part2 input


valueAt :: Members '[Error MachineError, State MachineState] r => Int -> Sem r Int
valueAt i = gets (^? tape . element i) >>= \case
  Nothing -> throw TooHighIndex
  Just value -> return value

readAdvance :: Members '[Trace, Error MachineError, State MachineState] r => Sem r Int
readAdvance = do
  instr <- gets (^. instruction)
  value <- valueAt instr
  modify (instruction +~ 1)
  return value

step :: Members '[Trace, Input Int, Output Int, Error MachineError, State MachineState] r => Sem r ()
step = readAdvance >>= \instr -> case instr `mod` 100 of
  1 -> do
    a' <- readAdvance
    a <- if (instr `div` 100) `mod` 10 == 0
      then valueAt a'
      else return a'
    b' <- readAdvance
    b <- if (instr `div` 1000) `mod` 10 == 0
      then valueAt b'
      else return b'
    ci <- readAdvance
    modify (tape . element ci .~ a + b)
    step
  2 -> do
    a' <- readAdvance
    a <- if (instr `div` 100) `mod` 10 == 0
      then valueAt a'
      else return a'
    b' <- readAdvance
    b <- if (instr `div` 1000) `mod` 10 == 0
      then valueAt b'
      else return b'
    ci <- readAdvance
    modify (tape . element ci .~ a * b)
    step
  3 -> do
    v <- input
    p <- readAdvance
    modify (tape . element p .~ v)
    step
  4 -> do
    a' <- readAdvance
    a <- if (instr `div` 100) `mod` 10 == 0
      then valueAt a'
      else return a'
    output a
    step
  5 -> do
    c' <- readAdvance
    c <- if (instr `div` 100) `mod` 10 == 0
      then valueAt c'
      else return c'
    i' <- readAdvance
    i <- if (instr `div` 1000) `mod` 10 == 0
      then valueAt i'
      else return i'
    when (c /= 0) $ modify (instruction .~ i)
    step
  6 -> do
    c' <- readAdvance
    c <- if (instr `div` 100) `mod` 10 == 0
      then valueAt c'
      else return c'
    i' <- readAdvance
    i <- if (instr `div` 1000) `mod` 10 == 0
      then valueAt i'
      else return i'
    when (c == 0) $ modify (instruction .~ i)
    step
  7 -> do
    a' <- readAdvance
    a <- if (instr `div` 100) `mod` 10 == 0
      then valueAt a'
      else return a'
    b' <- readAdvance
    b <- if (instr `div` 1000) `mod` 10 == 0
      then valueAt b'
      else return b'
    ci <- readAdvance
    modify (tape . element ci .~ if a < b then 1 else 0)
    step
  8 -> do
    a' <- readAdvance
    a <- if (instr `div` 100) `mod` 10 == 0
      then valueAt a'
      else return a'
    b' <- readAdvance
    b <- if (instr `div` 1000) `mod` 10 == 0
      then valueAt b'
      else return b'
    ci <- readAdvance
    modify (tape . element ci .~ if a == b then 1 else 0)
    step
  99 -> return ()
  _ -> throw UnknownOpCode

