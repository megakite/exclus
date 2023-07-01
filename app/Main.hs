-- Example:
--      a   b   c   d   e   f
-- I1   √
-- I2   √
-- I3       √   √
-- I4       √
-- I5           √   √
-- I6               √   √
-- I7                   √   √
-- I8                       √
-- > fdba ec

module Main (main) where

import Control.Monad (replicateM)
import Data.List (transpose)
import Lib
import System.Exit (exitSuccess)

inputMatrix :: Int -> Int -> IO Matrix
inputMatrix rows cols = replicateM rows $ fmap (take cols . map (toEnum . read) . words) getLine

main :: IO ()
main = do
  putStrLn "Number of microinstructions: "
  strNumInst <- getLine
  putStrLn "Number of signals: "
  strNumSig <- getLine
  let numInst = read strNumInst :: Int
  let numSig = read strNumSig :: Int

  putStrLn "Define the table:"
  matrix <- inputMatrix numInst numSig
  putStrLn . unlines . map (unwords . map reverse) . getFieldFormats . mkTable . transpose $ matrix

  exitSuccess
