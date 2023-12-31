module Main (main) where

import Lib
import System.Exit (exitSuccess)

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
  putStrLn . getFieldFormats $ matrix

  exitSuccess
