-- Example:
--       a   b   c   d   e   f
--  I1   √
--  I2   √
--  I3       √   √
--  I4       √
--  I5           √   √
--  I6               √   √
--  I7                   √   √
--  I8                       √
-- Output:
--  adbf ce
--  bdfa ce
--  cea dfb
--  dfab ec
--  eab fc d
--  fabd ce

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
