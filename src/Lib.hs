module Lib
  ( getFieldFormats,
    inputMatrix,
    Matrix,
  )
where

import Control.Monad (replicateM)
import Data.Function (on)
import Data.List (minimumBy, sort, transpose, (\\))
import GHC.Float.RealFracMethods (ceilingDoubleInt)

type Matrix = [[Bool]]

type Column = (Char, [Bool])

type Table = [Column]

rotateR :: Int -> [a] -> [a]
rotateR = drop <> take

inputMatrix :: Int -> Int -> IO Matrix
inputMatrix rows cols = replicateM rows $ fmap (take cols . map (toEnum . read) . words) getLine

mkTable :: Matrix -> Table
mkTable [] = []
mkTable mat = zip ['a' ..] mat

isMutExcl :: Column -> Column -> Bool
isMutExcl x y
  | x == y = False
  | otherwise = all (== False) $ zipWith (&&) (snd x) (snd y)

getTotalLength :: [Table] -> Int
getTotalLength [] = 0
getTotalLength cols = sum $ map (ceilingDoubleInt . logBase 2 . fromIntegral . (+ 1) . length) cols

findMutExcl :: Table -> [Table]
findMutExcl [] = []
findMutExcl table =
  let excl = snd $ findMutExclImpl (table, [])
   in excl : findMutExcl (table \\ excl)

findMutExclImpl :: (Table, Table) -> (Table, Table)
findMutExclImpl ([], fin) = ([], fin)
findMutExclImpl (comp, excl) =
  if all (isMutExcl $ head comp) excl
    then findMutExclImpl (tail comp, head comp : excl)
    else findMutExclImpl (tail comp, excl)

findAllMutExclGroups :: Table -> Int -> [[Table]]
findAllMutExclGroups _ 0 = []
findAllMutExclGroups table n = findMutExcl table : findAllMutExclGroups (rotateR 1 table) (n - 1)

findShortest :: [[Table]] -> [Table]
findShortest [] = []
findShortest tables = minimumBy (compare `on` getTotalLength) tables

getFieldFormats :: Matrix -> String
getFieldFormats [] = []
getFieldFormats mat =
  let table = mkTable $ transpose mat
      len = length table
   in unwords . map sort . (map . map) fst . findShortest $ findAllMutExclGroups table len
