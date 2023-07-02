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
import Util (rotateL)

type Matrix = [[Bool]]

type Column = (Char, [Bool])

type Table = [Column]

inputMatrix :: Int -> Int -> IO Matrix
inputMatrix rows cols = replicateM rows $ fmap (take cols . map (toEnum . read) . words) getLine

mkTable :: Matrix -> Table
mkTable [] = []
mkTable mat = zip ['a' ..] mat

isMutExcl :: Column -> Column -> Bool
isMutExcl x y
  | x == y = False
  | otherwise = all (== False) $ zipWith (&&) (snd x) (snd y)

-- Find mutually exclusive groups, using the implementation of pairs
_findMutExclPair :: Table -> [Table]
_findMutExclPair [] = []
_findMutExclPair table =
  let excls = snd $ _findMutExclPairImpl (table, [])
   in excls : _findMutExclPair (table \\ excls)

_findMutExclPairImpl :: (Table, Table) -> (Table, Table)
_findMutExclPairImpl ([], excls) = ([], excls)
_findMutExclPairImpl (comps, excls) =
  if all (isMutExcl $ head comps) excls
    then _findMutExclPairImpl (tail comps, head comps : excls)
    else _findMutExclPairImpl (tail comps, excls)

-- Find mutually exclusive groups, using the implementation of triples
findMutExclTriple :: Table -> [Table]
findMutExclTriple [] = []
findMutExclTriple table =
  let (_, excls, comps) = findMutExclTripleImpl (table, [], [])
   in excls : findMutExclTriple comps

findMutExclTripleImpl :: (Table, Table, Table) -> (Table, Table, Table)
findMutExclTripleImpl ([], excls, comps) = ([], excls, comps)
findMutExclTripleImpl (currs, excls, comps) =
  if all (isMutExcl $ head currs) excls
    then findMutExclTripleImpl (tail currs, head currs : excls, comps)
    else findMutExclTripleImpl (tail currs, excls, head currs : comps)

findAllMutExclGroups :: Table -> Int -> [[Table]]
findAllMutExclGroups _ 0 = []
findAllMutExclGroups table n = findMutExclTriple table : findAllMutExclGroups (rotateL 1 table) (n - 1)

getTotalLength :: [Table] -> Int
getTotalLength [] = 0
getTotalLength cols = sum $ map (ceilingDoubleInt . logBase 2 . fromIntegral . (+ 1) . length) cols

findShortest :: [[Table]] -> [Table]
findShortest [] = []
findShortest tables = minimumBy (compare `on` getTotalLength) tables

getFieldFormats :: Matrix -> String
getFieldFormats [] = []
getFieldFormats mat =
  let table = mkTable $ transpose mat
      len = length table
   in unwords . map sort . (map . map) fst . findShortest $ findAllMutExclGroups table len
