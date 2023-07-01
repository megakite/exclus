module Lib
  ( getFieldFormats,
    mkTable,
    Matrix,
  )
where

import Data.List ((\\))

type Matrix = [[Bool]]

type Column = (Char, [Bool])

type Table = [Column]

mkTable :: Matrix -> Table
mkTable mat = zip ['a' ..] mat

rotate :: Int -> [a] -> [a]
rotate = drop <> take

isExclusive :: Column -> Column -> Bool
isExclusive x y = all (== False) $ zipWith (&&) (snd x) (snd y)

findExcl :: Table -> [Table]
findExcl [] = []
findExcl table =
  let excl = snd $ findExclImpl (table, [])
   in excl : findExcl (table \\ excl)

findExclImpl :: (Table, Table) -> (Table, Table)
findExclImpl ([], fin) = ([], fin)
findExclImpl (comp, excl) =
  if all (isExclusive $ head comp) excl
    then findExclImpl (tail comp, head comp : excl)
    else findExclImpl (tail comp, excl)

findAllCases :: Table -> Int -> [[Table]]
findAllCases _ 0 = []
findAllCases table n = findExcl table : findAllCases (rotate 1 table) (n - 1)

getFieldFormats :: Table -> [[[Char]]]
getFieldFormats table = (map . map . map) fst $ findAllCases table (length table)
