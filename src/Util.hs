module Util (rotateR) where

rotateR :: Int -> [a] -> [a]
rotateR = drop <> take
