module Util (rotateL) where

rotateL :: Int -> [a] -> [a]
rotateL = drop <> take
