module Count where

import Control.Monad
numbers :: [[Int]]
numbers = iterate step []

step :: [Int] -> [Int]
step [] = [1]
step [1] = [0,1]
step (a:xs@(b:_)) | a <= b = a+1 : xs
                  | otherwise = 0 : step xs

splits :: [Int] -> [[Int]]
splits i = sequence (fmap (\n->[0..n]) i)