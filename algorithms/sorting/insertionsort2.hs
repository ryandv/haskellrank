-----------------------------------------------------------
-- Solution to Algorithms/Sorting/Insertion Sort - Part 2
-- https://www.hackerrank.com/challenges/insertionsort2
-----------------------------------------------------------
import Control.Monad.Writer

tellArray                 :: [Int] -> Writer String [Int]
tellArray xs              = (tell $ (unwords . map show $ xs) ++ "\n") >> return xs

insertionSort             :: [Int] -> Writer String [Int]
insertionSort (x:xs)      = insertionSort' xs [x] where

  insertionSort'              :: [Int] -> [Int] -> Writer String [Int]
  insertionSort' [] acc       = return acc
  insertionSort' (x:xs) acc   = tellArray (insertionShift x acc ++ xs) >> insertionSort' xs (insertionShift x acc)

  insertionShift            :: Int -> [Int] -> [Int]
  insertionShift x []       = [x]
  insertionShift x (y:ys)   | x <= y    = x:y:ys
                            | otherwise = y:insertionShift x ys

main :: IO ()
main = do
  getLine
  elements <- getLine

  let array = map read . words $ elements

  putStrLn . snd . runWriter $ insertionSort array
