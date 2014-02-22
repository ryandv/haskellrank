import Data.List as L
import Data.Text as T

import Control.Monad.RWS

-- First tuple element contains visited numbers in the array.
-- Second tuple element contains unvisited numbers.
arrayInsert                    :: Int -> RWST () [Text] ([Int], [Int]) IO ()
arrayInsert x                  = modify newState where
  newState (visited, [])       = ((L.take ((L.length visited) - 1) visited) ++ [x], [])
  newState ([], (y:ys))        = ([x,y], ys)
  newState (visited, (y:ys))   = (L.take ((L.length visited) - 1) visited ++ [x, y], ys)

-- Print out our steps.
tellArray :: RWST () [Text] ([Int], [Int]) IO ()
tellArray = do
  arrState <- get
  tell $ [T.pack $ (L.unwords . L.map show $ L.reverse (fst arrState ++ snd arrState)) ++ "\n"]

showInsertionShift         :: [Int] -> RWST () [Text] ([Int], [Int]) IO ()
showInsertionShift []      = tell []
showInsertionShift (x:xs)  = showInsertionShift' x xs where

  showInsertionShift'          :: Int -> [Int] -> RWST () [Text] ([Int], [Int]) IO ()
  showInsertionShift' x []     = arrayInsert x >> tellArray
  showInsertionShift' x (y:ys) | x >= y = arrayInsert x >> tellArray
                               | otherwise = arrayInsert y >> tellArray >> showInsertionShift' x ys where

main :: IO ()
main = do
  arrLen   <- getLine
  elements <- getLine

  let array = L.reverse . L.map read . L.words $ elements

  sortresult <- execRWST (showInsertionShift array) () ([], L.tail array)
  mapM_ (putStrLn . T.unpack . T.strip) $ snd sortresult
