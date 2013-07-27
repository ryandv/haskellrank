-----------------------------------------------------------
-- Solution to Artificial Intelligence/Bot Saves Princess
-- https://www.hackerrank.com/challenges/saveprincess
-----------------------------------------------------------

module Main where

import Data.List

getList :: Int -> IO[String]
getList n = if n==0 then return [] else do i <- getLine; is <- getList(n-1); return (i:is)
-- Head ends here

-- From the Haskell wiki: http://www.haskell.org/haskellwiki/Case
select :: a -> [(Bool, a)] -> a
select def = maybe def snd . find fst

-- PRE: cell is an element of the grid.
findCell			:: [String] -> Char -> Int -> (Int, Int)
findCell (x:xs) cell acc	= case findIndex (== cell) x of
  					Nothing -> findCell xs cell (1 + acc)
					Just n  -> (acc, n)

buildPath			:: Int -> (String, String) -> String
buildPath delta (dir1, dir2)	= concat $ replicate (abs delta)
     						(select "" [ ((delta > 0), dir1)
					    	           , ((delta < 0), dir2) ])



displayPathtoPrincess		:: Int -> [String] -> String
displayPathtoPrincess _ grid	= let bot	= findCell grid 'm' 0
				      princess  = findCell grid 'p' 0 in
				      buildPath (fst bot - fst princess) ("UP\n", "DOWN\n")
				   ++ buildPath (snd bot - snd princess) ("LEFT\n", "RIGHT\n")

-- Tail starts here
main = do
	n <- getLine
    	let i = read n
    	grid <- getList i
    	putStrLn.displayPathtoPrincess i $ grid
