-----------------------------------------------------------
-- Solution to Artificial Intelligence/Bot Saves Princess 2
-- https://www.hackerrank.com/challenges/saveprincess2
-----------------------------------------------------------

module Main where

import Data.List

getList :: Int -> IO[String]
getList n = if n==0 then return [] else do i <- getLine; is <- getList(n-1); return (i:is)
-- Head ends here

-- From the Haskell wiki: http://www.haskell.org/haskellwiki/Case
select :: a -> [(Bool, a)] -> a
select def = maybe def snd . find fst

readPos				:: String -> (Int, Int)
readPos xs			= let line = words xs in
					(read (line !! 0), read (line !! 1))

findCell			:: [String] -> Char -> Int -> (Int, Int)
findCell (x:xs) cell acc	= case findIndex (== cell) x of
  					Nothing -> findCell xs cell (1 + acc)
					Just n  -> (acc, n)

step				:: Int -> Int -> String
step dy dx			| dx < 0	= "RIGHT\n"
				| dx > 0	= "LEFT\n"
				| dy < 0	= "DOWN\n"
				| dy > 0	= "UP\n"
				| otherwise	= ""

nextMove			:: Int -> Int -> Int -> [String] -> String
nextMove _ r c grid		= let princess  = findCell grid 'p' 0 in
				      step (r - fst princess) (c - snd princess)

-- Tail starts here
main = do
	n <- getLine
	m <- getLine
    	let i = read n
	let bot = readPos m
    	grid <- getList i
    	putStrLn((uncurry $ nextMove i) bot grid)
