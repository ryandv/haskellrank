import Data.List

-- Functional if statement from
-- http://www.haskell.org/haskellwiki/If-then-else
if'                     :: Bool -> a -> a -> a
if' True x _            = x
if' False _ y           = y

-- Reads in a space-delimited string of numbers as a list of integers.
intsFromStr             :: String -> [Int]
intsFromStr             = map read . words

-- Turns a list of integers into a space-delimited string of numbers.
intsToStr               :: [Int] -> String
intsToStr               = intercalate " " . map show

-- Replaces the value at position "n" in a list. If n > length lst, 
-- will append val to lst.
replace			:: Int -> Int -> [Int] -> [Int]
replace n val lst	= (take n lst) ++ (val : (drop (n + 1) lst))

-- Implements one step of insertion sort. Shifts an element further
-- down the list or places an unsorted element in its place.
--
-- Builds up a list of lists, one per step of the sort.
insertionStep				:: Int -> Int -> (Int -> Int -> Bool) -> [Int] -> [[Int]]
insertionStep (-1) unsorted _ lst	= [(unsorted : (drop 1 lst))]
insertionStep n unsorted ord lst
	| (lst !! n) `ord` unsorted	= [newList unsorted]
	| otherwise			= (newList $ lst !! n)
						: (insertionStep (n - 1) unsorted ord . newList $ lst !! n)
   	where newList newval	= replace (n + 1) newval lst

main			:: IO ()
main			= do
				len	<- fmap read getLine
				lst	<- fmap intsFromStr getLine
				putStr . unlines . map intsToStr . insertionStep (len - 2) (last lst) (<=) $ take (len - 1) lst
