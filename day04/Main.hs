module Main where
import System.Environment (getArgs)
import Data.Array.IArray
    ( Ix(inRange), Array, (!), array, indices, IArray(..) )
import Data.Maybe (catMaybes, isJust)

-- PART 1

tplus :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
tplus (a, b) (x, y) = (a + x, b + y)

array2D :: [[a]] -> Array (Int, Int) a
array2D ll = array abounds [((ri,ci), e) | (ri, row) <- zip [1..] ll, (ci, e) <- zip [1..] row] where
    rowCount = length ll
    colCount = length (head ll)
    abounds = ((1,1), (rowCount, colCount))

ray :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
ray origin direction = iterate (tplus direction) origin

directions :: [(Int, Int)]
directions = [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]

lookUp :: (Ix i, IArray a1 a2) => a1 i a2 -> i -> Maybe a2
lookUp a i | inRange (bounds a) i = Just (a ! i)
lookUp _ _ = Nothing

lookupList :: (Ix i) => Array i e -> [i] -> [e]
lookupList a ixs = catMaybes $ takeWhile isJust [lookUp a i | i <- ixs]

part1 :: IO ()
part1 = do
    input <- getContents
    let rows = lines input :: [[Char]]
    let a = array2D rows
    let allWords = [lookupList a (ray origin direction) | origin <- indices a, direction <- directions]
    print (length (filter ((== "XMAS") . take 4) allWords))

-- PART 2

slashDown :: (Int, Int) -> [(Int, Int)]
slashDown a = take 3 $ ray (tplus a (-1,-1)) (1, 1)

slashUp :: (Int, Int) -> [(Int, Int)]
slashUp a = take 3 $ ray (tplus a (-1, 1)) (1, -1)

check :: String -> String -> Bool
check a b = (a == "MAS" || a == "SAM") && (b == "MAS" || b == "SAM")

cross :: (Int, Int) -> ([(Int, Int)], [(Int, Int)])
cross a = (ray upLeft (1,1), ray downLeft (1, -1)) where
    upLeft   = tplus a (-1,-1)
    downLeft = tplus a (-1, 1)

part2 :: IO ()
part2 = do
    input <- getContents
    let rows = lines input :: [[Char]]
    let a = array2D rows
    let isXmas = [check (lookupList a (slashUp origin)) (lookupList a (slashDown origin)) | origin <- indices a]
    print (length (filter id isXmas))

-- MAIN (select part)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["1"] -> part1
        ["2"] -> part2
        _ -> putStrLn "Specify either 1 or 2 for part 1 or 2 respectively"