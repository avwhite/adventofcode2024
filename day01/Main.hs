module Main where

import Data.List (sort, transpose, group)
import Data.Map
import System.Environment (getArgs)

-- PART 1

getTwoColsOrFail :: [[a]] -> IO ([a], [a])
getTwoColsOrFail [first, second] = return (first, second)
getTwoColsOrFail _ = fail "need two columns of data"

part1 :: IO ()
part1 = do
    input <- getContents
    let rows = fmap (fmap read . words) (lines input) :: [[Integer]]
    let cols = transpose rows
    let sortedCols = fmap sort cols
    (first, second) <- getTwoColsOrFail sortedCols
    print $ sum (fmap (abs . uncurry (-)) (zip first second))

-- PART 2

histogram :: (Ord a) => [a] -> Map a Int
histogram l = fromAscList $ fmap bucket (group (sort l)) where
    bucket sl = (head sl, length sl)

similarityScore :: (Num k, Ord k) => Map k k -> k -> k
similarityScore histo ident = ident * findWithDefault 0 ident histo

part2 :: IO ()
part2 = do
    input <- getContents
    let rows = fmap (fmap read . words) (lines input)
    let cols = transpose rows
    (first, second) <- getTwoColsOrFail cols
    let histo = histogram second
    print $ sum (fmap (similarityScore histo) first)

-- MAIN (select part)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["1"] -> part1
        ["2"] -> part2
        _ -> putStrLn "Specify either 1 or 2 for part 1 or 2 respectively"
