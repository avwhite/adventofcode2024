module Main where

import System.Environment (getArgs)
import Data.Bifunctor (bimap, Bifunctor (second))
import Data.List.Split ( splitOn )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Graph ( graphFromEdges, topSort )
import Data.Maybe (mapMaybe)

-- Assumes that all given inputs can be correctly sorted (if not, wrong output will be given)
-- If an input has more than 1 correct order, and arbitrary will be chosen

-- PART 1

getCenter :: [a] -> a
getCenter s =  s !! (length s `div` 2)

parseRelationInput :: [String] -> Map.Map Int [Int]
parseRelationInput relationIn = Map.fromListWith (++) ((second (:[]) <$> pairs) ++ allVerts) where
    allVerts = pairs >>= (\(a, b) -> [(a, []), (b, [])])
    pairs = bimap read (read . tail) . break (== '|') <$> relationIn :: [(Int, Int)]

correctOrder :: Ord a => Map.Map a [a] -> [a] -> [a]
correctOrder rel l = [ e | (e, _, _) <- fmap nodeFromVert (topSort graph)] where
    (graph, nodeFromVert, _) = graphFromEdges [(vert, vert, edgs) | (vert, edgs) <- Map.toAscList rel, Set.member vert itemSet]
    itemSet = Set.fromList l

isCorrectOrder :: Ord a => Map.Map a [a] -> [a] -> Bool
isCorrectOrder rel l = l == correctOrder rel l

part1 :: (Num a, Ord a) => Map.Map a [a] -> [[a]] -> a
part1 relation listsToCheck = sum (fmap getCenter okLists) where
    okEntries = isCorrectOrder relation <$> listsToCheck
    okLists = fst <$> filter snd (zip listsToCheck okEntries)

-- PART 2

correctIfWrong ::Ord a => Map.Map a [a] -> [a] -> Maybe [a]
correctIfWrong rel l = if l == correct then Nothing else Just correct where
    correct = correctOrder rel l

part2 :: (Num a, Ord a) => Map.Map a [a] -> [[a]] -> a
part2 relation listsToCheck = sum (fmap getCenter corrected) where
    corrected = mapMaybe (correctIfWrong relation) listsToCheck

-- MAIN (read input and select part)

skip1OrFail :: [a] -> IO [a]
skip1OrFail (_:as) = return as
skip1OrFail _ = fail ""

main :: IO ()
main = do
    input <- fmap lines getContents
    let (relationIn, rest) = break (== "") input -- Split into the two sections 
    listsIn <- skip1OrFail rest -- Skip the empty line

    let relation = parseRelationInput relationIn
    let listsToCheck = fmap read . splitOn "," <$> listsIn

    args <- getArgs
    case args of
        ["1"] -> print (part1 relation listsToCheck)
        ["2"] -> print (part2 relation listsToCheck)
        _ -> putStrLn "Specify either 1 or 2 for part 1 or 2 respectively"