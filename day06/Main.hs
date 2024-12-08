{-# LANGUAGE TupleSections #-}
module Main where

import System.Environment (getArgs)
import Data.Array.IArray
    ( Ix(inRange), Array, (!), array, IArray(..), (//) )
import Data.Maybe (catMaybes, isJust)
import Data.List (elemIndex, find)
import qualified Data.Set as Set
import Data.Array (assocs)
import Data.Bifunctor (first)

-- PART 1

tplus :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
tplus (a, b) (x, y) = (a + x, b + y)

turnRight :: Num a => (a, b) -> (b, a)
turnRight (a, b) = (b, -a)

array2D :: [[a]] -> Array (Int, Int) a
array2D ll = array abounds [((ri,ci), e) | (ri, row) <- zip [1..] ll, (ci, e) <- zip [1..] row] where
    rowCount = length ll
    colCount = length (head ll)
    abounds = ((1,1), (rowCount, colCount))

ray :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
ray origin direction = iterate (tplus direction) origin

lookUp :: (Ix i, IArray a1 a2) => a1 i a2 -> i -> Maybe a2
lookUp a i | inRange (bounds a) i = Just (a ! i)
lookUp _ _ = Nothing

lookupList :: (Ix i) => Array i e -> [i] -> [e]
lookupList a ixs = catMaybes $ takeWhile isJust [lookUp a i | i <- ixs]

guardPath' :: Array (Int, Int) Char -> (Int, Int) -> (Int, Int) -> [((Int, Int),(Int, Int))]
guardPath' room pos dir = let forward = ray pos dir in
    case elemIndex '#' (lookupList room forward) of
        Just index -> path ++ guardPath' room (fst (last path)) (turnRight dir) where
            path = take index (map (, dir) forward)
        Nothing -> takeWhile (inRange (bounds room) . fst) (map (, dir) forward)

guardPath :: Array (Int, Int) Char -> (Int, Int) -> (Int, Int) -> ([(Int, Int)], Bool)
guardPath room pos dir = first (fmap fst) $ detectLoop Set.empty (guardPath' room pos dir) where
    detectLoop encountered (x:xs)
        | x `Set.member` encountered = ([], True)
        | otherwise = first (x :) (detectLoop (Set.insert x encountered) xs)
    detectLoop _ [] = ([], False)

part1 :: IO ()
part1 = do
    input <- getContents
    let rows = lines input :: [[Char]]
    let a = array2D rows
    let startPos = fst <$> find (\(_,b) -> b == '^') (assocs a)
    let visited = case startPos of {
        Just sp -> Set.fromList (sp:fst (guardPath a sp (-1, 0)));
        Nothing -> Set.empty
    }
    print $ Set.size visited

-- PART 2

-- This is kind of slow.. wonder if there is a better way than checking each position on the path

assertJust :: Maybe a -> IO a
assertJust (Just a) = return a
assertJust Nothing = fail "fail"

part2 :: IO ()
part2 = do
    input <- getContents
    let rows = lines input :: [[Char]]
    let a = array2D rows -- // [((7,4), '#')]
    startPos <- assertJust (fst <$> find (\(_,b) -> b == '^') (assocs a))
    let (path,_) =  guardPath a startPos (-1, 0);
    let uniquePathPos = Set.delete startPos (Set.fromList path)
    let loopingPaths = filter snd $ fmap (\pos -> guardPath (a // [(pos, '#')]) startPos (-1, 0)) (Set.toList uniquePathPos)
    print (length  loopingPaths)

-- MAIN (select part)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["1"] -> part1
        ["2"] -> part2
        _ -> putStrLn "Specify either 1 or 2 for part 1 or 2 respectively"