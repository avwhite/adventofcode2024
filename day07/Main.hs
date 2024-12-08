module Main where

import System.Environment (getArgs)
import Text.Parsec.String ( Parser )
import Text.Parsec ( digit, string, sepBy, parse, sepEndBy )
import Text.Parsec.Combinator (many1)
import Text.Parsec.Char (newline)

canEvalTo :: [Int -> Int -> Int] -> Int -> [Int] -> Bool
canEvalTo _ _ [] = False -- Behavior for empty lists not specified
canEvalTo ops target (x:xs) = canEvalTo' x xs where
    canEvalTo' accum [] = target == accum
    canEvalTo' accum (y:ys)
        | accum > target = False
        | otherwise = any (\op -> canEvalTo' (accum `op` y) ys) ops

number :: Parser Int
number =  read <$> many1 digit

lineParser :: Parser (Int, [Int])
lineParser = do
    target <- number
    _ <- string ": "
    list <- sepBy number (string " ")
    return (target, list)

inputParser :: Parser [(Int, [Int])]
inputParser = sepEndBy lineParser newline

solve :: [Int -> Int -> Int] -> IO ()
solve ops  = do
    input <- getContents
    p <- case parse inputParser "" input of
        Left err -> print err >> fail "parse error"
        Right res -> return res
    let good = fst <$> filter (uncurry (canEvalTo ops)) p
    print (sum good)

part1 :: IO ()
part1 = solve [(*), (+)]

-- part 2

digits :: Int -> Int
digits x = floor (logBase 10 (fromIntegral x :: Double)) + 1

concatOp :: Int -> Int -> Int
concatOp a b = (a * (10 ^ digits b)) + b

part2 :: IO ()
part2 =  solve [(*), (+), concatOp]

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["1"] -> part1
        ["2"] -> part2
        _ -> putStrLn "Specify either 1 or 2 for part 1 or 2 respectively"