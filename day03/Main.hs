{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main where
import System.Environment (getArgs)
import Text.Parsec
    ( anyChar, digit, string, count, option, (<|>), many, parse, try )
import Data.Maybe (catMaybes)
import Text.Parsec.String (Parser)

-- Probably more reliant on backtracking in the parsers than strictly neccesary,
-- but works fine

-- PART 1

atMostNDigitNum :: Int -> Parser Int
atMostNDigitNum n = do
    first <- Just <$> digit -- Must read at least 1 digit
    rest  <- count (n-1) (option Nothing (Just <$> digit)) -- read remaining optional ones
    return (read (catMaybes (first:rest)))

mulInstr :: Parser Int
mulInstr = do
    string "mul("
    first <- atMostNDigitNum 3
    string ","
    second <- atMostNDigitNum 3
    string ")"
    return (first * second)

part1Parser :: Parser Int
part1Parser = fmap sum (many (try mulInstr <|> (anyChar >> pure 0)))

part1 :: IO ()
part1 = do
    input <- getContents
    case parse part1Parser "" input of
        (Left _) -> putStrLn "parser error"
        (Right res) -> print res

-- PART 2

data Instr = Do | Dont | Number Int 
    deriving (Show)

parseInstr :: Parser Instr
parseInstr = try (Number <$> mulInstr)
    <|> try (string "do()" >> pure Do)
    <|> try (string "don't()" >> pure Dont)

part2Parser :: Parser [Instr]
part2Parser = fmap catMaybes (many ((Just <$> parseInstr) <|> (anyChar >> pure Nothing)))

eval :: Bool -> [Instr] -> Int
eval True  ((Number x):xs) = x + eval True xs -- enabled
eval False ((Number _):xs) = eval False xs    -- disabled
eval _     (Do:xs)         = eval True xs
eval _     (Dont:xs)       = eval False xs
eval _     []              = 0

part2 :: IO ()
part2 = do
    input <- getContents
    case parse part2Parser "" input of
        (Left _) -> putStrLn "parser error"
        (Right res) -> print (eval True res)

-- MAIN (select part)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["1"] -> part1
        ["2"] -> part2
        _ -> putStrLn "Specify either 1 or 2 for part 1 or 2 respectively"