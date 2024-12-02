module Main where
import System.Environment (getArgs)

-- PART 1

forwardDerivative :: [Int] -> [Int]
forwardDerivative (x:xs) = fmap (uncurry (-)) (zip xs (x:xs))
forwardDerivative [] = []

firstCondition :: [Int] -> Bool
firstCondition l = all (> 0) l || all (< 0) l

secondCondition :: [Int] -> Bool
secondCondition = all ((<= 3) . abs) -- at least 1 is included in first condition

isSafe :: [Int] -> Bool
isSafe l = firstCondition dl && secondCondition dl where
    dl = forwardDerivative l

part1 :: IO ()
part1 = do
    input <- getContents
    let rows = fmap (fmap read . words) (lines input) :: [[Int]]
    let safeRows = fmap isSafe rows
    print (length (filter id safeRows))

-- PART 2

currentPrevAndNext :: [Int] -> [(Maybe Int, Int, Maybe Int)]
currentPrevAndNext l = zip3 (Nothing:ml) l (tail ml ++ [Nothing]) where
    ml = Just <$> l

isOkay :: (Ord a, Num a) => a -> Bool
isOkay x = x > 0 && x <= 3

data Status = Good | Fixable (Bool, Bool) deriving (Show)

check :: (Maybe Int, Int, Maybe Int) -> Status
check (_,         current, _         ) | isOkay current = Good
check (Just left, current, Just right) = Fixable (isOkay (left + current), isOkay (current + right))
check (Nothing,   current, Just right) = Fixable (True,                    isOkay (current + right))
check (Just left, current, Nothing)    = Fixable (isOkay (left + current), True)
check (Nothing,   _,       Nothing)    = Fixable (True,                    True)

fixesRequired :: [Status] -> Maybe Int -- Nothing means unfixable
fixesRequired ((Fixable (False, False)):_)                 = Nothing -- unfixable :(
fixesRequired (Good:xs)                                    = fixesRequired xs
fixesRequired ((Fixable (_, True)):(Fixable (True, _)):xs) = (1 +) <$> fixesRequired xs -- We can merge two fixes in this case
fixesRequired ((Fixable (_, True)):xs)                     = (1 +) <$> fixesRequired xs
fixesRequired ((Fixable (True, _)):xs)                     = (1 +) <$> fixesRequired xs
fixesRequired []                                           = Just 0

isSafe2 :: [Int] -> Bool
isSafe2 l = any (<= 1) $ fixesRequired (fmap check (currentPrevAndNext (forwardDerivative l)))

part2 :: IO ()
part2 = do
    input <- getContents
    let rows = fmap (fmap read . words) (lines input) :: [[Int]]
    let safeForwards = fmap isSafe2 rows
    let safeBackwards = fmap (isSafe2 . reverse) rows
    let safe = fmap (uncurry (||)) (zip safeForwards safeBackwards)
    print (length (filter id safe))

-- MAIN (select part)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["1"] -> part1
        ["2"] -> part2
        _ -> putStrLn "Specify either 1 or 2 for part 1 or 2 respectively"