import Data.List (find)
import Data.Maybe (isJust)

readInput :: String -> IO [String]
readInput filename = 
    lines <$> readFile filename

differsBy1Letter :: Eq a => [a] -> [a] -> Bool
differsBy1Letter s1 s2 =
    (length $ filter (uncurry (/=)) (s1 `zip` s2)) == 1

findDifferingBy1Letter :: [String] -> String -> Maybe String
findDifferingBy1Letter xs base = 
    find (differsBy1Letter base) xs

pairUp :: Eq a => [a] -> (a -> Maybe a) -> Maybe (a, a)
pairUp [] _ = Nothing
pairUp (x:xs) f = 
    let res = (\y -> (x, y)) <$> f x 
    in if isJust res then res else pairUp xs f

removeDifferingLetters :: String -> String -> String
removeDifferingLetters s1 s2 =
    fst <$> filter (uncurry (==)) (s1 `zip` s2)

main :: IO ()
main = do
    input <- readInput "input.txt"
    let differingPair = pairUp input (findDifferingBy1Letter input)
    let diff = (uncurry removeDifferingLetters) <$> differingPair
    print diff