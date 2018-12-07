import qualified Data.Map.Strict as M
import Data.List (foldl')

readInput :: String -> IO [String]
readInput filename = 
    lines <$> readFile filename

data Claim = Claim {
    id :: Int,
    x :: Int,
    y :: Int,
    w :: Int,
    h :: Int
} deriving (Show)

dropRight :: [a] -> [a]
dropRight = reverse . tail . reverse

readInt :: String -> Int
readInt = read

parsePairOfInts :: Char -> String -> (Int, Int)
parsePairOfInts sep s =
    let s' = map (\ch -> if ch == sep then ' ' else ch) s
        [n1, n2] = readInt <$> words s'
    in (n1, n2)

parseClaim :: String -> Claim
parseClaim s =
    let parts = words s
        claimId = readInt $ tail (head parts)
        (x, y) = parsePairOfInts ',' (dropRight (parts !! 2))
        (w, h) = parsePairOfInts 'x' (last parts)
    in Claim claimId x y w h
    
type Fabric = M.Map (Int, Int) Int

newFabric :: Fabric
newFabric = M.empty

applyClaimYX :: Int -> Int -> Fabric -> Fabric
applyClaimYX y x = M.insertWith (+) (x,y) 1

applyClaimRow :: Int -> Int -> (Int -> Fabric -> Fabric) -> Fabric -> Fabric
applyClaimRow x width f =
    let go x' fabric = if x' >= x + width then fabric else go (x' + 1) (f x' fabric)
    in go x

applyClaim :: Claim -> Fabric -> Fabric
applyClaim claim =
    let go y' fabric = if y' >= (y claim) + (h claim) then fabric else go (y' + 1) (applyClaimRow (x claim) (w claim) (applyClaimYX y') fabric)
    in go (y claim)

applyAllClaims :: [Claim] -> Fabric
applyAllClaims =
    foldl' (flip applyClaim) newFabric

main :: IO ()
main = do
    input <- readInput "input.txt"
    let claims = parseClaim <$> input 
    print . length . filter (\(k,v) -> v > 1) $ M.toList . applyAllClaims $ claims