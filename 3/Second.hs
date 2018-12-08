import qualified Data.Map.Strict as M
import Data.List (foldl', groupBy, sort, find)
import Prelude hiding (id)
import Control.Arrow ((&&&))

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
    
type Fabric = M.Map (Int, Int) [Int]

newFabric :: Fabric
newFabric = M.empty

applyClaimYX :: Int -> Int -> Int -> Fabric -> Fabric
applyClaimYX claimId y x = M.insertWith (++) (x,y) [claimId]

applyClaimRow :: Int -> Int -> (Int -> Fabric -> Fabric) -> Fabric -> Fabric
applyClaimRow x width f =
    let go x' fabric = if x' >= x + width then fabric else go (x' + 1) (f x' fabric)
    in go x

applyClaim :: Claim -> Fabric -> Fabric
applyClaim claim =
    let go y' fabric = if y' >= (y claim) + (h claim) then fabric else go (y' + 1) (applyClaimRow (x claim) (w claim) (applyClaimYX (id claim) y') fabric)
    in go (y claim)

applyAllClaims :: [Claim] -> Fabric
applyAllClaims =
    foldl' (flip applyClaim) newFabric

findSquaresWithOnlyOneClaim :: Fabric -> [Int]
findSquaresWithOnlyOneClaim fabric =
    map (\(k,v) -> head v) . filter (\(k,v) -> length v == 1) . M.toList $ fabric

idCounts :: Ord a => [a] -> [(a, Int)]
idCounts ids = fmap (head &&& length) $ groupBy (==) (sort ids)

claimSize :: Claim -> Int
claimSize claim = (w claim) * (h claim)

findUntouchedClaim :: M.Map Int Int -> [(Int, Int)] -> Maybe Int
findUntouchedClaim cs idcs = 
    fst <$> find (\idc -> M.lookup (fst idc) cs == Just (snd idc)) idcs

main :: IO ()
main = do
    input <- readInput "input.txt"
    let claims = parseClaim <$> input 
    let fabric = applyAllClaims claims
    let sizes = M.fromList $ (id &&& claimSize) <$> claims
    let counts = idCounts . findSquaresWithOnlyOneClaim $ fabric
    print $ findUntouchedClaim sizes counts
