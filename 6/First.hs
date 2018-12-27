import qualified Data.List as L
import Data.Function (on)
import Data.Maybe

data Point = Point {
  index :: Int,
  xLoc :: Int,
  yLoc :: Int
} deriving Show

-- translating input

mkPoint :: Int -> String -> Point
mkPoint idx s = 
  let (x, y) = (read :: String -> (Int, Int)) $ "(" ++ s ++ ")"
  in Point idx x y

readPoints :: String -> [Point]
readPoints text = uncurry mkPoint <$> [0..] `zip` lines text

-- calculation

-- left, right, top, bottom 
boundingBox :: [Point] -> (Int, Int, Int, Int)
boundingBox points = 
  let xs = xLoc <$> points
      ys = yLoc <$> points
  in (minimum xs, maximum xs, minimum ys, maximum ys)

allLocations :: (Int, Int, Int, Int) -> [(Int, Int)]
allLocations (l, r, t, b) = [(x, y) | x <- [l..r], y <- [t..b]]

manhattanDistance :: Int -> Int -> Point -> Int
manhattanDistance x' y' (Point _ x y) = abs (x - x') + abs (y - y')

nearestPoint :: Int -> Int -> [Point] -> Maybe Point
nearestPoint x y points =
  let withDistance = L.sortBy (compare `on` fst) $ (manhattanDistance x y <$> points) `zip` points
  in case withDistance of
    [] -> Nothing
    [(_, p)] -> Just p
    ((d1, p) : (d2, _) : _) -> if d1 == d2 then Nothing else Just p

isOnPerimeter :: (Int, Int, Int, Int) -> Point -> Bool
isOnPerimeter (l, r, t, b) p = 
  let x = xLoc p
      y = yLoc p
  in x == l || x == r || y == t || y == b

calcInternalIndices :: [Point] -> [Int]
calcInternalIndices points =
  let bounds = boundingBox points
      locs = allLocations bounds
      np (x, y) = 
        let maybeIndex = index <$> nearestPoint x y points
        in (\idx -> Point idx x y) <$> maybeIndex
      nearestPoints = catMaybes $ np <$> locs
      indicesOnPerimeter = index <$> filter (isOnPerimeter bounds) nearestPoints
      indices = index <$> nearestPoints
      internalIndices = filter (`L.notElem` indicesOnPerimeter) indices
  in L.sort internalIndices

mostFrequentElementCount :: Eq a => [a] -> Int
mostFrequentElementCount = length . L.maximumBy (compare `on` length) . L.group

calc :: String -> Int
calc = mostFrequentElementCount . calcInternalIndices . readPoints

main :: IO ()
main = calc <$> readFile "input.txt" >>= print