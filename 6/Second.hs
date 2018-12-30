import qualified Data.List as L
import Data.Function (on)
import Data.Maybe

data Point = Point {
  index :: Int,
  xLoc :: Int,
  yLoc :: Int
} deriving Show

input = "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9"

maxScore :: Int
maxScore = 10000

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
  let range = 1000
      xs = xLoc <$> points
      ys = yLoc <$> points
  in ((minimum xs - range) `min` 0, minimum xs + range, (minimum ys - range) `min` 0, minimum ys + range)

allLocations :: (Int, Int, Int, Int) -> [(Int, Int)]
allLocations (l, r, t, b) = [(x, y) | x <- [l..r], y <- [t..b]]

manhattanDistance :: Int -> Int -> Point -> Int
manhattanDistance x' y' (Point _ x y) = abs (x - x') + abs (y - y')

pointScore :: [Point] -> (Int, Int) -> Int
pointScore points (x,y) = 
  let go [] acc = acc
      go (p:ps) acc = if acc >= maxScore then acc else go ps (acc + manhattanDistance x y p)
  in go points 0

findPointsInRange :: [Point] -> [Int]
findPointsInRange points = 
  let locs = allLocations $ boundingBox points
      scores = pointScore points <$> locs
  in filter (< maxScore) scores

calc :: String -> Int
calc = length . findPointsInRange . readPoints

main :: IO ()
main = calc <$> readFile "input.txt" >>= print