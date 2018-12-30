import Data.Graph
import Data.Char (ord, chr)
import Data.Array ((!))
import Data.List

data StepOrdering = StepOrdering {
  first :: Char,
  second :: Char
} deriving Show

input :: String
input = "Step C must be finished before step A can begin.\nStep C must be finished before step F can begin.\nStep A must be finished before step B can begin.\nStep A must be finished before step D can begin.\nStep B must be finished before step E can begin.\nStep D must be finished before step E can begin.\nStep F must be finished before step E can begin."

readStepOrderings :: String -> [StepOrdering]
readStepOrderings str = 
  let mkStepOrdering s = StepOrdering (s !! 5) (s !! 36)
  in mkStepOrdering <$> lines str

bounds :: [StepOrdering] -> (Vertex, Vertex)
bounds sos = 
  let min3 x y z = x `min` (y `min` z)
      max3 x y z = x `max` (y `max` z)
      go [] acc = acc
      go (StepOrdering f s : sos) (lower, upper) = go sos (min3 lower (ord f) (ord s), max3 upper (ord f) (ord s)) 
  in go sos (1000, 0)

buildGraph :: [StepOrdering] -> Graph
buildGraph sos = 
  let edge (StepOrdering f s) = (ord f, ord s)
      edges = edge <$> sos
      b = bounds sos
  in buildG b edges

adjacentVertices :: Graph -> Vertex -> [Vertex]
adjacentVertices g v = sort $ g ! v

dfs' :: Graph -> [Vertex]
dfs' g =
  let start = head . topSort $ g
      go [] acc = acc
      go (v:rest) acc =
        let vs = adjacentVertices g v
            acc' = go vs (v:acc)
        in go rest acc'    
  in reverse . nub $ go [start] []

calc :: String -> String
calc input = 
  let g = buildGraph . readStepOrderings $ input
  in chr <$> dfs' g

main :: IO ()
main = calc <$> readFile "input.txt" >>= print