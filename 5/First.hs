import Data.Char
import Data.Maybe

readInput :: String -> IO String
readInput filename = readFile filename

polymerReaction1 :: String -> String
polymerReaction1 s = 
    let hasReaction c1 c2 = abs (ord c1 - ord c2) == 32 && isAlpha c1 && isAlpha c2
        react [] s = s
        react [c] s = c:s
        react [c1,c2] s = if hasReaction c1 c2 then s else c2:c1:s
        react (c1:c2:c3:rest) s =
                if hasReaction c1 c2
                then react (c3:rest) s 
                else react (c2:c3:rest) (c1:s)
    in reverse $ react s ""

polymerReaction :: String -> String
polymerReaction s =
    let s' = polymerReaction1 s
    in if s == s' then s' else polymerReaction s'

main :: IO ()
main = do
    input <- readInput "input.txt"
    print $ length (polymerReaction input)
    