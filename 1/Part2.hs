import qualified Data.Set as S

partialSums :: Num a => [a] -> [a]
partialSums = tail . scanl (+) 0 . cycle

firstDup :: Ord a => [a] -> Maybe a
firstDup = go S.empty where
  go values [] = Nothing
  go values (x:xs) = 
    if S.member x values 
        then Just x 
        else go (S.insert x values) xs

answer :: (Ord a, Num a) => [a] -> Maybe a
answer = firstDup . partialSums

main :: IO ()
main = do
  input <- fmap read . words <$> readFile "input.txt"
  print $ answer input