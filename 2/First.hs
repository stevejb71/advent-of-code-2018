readInput :: String -> IO [String]
readInput filename = 
    lines <$> readFile filename

countRepeatsOf :: Char -> String -> Int
countRepeatsOf ch =
    length . filter ((==) ch)

alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyz"

repeatsAnyLetter :: Int -> String -> Bool
repeatsAnyLetter repeats s =
    any ((==) repeats) $ (flip countRepeatsOf s) <$> alphabet

repeatsCount :: Int -> [String] -> Int
repeatsCount repeats =
    length . filter (repeatsAnyLetter repeats)

checksum :: [String] -> Int
checksum ss =
    let count2 = repeatsCount 2 ss
        count3 = repeatsCount 3 ss
    in count2 * count3

main :: IO ()
main = readInput "input.txt" >>= print . checksum
