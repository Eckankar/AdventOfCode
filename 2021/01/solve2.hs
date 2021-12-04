

main :: IO ()
main = do
    input <- fmap (map (read :: String -> Int) . lines) getContents
    let windowed = zipWith3 (\a b c -> a+b+c) input (tail input) (tail $ tail input)
    print $ length $ filter id $ zipWith (<) windowed (tail windowed)
