

main :: IO ()
main = do
    input <- fmap (map (read :: String -> Int) . lines) getContents
    print $ length $ filter id $ zipWith (<) input (tail input)
