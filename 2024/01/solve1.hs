import Data.List (sort, transpose)

main :: IO ()
main = do
    lists <- fmap (transpose . map (map (read :: String -> Int) . words) . lines) getContents

    let slists = map sort lists

    print $ sum $ map (\[x, y] -> abs (x-y)) $ transpose slists
