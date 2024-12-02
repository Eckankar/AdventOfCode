import Data.List (sort)

isSafe :: [Int] -> Bool
isSafe l = (l == sl || l == reverse sl) && hasBoundedDiff
    where sl = sort l
          hasBoundedDiff = and $ zipWith boundedDiff l $ tail l
          boundedDiff a b = n > 0 && n <= 3
            where n = abs (a-b)

main :: IO ()
main = do
    nums <- fmap (map (map (read :: String -> Int) . words) . lines) getContents

    print $ length $ filter isSafe nums
