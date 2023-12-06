countWays :: Int -> Int -> Int
countWays time dist = length [r | ht <- [0..time], let r = ht*(time-ht), r > dist]

main :: IO ()
main = do
    [ts, ds] <- fmap (map (map read . tail . words) . lines) getContents
    print $ foldl (*) 1 $ zipWith countWays ts ds
