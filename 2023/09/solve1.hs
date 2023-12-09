extrapolate (n:ns)
    | all (== 0) $ n:ns = 0
    | otherwise = dnext + last ns
    where diffs = zipWith (-) ns (n:ns)
          dnext = extrapolate diffs

main :: IO ()
main = do
    input <- fmap (map (map (read :: String -> Int) . words) . lines) getContents

    print $ sum $ map extrapolate input
