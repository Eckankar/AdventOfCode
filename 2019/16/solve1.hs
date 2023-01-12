fft key vs = map (abs . (`rem` 10) . sum . zipWith (*) vs) keys
    where keys = map (\n -> tail $ cycle $ concatMap (replicate n) key) [1 .. length vs]

main :: IO ()
main = do
    input <- fmap (map ((read :: String -> Int) . (:[]))) getLine
    let ffts = iterate (fft [0, 1, 0, -1]) input
    print $ foldl1 (\a d -> 10*a+d) $ take 8 $ ffts !! 100
