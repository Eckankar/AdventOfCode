digitize :: String -> [Int]
digitize s = map (read . (:[])) s

findLargestJoltage (n1:n2:ns) = snd $ foldl update (max n1 n2, n1*10+n2) ns
        where update (bestD, s) d = (max d bestD, max s s')
                where s' = bestD * 10 + d

main = do
    input <- fmap (map digitize . lines) getContents
    
    print $ sum $ map findLargestJoltage input
