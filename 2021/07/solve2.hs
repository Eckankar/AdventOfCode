main :: IO ()
main = do
    nums <- fmap (map read . words . map deComma) getLine
    print $ score nums $ optimalPos nums (foldl1 min nums) (foldl1 max nums)

        where deComma ',' = ' '
              deComma c   = c

              optimalPos :: [Int] -> Int -> Int -> Int
              optimalPos ns n m | n   == m  = n
              optimalPos ns n m | n+1 == m  = if score ns n < score ns m then n else m
              optimalPos ns n m | otherwise =
                if score ns avg > score ns (avg+1)
                then optimalPos ns (avg+1) m
                else optimalPos ns n avg
                    where avg = n + (m-n) `div` 2

              score :: [Int] -> Int -> Int
              score ns n = foldl (+) 0 $ map ((\x -> x * (x+1) `div` 2) . abs . ((-) n)) ns
