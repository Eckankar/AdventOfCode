numDigits :: Int -> Int
numDigits n = ceiling $ logBase 10 $ fromIntegral $ n + 1

update :: Int -> [Int]
update 0 = [1]
update n | nl `mod` 2 == 0 = [n1, n2]
         | otherwise = [ n * 2024 ]
                where nl = numDigits n
                      mask = 10 ^ (nl `div` 2)
                      (n1, n2) = n `divMod` mask

main :: IO ()
main = do
    nums <- fmap (map (read :: String -> Int) . words) getLine
    print $ length $ (!! 25) $ iterate (concatMap update) nums
