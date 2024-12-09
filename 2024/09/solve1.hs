expandInput :: [Char] -> [Int]
expandInput s = expand s' 0
    where s' = map readDigit s

          readDigit '.' = -1
          readDigit n   = (read :: String -> Int) [n]

          expand [] _ = []
          expand [on] i = replicate on i
          expand (on:off:rs) i = replicate on i ++ replicate off (-1) ++ expand rs (i+1)

defrag :: [(Int, Int)] -> [(Int, Int)] -> [Int]
defrag ((n,i):ns) ((m,j):ms)
    | i > j = []
    | n /= -1   = n : defrag ns ((m,j):ms)
    | m == -1   = defrag ((n,i):ns) ms
    | otherwise = m : defrag ns ms
defrag _ _ = []

main :: IO ()
main = do
    input <- fmap expandInput getLine

    let numberedInput = zip input [0..]

    let defragged = defrag numberedInput $ reverse numberedInput

    print $ sum $ zipWith (*) [0..] defragged
