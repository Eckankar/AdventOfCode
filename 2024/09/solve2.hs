parseInput :: [Char] -> [(Int, Int)]
parseInput s = parse s' 0
    where s' = map readDigit s

          readDigit '.' = -1
          readDigit n   = (read :: String -> Int) [n]

          parse [] _ = []
          parse [on] i = [(i, on)]
          parse (on:off:rs) i = (i, on) : (-1, off) : parse rs (i+1)

expandInput :: [(Int, Int)] -> [Int]
expandInput = concatMap (uncurry $ flip replicate)

defrag :: [(Int, Int)] -> [(Int, Int)]
defrag ps = defrag' lastNum ps
    where lastNum = head $ filter (/= -1) $ map fst $ reverse ps

          defrag' :: Int -> [(Int, Int)] -> [(Int, Int)]
          defrag' (-1) !ms = ms
          defrag' n !ms    = defrag' (n-1) ms'
            where (preN, (_, nc) : postN) = span ((/= n) . fst) ms
                  (preBlock, postBlock) = span (\(t, tc) -> t /= -1 || tc < nc) preN

                  (-1, mc) : postBlock' = postBlock
                  ms' = consolidate $ if null postBlock then ms else preBlock ++ [(n, nc), (-1, mc-nc)] ++ postBlock' ++ [(-1, nc)] ++ postN

                  consolidate :: [(Int, Int)] -> [(Int, Int)]
                  consolidate bs = if bs /= bs' then consolidate bs' else bs
                    where bs' = consolidate' bs
                          consolidate' ((_, 0):bs) = consolidate' bs
                          consolidate' ((-1, a):(-1, b):bs) = consolidate' $ (-1, a+b):bs
                          consolidate' (b:bs) = b : consolidate' bs
                          consolidate' [] = []


main :: IO ()
main = do
    input <- fmap parseInput getLine

    let input' = expandInput $ defrag input

    print $ sum $ zipWith (*) [0..] $ map (\n -> if n == -1 then 0 else n) $ input'
