import Data.Function.Memoize (memoize)

numDigits :: Int -> Int
numDigits n = ceiling $ logBase 10 $ fromIntegral $ n + 1

memoCounts :: Int -> [Int]
memoCounts = memoize counts
    where counts 0 = 1 : memoCounts 1
          counts n | nl `mod` 2 == 0 = 2 : zipWith (+) (memoCounts n1) (memoCounts n2)
                   | otherwise       = 1 : memoCounts (2024 * n)
                where nl = numDigits n
                      mask = 10 ^ (nl `div` 2)
                      (n1, n2) = n `divMod` mask

main :: IO ()
main = do
    nums <- fmap (map (read :: String -> Int) . words) getLine

    print $ sum $ map ((!! 74) . memoCounts) nums
