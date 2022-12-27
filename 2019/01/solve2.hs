calculateFuel = sum . takeWhile (> 0) . tail . iterate fuel
    where fuel n = n `div` 3 - 2

main :: IO ()
main = fmap (sum . map (calculateFuel . read) . lines) getContents >>= print
