import Data.List.Extra (maximumOn)

powerLevel serial x y = ((rid * (rid * y + serial)) `quot` 100 `rem` 10) - 5
    where rid = x+10

square serial x y = [ powerLevel serial (x+dx) (y+dy) | dy <- [0..2], dx <- [0..2] ]

main :: IO ()
main = do
    serial <- fmap (read :: String -> Int) getContents

    let (x, y) = maximumOn (sum . uncurry (square serial)) [ (x, y) | y <- [1..298], x <- [1..298] ]
    putStrLn $ show x ++ "," ++ show y
