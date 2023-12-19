import qualified Data.Array as A

import Data.List.Extra (maximumOn, transpose)
import Data.Tuple.Extra (uncurry3)

powerLevel serial x y = rid * (rid * y + serial) `quot` 100 `rem` 10 - 5
    where rid = x+10

squareSum rectSumA x y size = v (x', y') - v (x', y-1) - v(x-1, y') + v(x-1, y-1)
    where x' = x + size - 1
          y' = y + size - 1
          v (x, y) | x < 1 || y < 1 = 0
                   | otherwise      = rectSumA A.! (x, y)

main :: IO ()
main = do
    serial <- fmap (read :: String -> Int) getContents

    let fuelCells = [ [ powerLevel serial x y | x <- [1..300] ] | y <- [1..300] ]

    -- index (i, j) has the sum of the rectangles with corners (1, 1) and (i, j)
    let rectSums = transpose $ map (scanl1 (+)) $ transpose $ map (scanl1 (+)) fuelCells
    let rectSumA = A.array ((1,1), (300,300)) [ ((x, y), v) | (y, r) <- zip [1..] rectSums, (x, v) <- zip [1..] r ]

    let (x, y, size) = maximumOn (uncurry3 (squareSum rectSumA)) [ (x, y, s) | s <- [1..300], y <- [1..(300-s)], x <- [1..(300-s)] ]
    putStrLn $ show x ++ "," ++ show y ++ "," ++ show size
