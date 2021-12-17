import qualified Data.Set as S

import Data.List.Split (splitOn)

rSum n m | n == m    = n
         | otherwise = n + rSum (n+1) m

xCandidates (xs, xe) =
    [ (n, m) | m <- [xStart .. xe],
               n <- [0 .. m],
               let s = rSum n m,
               s >= xs && s <= xe ]
        where xStart = floor $ sqrt $ fromIntegral $ 2 * xs

yCandidates xs (ys, ye) =
    [ (y, y * (y+1) `div` 2) | y <- [0 .. -ys],
                               s <- steps (-y-1) 0 0,
                               let steps = s + 2*y,
                               validSteps s ]
    where steps n k i | k < ys             = []
                      | k >= ys && k <= ye = [i]
                      | k > ye             = steps (n-1) (n+k) (i+1)
          validSteps s = any (validatesStep s) xs
          validatesStep s (0, k) | k <= s = True
          validatesStep s (n, m)          = m-n+1 == s

parseInput :: String -> ((Int, Int), (Int, Int))
parseInput s = ((xs, xe), (ys, ye))
    where [_, _, 'x':'=':xrng', 'y':'=':yrng] = words s
          xrng = init xrng'
          [xs, xe] = parseRange xrng
          [ys, ye] = parseRange yrng
          parseRange s = map read $ splitOn ".." s

main :: IO ()
main = do
    (xs, ys) <- fmap parseInput getLine
    let xc = xCandidates xs
    print $ maximum $ map snd $ yCandidates xc ys
