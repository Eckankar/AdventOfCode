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
    [ (x, y) | y <- [ys .. -ys],
               s <- steps y 0 0,
               (_, x) <- validSteps s ]
    where steps n k i | k < ys             = []
                      | k >= ys && k <= ye = i : steps (n-1) (n+k) (i+1)
                      | k > ye             = steps (n-1) (n+k) (i+1)
          validSteps s = filter (validatesStep s) xs
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
    print $ S.size $ S.fromList $ yCandidates xc ys
