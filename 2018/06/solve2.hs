import qualified Data.Map as M
import qualified Data.Set as S

parsePoint :: String -> (Int, Int)
parsePoint s = read $ "(" ++ s ++ ")"

findNearbyRegion :: [(Int, Int)] -> Int -> S.Set (Int, Int)
findNearbyRegion ps d = floodFill ps S.empty
    where neighbors (x, y) = [ (x+1, y), (x-1, y), (x, y+1), (x, y-1) ]

          dist (x, y) (x', y') = abs (x-x') + abs (y-y')
          withinRange p = (< d) $ sum $ map (dist p) ps

          floodFill :: [(Int, Int)] -> S.Set (Int, Int) -> S.Set (Int, Int)
          floodFill []     s = s
          floodFill (q:qs) s | S.member q s        = floodFill qs s
                             | not $ withinRange q = floodFill qs s
                             | otherwise  = floodFill qs' s'
                                where s'  = S.insert q s
                                      qs' = qs ++ filter (not . flip S.member s) (neighbors q)

main :: IO ()
main = do
    input <- fmap (map parsePoint . lines) getContents

    print $ S.size $ findNearbyRegion input 10000
