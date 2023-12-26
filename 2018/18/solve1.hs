import qualified Data.Map as M
import qualified Data.Set as S

import Data.Maybe (mapMaybe)

type Point = (Int, Int)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [0..] r) . zip [0..]

fromMap m = [ [ m M.! (y, x) | x <- [0..maxX] ] | y <- [0..maxY] ]
    where (maxY, maxX) = maximum $ M.keys m

neighbors (x, y) = [ (x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0) ]

increaseBy m Nothing  = Just m
increaseBy m (Just n) = Just $ n+m

count = foldr (M.alter (increaseBy 1)) M.empty

step m = M.mapWithKey step' m
    where step' p v =
            case v of
              '.' -> if M.findWithDefault 0 '|' nsCounts >= 3 then '|' else '.'
              '|' -> if M.findWithDefault 0 '#' nsCounts >= 3 then '#' else '|'
              '#' -> if M.findWithDefault 0 '#' nsCounts >= 1 && M.findWithDefault 0 '|' nsCounts >= 1 then '#' else '.'
            where nsCounts = count $ mapMaybe (flip M.lookup m) $ neighbors p

main :: IO ()
main = do
    input <- fmap (toMap . lines) getContents

    let final = (!! 10) $ iterate step input
    let finalCounts = count $ M.elems final

    mapM_ (putStrLn . unlines . (++ []) . fromMap) $ take 11 $ iterate step input 

    print $ (finalCounts M.! '#') * (finalCounts M.! '|')
