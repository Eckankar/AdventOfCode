import qualified Data.Map as M

import Data.Char (isDigit)
import Data.Function.Memoize (memoize3)

type Point = (Int, Int)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((j, i), c)) $ zip [0..] r) . zip [0..]

add (x, y) (x', y') = (x+x', y+y')
neighbors = [ (0,1), (0,-1), (1,0), (-1,0) ]

toDir ( 0,  1) = 'v'
toDir ( 0, -1) = '^'
toDir ( 1,  0) = '>'
toDir (-1,  0) = '<'

bfs :: M.Map Point Char -> M.Map Point [String] -> M.Map Point [String] -> M.Map Point [String]
bfs m seen fs | M.null fs = M.map (map (reverse . ('A':))) seen
              | otherwise = bfs m seen' fs'
    where fs' = M.unionsWith (++) $ map (uncurry step) $ M.toList fs

          step p ss = 
                flip M.intersection m $ flip M.difference seen $ M.unionsWith (++) $
                [ M.singleton p' [ds:s]
                | s <- ss
                , d <- neighbors
                ,  let p' = add p d
                ,  let ds = toDir d
                ]

          seen' = M.union seen fs'

allPaths :: M.Map Point Char -> M.Map (Char, Char) [String]
allPaths m = M.unions $ map findPaths $ M.elems m
    where findPaths c = M.mapKeys (\p -> (c, m M.! p)) $ bfs m (M.singleton cPos [""]) (M.singleton cPos [""])
            where [cPos] = M.keys $ M.filter (== c) m

computeBestScore :: M.Map (Char, Char) [String] -> M.Map (Char, Char) [String] -> String -> Int -> Int
computeBestScore cp dp s t = computeBestScore'' s 0 t 
    where computeBestScore'' = memoize3 computeBestScore'
          computeBestScore' ss i t | i > t = length ss - 1
          computeBestScore' [s] _ _        = 0
          computeBestScore' (s:s':ss) i t  = minimum ps + computeBestScore'' (s':ss) i t
              where ps = map (\ss' -> computeBestScore'' ('A':ss') (i+1) t) $ m M.! (s,s')
                    m = if i == 0 then cp else dp


findComplexity cp dp t s = (bs, num)
    where bs = computeBestScore cp dp ('A':s) t
          num = (read :: String -> Int) $ takeWhile isDigit s 


main :: IO ()
main = do
    codes <- fmap lines getContents

    let codePad = M.filter (/= ' ') $ toMap ["789", "456", "123", " 0A"]
    let dirPad  = M.filter (/= ' ') $ toMap [" ^A", "<v>"]

    let codePaths = allPaths codePad
    let dirPaths = allPaths dirPad

    print $ sum $ map (uncurry (*)) $ map (findComplexity codePaths dirPaths 25) codes
