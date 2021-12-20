import qualified Data.Map as M
import qualified GHC.Arr as A

import GHC.Exts (groupWith)

neighbors (x, y) = [ (x + dx, y + dy) | dx <- [0..2], dy <- [0..2] ]

fromBinary = foldl (\a n -> a*2 + n) 0

activeRegion m = [ (x, y) | x <- [(minimum xs - 2) .. (maximum xs + 2)],
                            y <- [(minimum ys - 2) .. (maximum ys + 2)] ]
    where m' = M.filter (== 1) m
          (xs, ys) = unzip $ M.keys m'

enhance enhancement (m, d) = (M.fromList ps, d')
    where ps = map getArea $ activeRegion m
          getArea p = (p,  enhancement A.! fromBinary (map (flip (M.findWithDefault d) m) $ neighbors p))
          d' = enhancement A.! fromBinary (replicate 9 d)

binarify '#' = 1
binarify _   = 0

printMap (m, d) = putStrLn $ unlines $ map (concatMap (show . flip (M.findWithDefault d) m)) gs
    where gs = groupWith fst $ activeRegion m

main :: IO ()
main = do
    enhancement <- fmap (A.listArray (0, 511) . map binarify) getLine
    getLine
    m <- fmap lines getContents
    let (height, width) = (length m, length $ head m)
    let inputMap = M.fromList $ A.assocs $ A.listArray ((0, 0), (height-1, width-1)) $ map binarify $ concat m
    let enhancements = iterate (enhance enhancement) (inputMap, 0)
    print $ M.size $ M.filter (== 1) $ fst $ enhancements !! 2
