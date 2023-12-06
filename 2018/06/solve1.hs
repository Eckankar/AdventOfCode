import qualified Data.Map as M
import qualified Data.Set as S

data FloodCell = Empty | Region Int (Int, Int) | Contested
    deriving (Eq, Show)

parsePoint :: String -> (Int, Int)
parsePoint s = read $ "(" ++ s ++ ")"

minMax xs = (minimum xs, maximum xs)

findClosedRegionSize :: [(Int, Int)] -> M.Map (Int, Int) Int
findClosedRegionSize ps = floodFill [ (p, p) | p <- ps ] M.empty S.empty 0
    where (minX, maxX) = minMax $ map fst ps
          (minY, maxY) = minMax $ map snd ps

          outOfBounds (x, y) = x < minX || x > maxX || y < minY || y > maxY

          neighbors (x, y) = [ (x+1, y), (x-1, y), (x, y+1), (x, y-1) ]

          floodFill :: [((Int, Int), (Int, Int))] -> M.Map (Int, Int) FloodCell -> S.Set (Int, Int) -> Int -> M.Map (Int, Int) Int
          floodFill qs m s n =
            if not $ null qs'
            then floodFill qs' m' s' (n+1)
            else count $ map deRegion $ M.elems $ M.filter innerRegion m'
            where (m', s') = floodFillStep qs m s

                  newPoints = M.toList $ M.filter regionFromThisTurn m'
                  qs' = [ (np, op) | (p, Region _ op) <- newPoints, np <- neighbors p ]

                  regionFromThisTurn (Region m _) = m == n
                  regionFromThisTurn _            = False

                  innerRegion (Region _ op) = not $ S.member op s'
                  innerRegion _             = False

                  deRegion (Region _ op) = op

                  count ls = foldr (M.alter (increaseBy 1)) M.empty ls

                  increaseBy m Nothing  = Just m
                  increaseBy m (Just n) = Just $ n+m

                  floodFillStep :: [((Int, Int), (Int, Int))] -> M.Map (Int, Int) FloodCell -> S.Set (Int, Int) -> (M.Map (Int, Int) FloodCell, S.Set (Int, Int))
                  floodFillStep [] m s = (m, s)
                  floodFillStep ((p, po):qs) m s
                    | outOfBounds p = floodFillStep qs m $ S.insert po s
                    | otherwise = case M.findWithDefault Empty p m of
                                    Empty                     -> floodFillStep qs mAddReg s
                                    Region n' po' | po == po' -> floodFillStep qs m s
                                                  | n' < n    -> floodFillStep qs m s
                                                  | n' == n   -> floodFillStep qs mAddContest s
                                                  | n' > n    -> error "Can't happen"
                                    Contested                 -> floodFillStep qs m s
                    where mAddReg     = M.insert p (Region n po) m
                          mAddContest = M.insert p Contested m

main :: IO ()
main = do
    input <- fmap (map parsePoint . lines) getContents

    let innerRegions = findClosedRegionSize input
    print $ maximum $ M.elems innerRegions
