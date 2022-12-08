import           Data.List (transpose)
import qualified Data.Map as M
import qualified Data.Set as S

findVisibleLine :: Int -> [Int] -> [((Int, Int), Int)]
findVisibleLine x ls =
    fst (foldl findVisible init cls) ++ fst (foldr (flip findVisible) init cls)
    where init = ([], M.empty)
          cls = zip [0..] ls

          findVisible :: ([((Int, Int), Int)], M.Map Int Int) ->
                         (Int, Int) ->
                         ([((Int, Int), Int)], M.Map Int Int)
          findVisible (cs, m) (y, e) = (((x, y), dist) : cs, m')
            where m' = M.insert y e m
                  gs = map (abs . (\x -> x - y)) $ M.keys $ M.filter (>= e) m
                  dist = if null gs then M.size m else minimum gs

findVisibleGrid :: [[Int]] -> [((Int, Int), Int)]
findVisibleGrid ls =
    checkRows cls ++ map swapFirst (checkRows tcls)
    where cls  = zip [0..] ls
          tcls = zip [0..] $ transpose ls
          swapFirst ((a, b), c) = ((b, a), c)
          checkRows = concatMap (uncurry findVisibleLine)

productSides :: [((Int, Int), Int)] -> M.Map (Int, Int) Int
productSides = foldr timesSide M.empty
    where timesSide (slot, v) m = M.alter (multiply v) slot m
          multiply v Nothing  = Just v
          multiply v (Just n) = Just $ v * n

main :: IO ()
main = do
    input <- fmap (map (map (read . (: []))) . lines) getContents
    print $ maximum $ M.elems $ productSides $ findVisibleGrid input

