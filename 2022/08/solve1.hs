import           Data.List (transpose)
import qualified Data.Set as S
import           Data.Tuple (swap)

findVisibleLine :: Int -> [Int] -> [(Int, Int)]
findVisibleLine x ls =
    snd (foldl findVisible init cls) ++ snd (foldr (flip findVisible) init cls)
    where init = (-1, [])
          cls = zip [0..] ls

          findVisible :: (Int, [(Int, Int)]) -> (Int, Int) -> (Int, [(Int, Int)])
          findVisible (best, cs) (y, e) | e > best  = (e, (x, y):cs)
                                        | otherwise = (best, cs)

findVisibleGrid :: [[Int]] -> [(Int, Int)]
findVisibleGrid ls =
    checkRows cls ++ map swap (checkRows tcls)
    where cls  = zip [0..] ls
          tcls = zip [0..] $ transpose ls
          checkRows = concatMap (uncurry findVisibleLine)


main :: IO ()
main = do
    input <- fmap (map (map (read . (: []))) . lines) getContents
    print $ length $ S.fromList $ findVisibleGrid input
