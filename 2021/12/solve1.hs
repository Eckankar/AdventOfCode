import Data.Char (isUpper)
import Data.List.Split (wordsBy)
import qualified Data.Map as M
import qualified Data.Set as S

addToEdges e Nothing  = Just $ S.singleton e
addToEdges e (Just s) = Just $ S.insert e s

findAllPaths m "end" seen path = [reverse path]
findAllPaths m v seen path     = concatMap explorePath ns
    where ns = S.filter canVisit $ m M.! v
          canVisit v | all isUpper v = True
                     | otherwise     = not $ S.member v seen
          explorePath x = findAllPaths m x (S.insert x seen) (x : path)

main :: IO ()
main = do
    edges <- fmap (map (wordsBy (== '-')) . lines) getContents
    let m = foldr (\[k, v]  m -> M.alter (addToEdges k) v $ M.alter (addToEdges v) k m) M.empty edges
    print $ length $ findAllPaths m "start" (S.singleton "start") ["start"]
