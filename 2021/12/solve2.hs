import Data.Char (isUpper)
import Data.List (intercalate)
import Data.List.Split (wordsBy)
import qualified Data.Map as M
import qualified Data.Set as S

addToEdges e Nothing  = Just $ S.singleton e
addToEdges e (Just s) = Just $ S.insert e s

findAllPaths m "end" b seen path = [reverse path]
findAllPaths m v     b seen path     = concatMap explorePath ns
    where ns = S.filter canReallyVisit $ m M.! v
          canVisit v | all isUpper v = True
                     | otherwise     = not $ S.member v seen
          specialNodes = S.fromList ["start", "end"]
          canReallyVisit v = canVisit v || not (v `S.member` specialNodes || b)
          explorePath x = findAllPaths m x (b || not (canVisit x)) (S.insert x seen) (x : path)

main :: IO ()
main = do
    edges <- fmap (map (wordsBy (== '-')) . lines) getContents
    let m = foldr (\[k, v]  m -> M.alter (addToEdges k) v $ M.alter (addToEdges v) k m) M.empty edges
    print $ length $ findAllPaths m "start" False (S.singleton "start") ["start"]
