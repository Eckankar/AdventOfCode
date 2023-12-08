import qualified Data.Map as M
import qualified Data.Set as S

import Algebra.PrincipalIdealDomain (chineseRemainderMulti)
import Data.Char (isAlphaNum)
import Data.List.Extra (groupSortOn)
import Text.ParserCombinators.ReadP

parseMapLine :: ReadP (String, (String, String))
parseMapLine = do
    node <- many1 $ satisfy isAlphaNum
    string " = ("
    left <- many1 $ satisfy isAlphaNum
    string ", "
    right <- many1 $ satisfy isAlphaNum
    char ')'
    eof
    return (node, (left, right))


runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

parseInput (directions:_:ms) = (directions, m)
    where m = M.fromList $ map (runParser parseMapLine) ms

followDirections :: [(Int, Char)] -> [String] -> [M.Map (String, Int) Int] -> M.Map (String, Int) Int -> S.Set String -> M.Map String (String, String) -> Int -> Int -> ([M.Map (String, Int) Int], M.Map (String, Int) Int)
followDirections ((dc, d):ds) cs firstSeen loopTime ts m s sEnd =
    if s == sEnd
    then (firstSeen', loopTime')
    else followDirections ds cs' firstSeen' loopTime' ts m (s+1) sEnd
        where (cfs', loopTime') = foldr followDirection ([], loopTime) $ zip cs firstSeen
              (cs', firstSeen') = unzip cfs'
              followDirection (c, firstSeen) (cs, loopTime) = ((c', firstSeen'):cs, loopTime')
                where (l, r) = m M.! c
                      c' = if d == 'L' then l else r

                      (firstSeen', loopTime') =
                        if not $ S.member c' ts
                        then (firstSeen, loopTime)
                        else if not $ M.member (c', dc) firstSeen
                        then (M.insert (c', dc) s firstSeen, loopTime)
                        else if not $ M.member (c', dc) loopTime
                        then (firstSeen, M.insert (c', dc) (s - (firstSeen M.! (c', dc))) loopTime)
                        else (firstSeen, loopTime)

findFirstOverlap :: [M.Map (String, Int) Int] -> M.Map (String, Int) Int -> Int
findFirstOverlap firstSeen loopTime = minimum $ map findFirstOverlap' $ S.toList overlapTimes
    where overlapTimes = S.map snd $ M.keysSet loopTime
          findFirstOverlap' t = minimum $ map calcOverlap $ cartesian $ map M.keys firstSeen'
            where firstSeen' = map (M.mapKeys fst . M.filterWithKey (\(_, t') _ -> t' == t)) firstSeen
                  loopTime'  = M.mapKeys fst $ M.filterWithKey (\(_, t') _ -> t' == t) loopTime

                  calcOverlap ps = m
                      where fss = zipWith (M.!) firstSeen' ps
                            lss = map (loopTime' M.!) ps
                            Just (m, _) = chineseRemainderMulti $ zip lss fss

          cartesian :: [[a]] -> [[a]]
          cartesian [] = [[]]
          cartesian (ls:lss) = do
            x <- ls
            xs <- cartesian lss
            return $ x:xs

main = do
    (directions, desertMap) <- fmap (parseInput . lines) getContents

    let startingNodes = filter ((== 'A') . last) $ M.keys desertMap
    let endingNodes   = S.filter ((== 'Z') . last) $ M.keysSet desertMap

    let startingNodeMaps = map (const M.empty) startingNodes

    let (firstSeen, loopTime) = followDirections (cycle $ zip [1..] directions) startingNodes startingNodeMaps M.empty endingNodes desertMap 0 (length directions * M.size desertMap)

    print $ findFirstOverlap firstSeen loopTime
