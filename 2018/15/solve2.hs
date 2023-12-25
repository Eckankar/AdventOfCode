import qualified Data.Map as M
import qualified Data.Set as S

import Data.List (sort)
import Data.List.Extra (minimumOn)
import Data.Maybe (mapMaybe)
import Data.Tuple.Extra (fst3, snd3)

type Point = (Int, Int)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [0..] r) . zip [0..]

neighbors (x, y) = [ (x-1, y), (x, y-1), (x, y+1), (x+1, y) ]

extractUnits :: M.Map Point Char -> (S.Set Point, M.Map Point (Char, Int, Int))
extractUnits m = (ms, units)
    where units = M.map (, 200, 3) $ M.filter (`elem` "GE") m
          ms = M.keysSet $ M.filter (/= '#') m 

takeStep ms um u utype
    | not $ S.null $ S.intersection (M.keysSet em) $ S.fromList $ neighbors u = u
    | otherwise = floodFill initActive $ S.singleton u
    where em = M.filter ((/= utype) . fst3) um
          emn = flip S.difference os $ S.unions $ map (S.fromList . neighbors) $ M.keys em
          os = M.keysSet $ M.filter ((== utype) . fst3) um
          initActive = flip M.withoutKeys os $
                       M.fromList $ map (\a -> (a, a)) $ filter (flip S.member ms) $ neighbors u
          floodFill active seen =
            if M.null active
            then u
            else if S.null activeEnemies
            then floodFill active' seen'
            else active M.! (minimum activeEnemies)
            where activeEnemies = S.intersection emn $ M.keysSet active
                  active' = flip M.withoutKeys os $ flip M.withoutKeys seen $ flip M.restrictKeys ms $
                            M.unionsWith min $ map (\(p, o) -> M.fromList $ map (, o) $ neighbors p) $
                            M.toList active
                  seen' = S.union seen $ M.keysSet active

attackEnemy u (utype, _, uattack) um us =
    if S.null nearbyEnemies
    then (um, us, False, False)
    else (um', us', enemyDied, elfDied) 
    where em = M.filter ((/= utype) . fst3) um
          nearbyEnemies = S.intersection (S.fromList $ neighbors u) $ M.keysSet em
          enemy = minimumOn (snd3 . (um M.!)) $ S.toList nearbyEnemies
          (etype, ehp, eatt) = em M.! enemy
          ehp' = ehp - uattack
          enemyDied = ehp' <= 0
          elfDied = enemyDied && etype == 'E'
          um' = if enemyDied then M.delete enemy um else M.insert enemy (etype, ehp', eatt) um
          us' = if enemyDied then filter (/= enemy) us else us
          

executeRounds n ms um [] = executeRounds (n+1) ms um $ sort $ M.keys um
executeRounds n ms um (u:us) =
    if elfDied
    then Nothing
    else if enemyDied && M.null liveEnemies
    then Just $ nFinal * (sum $ M.map snd3 um'')
    else executeRounds n ms um'' us'
    where uval@(utype, _, _) = um M.! u
          u' = takeStep ms um u utype
          um' = M.insert u' uval $ M.delete u um

          (um'', us', enemyDied, elfDied) = attackEnemy u' uval um' us
          liveEnemies = M.filter ((/= utype) . fst3) um''
          nFinal = n + if null us' then 1 else 0

startParams :: M.Map Point (Char, Int, Int) -> [M.Map Point (Char, Int, Int)]
startParams units = [ M.union others $ M.map (\(c, hp, a) -> (c, hp, a+i)) elves | i <- [1..] ]
    where (elves, others) = M.partition ((== 'E') . fst3) units

main :: IO ()
main = do
    input <- fmap (toMap . lines) getContents

    let (walkable, units) = extractUnits input

    print $ head $ mapMaybe (\us -> executeRounds (-1) walkable us []) $ startParams units
