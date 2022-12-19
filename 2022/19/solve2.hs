import qualified Data.Map as M

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit)
import Data.Maybe (catMaybes)
import Text.ParserCombinators.ReadP

import Debug.Trace (traceShow, traceShowId)

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- many (satisfy isDigit)
    return $ read $ sign : digits

parseSpec = do
    string "Blueprint "
    id <- parseInt
    string ": Each ore robot costs "
    oreCostOre <- parseInt
    string " ore. Each clay robot costs "
    clayCostOre <- parseInt
    string " ore. Each obsidian robot costs "
    obsCostOre <- parseInt
    string " ore and "
    obsCostClay <- parseInt
    string " clay. Each geode robot costs "
    geoCostOre <- parseInt
    string " ore and "
    geoCostObs <- parseInt
    string " obsidian."
    return (id, (oreCostOre, clayCostOre, (obsCostOre, obsCostClay), (geoCostOre, geoCostObs)))

runParser p = fst . head . filter (null . snd) . readP_to_S p

simulate :: Int -> (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> (Int, (Int, Int, (Int, Int), (Int, Int))) -> (Int, Int)
simulate t robots res (id, costs) = traceShowId (id, simulate' t robots res)
    where (oreCostOre, clayCostOre, (obsCostOre, obsCostClay), (geoCostOre, geoCostObs)) = costs

          simulate' 0 _ (_, _, _, ge) = ge
          simulate' time robots@(orr, clr, obr, ger) res@(or, cl, ob, ge) =
            maximum $ catMaybes $ map (uncurry simOpt) options
            where options = [
                        ((0, 0, 0, 1), (-geoCostOre, 0, -geoCostObs, 0)),
                        ((0, 0, 1, 0), (-obsCostOre, -obsCostClay, 0, 0)),
                        ((0, 1, 0, 0), (-clayCostOre, 0, 0, 0)),
                        ((1, 0, 0, 0), (-oreCostOre, 0, 0, 0))
                      ]

                  safDiv 0 _ = 0
                  safDiv n m = n `div` m
                  add4 (a,b,c,d) (a',b',c',d') = (a+a', b+b', c+c', d+d')
                  div4 (a,b,c,d) (a',b',c',d') = (a `safDiv` a', b `safDiv` b', c `safDiv` c', d `safDiv` d')
                  times n (a,b,c,d) = (n * a, n * b, n * c, n * d)
                  negate (a,b,c,d) = (-a,-b,-c,-d)

                  maxOreCost = maximum [oreCostOre, clayCostOre, obsCostOre, geoCostOre]
                  listify (a,b,c,d) = [a,b,c,d]

                  simOpt drobots@(dorr, dclr, dobr, dger) dres@(dor, dcl, dob, dge)
                    -- Don't buy more robots if we're already providing as much as
                    -- we need for the most expensive robot per turn
                    | dorr == 1 && orr >= maxOreCost  = Nothing
                    | dclr == 1 && clr >= obsCostClay = Nothing
                    | dobr == 1 && obr >= geoCostObs  = Nothing
                    -- If we don't have any robots to provide the resource; we can't build anything requiring it next
                    | dcl /= 0 && clr == 0 || dob /= 0 && obr == 0 || dge /= 0 && ger == 0 = Nothing
                    | otherwise =
                        Just $ if waitTime >= time
                               then ge + ger*time
                               else simulate' (time-waitTime-1) robots' res''
                        where waitTime = maximum $ (0:) $ listify $ negate $ div4 res' robots
                              robots'  = add4 robots drobots
                              res'     = add4 res dres
                              res''    = add4 res' (times (waitTime+1) robots)

main :: IO ()
main = do
    input <- fmap (map (runParser (parseSpec <* eof)) . lines) getContents
    let maxes = map (simulate 32 (1,0,0,0) (0,0,0,0)) $ take 3 input
    print maxes
    print $ foldl (*) 1 $ map snd maxes
