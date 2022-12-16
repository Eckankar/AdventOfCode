import qualified Data.Map as M
import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Control.Parallel.Strategies
import Data.Char (isDigit, isLetter)
import Data.List (permutations)
import Text.ParserCombinators.ReadP

import Debug.Trace (traceShow, traceShowId)

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- many (satisfy isDigit)
    return $ read $ sign : digits

parseValveName :: ReadP String
parseValveName = many1 $ satisfy isLetter

parseValve :: ReadP (String, Int, [String])
parseValve = do
    string "Valve "
    name <- parseValveName
    string " has flow rate="
    flow <- parseInt
    (string "; tunnels lead to valves " <|> string "; tunnel leads to valve ")
    neighbors <- sepBy parseValveName $ string ", "
    return (name, flow, neighbors)

runParser p = fst . head . filter (null . snd) . readP_to_S p

allSourceShortestPath :: M.Map String [String] -> M.Map (String, String) Int
allSourceShortestPath graph = foldl updateIfBetter init [(k, i, j) | k <- vs, i <- vs, j <- vs]
    where init = M.fromList $ concatMap (\(a, ns) -> ((a, a), 0) : map (\n -> ((a, n), 1)) ns) $ M.toList graph
          vs = M.keys graph
          updateIfBetter m (k, i, j) =
            case ((i,j) `M.lookup` m, (i,k) `M.lookup` m, (k,j) `M.lookup` m) of
                (Nothing, Just a, Just b) -> M.insert (i,j) (a+b) m
                (Just v,  Just a, Just b) -> if a+b < v then M.insert (i,j) (a+b) m else m
                _                         -> m

choose :: Int -> [a] -> [[a]]
choose 0 _      = [[]]
choose n []     = []
choose n (x:xs) = map (x:) (choose (n-1) xs) ++ choose n xs

totalFlow :: (String, Int) -> (String, Int) -> M.Map String Int -> M.Map (String, String) Int -> S.Set String -> Int
totalFlow (pos, n) (epos, en) flows sps valves = traceShow (length inits, "inits") $ maximum $ parMap rdeepseq flowPairs inits
    where inits = choose 2 $ S.toList valves
          flowPairs [pos', epos'] = doTrace $ (n-d-1)*fl + (en-de-1)*efl + totalFlow' (pos', n-d-1) (epos', en-de-1) vs'
              where vs' = S.delete pos' $ S.delete epos' valves
                    d   = dist pos pos'
                    de  = dist epos epos'
                    fl  = flows M.! pos'
                    efl = flows M.! epos'
                    doTrace v = traceShow (pos', epos', v) v
          totalFlow' (pos', n') (epos', en') vs
              | S.null vs = 0
              | n' < 0 || en' < 0 = 0
              | otherwise = S.findMax $ S.map procValve vs
                  where procValve v =
                          max (if d < n'  then   (n'-d-1)*fl + totalFlow' (v, n'-d-1) (epos', en')  vs' else 0)
                              (if d < en' then (en'-de-1)*fl + totalFlow' (pos', n')  (v, en'-de-1) vs' else 0)
                          where d  = dist pos' v
                                de = dist epos' v
                                fl = flows M.! v
                                vs' = S.delete v vs
          dist = curry (sps M.!)

main :: IO ()
main = do
    input <- fmap (map (runParser (parseValve <* eof)) . lines) getContents

    let valves = M.fromList $ map (\(a, fr, ns) -> (a, (fr, ns))) input
    let nonZeroValves = S.fromList $ map (\(a, _, _) -> a) $ filter (\(_, fr, _) -> fr /= 0) input
    let valveFlows = M.map fst $ M.filter (\(fr, _) -> fr /= 0) valves

    let shortestPaths = allSourceShortestPath $ M.map snd valves
    print $ totalFlow ("AA", 26) ("AA", 26) valveFlows shortestPaths nonZeroValves
