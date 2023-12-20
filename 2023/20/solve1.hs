import qualified Data.Map as M
import qualified Data.Set as S
import qualified Deque.Lazy as D

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit, isLetter)
import Text.ParserCombinators.ReadP

data Module = Module String ModType [String]
    deriving (Eq, Show)

data ModType = Broadcast | FlipFlop | Conjunction
    deriving (Eq, Show)

data SignalType = Low | High
    deriving (Eq, Show, Ord)

parseModule :: ReadP Module
parseModule = do
    modType <- (return Broadcast) <|> (char '%' >> return FlipFlop) <|> (char '&' >> return Conjunction)
    name <- munch1 isLetter
    string " -> "
    destinations <- sepBy (munch1 isLetter) (string ", ")

    return $ Module name modType destinations

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

getName (Module name _ _) = name
isType t (Module _ t' _) = t == t'

increaseBy m Nothing  = Just m
increaseBy m (Just n) = Just $ n+m

initConMap input = foldl insertPair M.empty $ concatMap pairUp input
    where conjs = S.fromList $ map getName $ filter (isType Conjunction) input
          pairUp (Module n _ ds) = flip zip (repeat n) $ filter (`S.member` conjs) ds
          insertPair :: M.Map String (M.Map String SignalType) -> (String, String) -> M.Map String (M.Map String SignalType)
          insertPair m (a, b) = M.alter insertPair' a m
            where insertPair' Nothing  = Just $ M.singleton b Low
                  insertPair' (Just m) = Just $ M.insert b Low m

triggerSignal conMap ffMap (Just (Module name Broadcast ds)) from sigType = (conMap, ffMap, map (, sigType) ds)
triggerSignal conMap ffMap (Just (Module name FlipFlop ds)) from High = (conMap, ffMap, [])
triggerSignal conMap ffMap (Just (Module name FlipFlop ds)) from Low = (conMap, ffMap', map (, outSignal) ds)
    where state = M.findWithDefault False name ffMap
          ffMap' = M.insert name (not state) ffMap
          outSignal = if state then Low else High
triggerSignal conMap ffMap (Just (Module name Conjunction ds)) from sigType = (conMap', ffMap, map (, outSignal) ds)
    where conMap' = M.insert name myMap conMap
          myMap = M.insert from sigType $ conMap M.! name
          outSignal = if all (== High) $ M.elems myMap then Low else High
triggerSignal conMap ffMap Nothing from sigType = (conMap, ffMap, [])
          
processSignals pathMap q (conMap, ffMap, counts)
    | D.null q = (conMap, ffMap, counts)
    | otherwise = processSignals pathMap q' (conMap', ffMap', counts')
    where (Just (from, to, et), q'') = (D.head q, D.tail q)
          (conMap', ffMap', qes) = triggerSignal conMap ffMap (M.lookup to pathMap) from et
          counts' = M.alter (increaseBy 1) et counts
          q' = foldl (flip D.snoc) q'' $ map (\(to', et') -> (to, to', et')) qes

main :: IO ()
main = do
    input <- fmap (map (runParser (parseModule <* eof)) . lines) getContents

    let pathMap = M.fromList $ map (\m@(Module name t dests) -> (name, m)) input
    let conMap = initConMap input
    let ffMap = M.empty :: M.Map String Bool
    let initCounts = M.fromList [(Low, 0), (High, 0)]
    let initQ = D.fromConsAndSnocLists [("button", "broadcaster", Low)] []

    let (conMap', ffMap', counts') = (!! 1000) $ iterate (processSignals pathMap initQ) (conMap, ffMap, initCounts)

    print $ product $ M.elems counts'
