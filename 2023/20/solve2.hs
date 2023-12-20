import qualified Data.Map as M
import qualified Data.Set as S
import qualified Deque.Lazy as D

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Control.DeepSeq (($!!))
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
          
processSignals targetGate pathMap q n (conMap, ffMap, firstHit)
    | D.null q = (conMap, ffMap, firstHit)
    | otherwise = processSignals targetGate pathMap q' n (conMap', ffMap', firstHit')
    where (Just (from, to, et), q'') = (D.head q, D.tail q)
          (conMap', ffMap', qes) = triggerSignal conMap ffMap (M.lookup to pathMap) from et
          q' = foldl (flip D.snoc) q'' $ map (\(to', et') -> (to, to', et')) qes
          firstHit' = if to == targetGate && et == High && not (M.member from firstHit)
                      then M.insert from n firstHit
                      else firstHit

iterUntilCount p n f x | p x       = x
                       | otherwise = iterUntilCount p (n+1) f $ f (n+1) $! x

main :: IO ()
main = do
    input <- fmap (map (runParser (parseModule <* eof)) . lines) getContents

    let pathMap = M.fromList $ map (\m@(Module name t dests) -> (name, m)) input
    let conMap = initConMap input
    let ffMap = M.empty :: M.Map String Bool
    let initQ = D.fromConsAndSnocLists [("button", "broadcaster", Low)] []

    -- it appears that rx is only hit by a single conjunction gate.
    -- maybe the inputs of this gate cycle offset from each other, so let's try doing an LCM
    let gatePairs = concatMap (\m@(Module name t dests) -> map (name,) dests) input
    let [(targetGate, _)] = filter ((== "rx") . snd) gatePairs
    let preReqs = S.fromList $ map fst $ filter ((== targetGate) . snd) gatePairs

    let (conMap', ffMap', firstHit) = iterUntilCount (\(_, _, firstHit) -> S.null $ S.difference preReqs $ M.keysSet firstHit) 0 (processSignals targetGate pathMap initQ) (conMap, ffMap, M.empty)
    print $ foldl1 lcm $ M.elems $ M.restrictKeys firstHit preReqs
