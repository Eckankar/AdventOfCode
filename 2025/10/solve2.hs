{-# LANGUAGE ScopedTypeVariables #-}
import qualified Data.Set as S
import qualified Data.Map as M

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import Text.ParserCombinators.ReadP

import qualified Data.SBV as V
import Data.SBV ((.>=), (.<=), (.==), (.&&))

import Debug.Trace (traceShow)

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- munch1 isDigit
    return $ read $ sign : digits

parseLights :: ReadP (S.Set Int)
parseLights = do
    rawLights <- between (char '[') (char ']') $ many1 parseLight
    return $ S.fromList $ map fst $ filter snd $ zip [0..] rawLights
    where parseLight = (char '#' >> return True) <|> (char '.' >> return False)

parseButton :: ReadP (S.Set Int)
parseButton = fmap S.fromList $ between (char '(') (char ')') $ sepBy1 parseInt (char ',')

parseJoltages :: ReadP [Int]
parseJoltages = between (char '{') (char '}') $ sepBy1 parseInt (char ',')

parseInput :: ReadP (S.Set Int, [S.Set Int], [Int])
parseInput = do
    lights <- parseLights
    char ' '
    buttons <- sepBy1 parseButton (char ' ')
    char ' '
    joltages <- parseJoltages

    return (lights, buttons, joltages)

runParser p = fst . head . filter (null . snd) . readP_to_S p




pressButton :: Int -> [Int] -> S.Set Int -> [Int]
pressButton n js button = zipWith (-) js $ buttonToJoltageDiff button
    where buttonToJoltageDiff button = map (\i -> if S.member i button then n else 0) [0..jl-1]
          jl = length js

relevantButtons :: [S.Set Int] -> M.Map Int [S.Set Int]
relevantButtons buttons = foldl logButton M.empty buttons
    where logButton :: M.Map Int [S.Set Int] -> S.Set Int -> M.Map Int [S.Set Int]
          logButton m button = foldl (flip $ M.alter (addButton button)) m $ S.toList button
          addButton :: S.Set Int -> Maybe [S.Set Int] -> Maybe [S.Set Int]
          addButton b Nothing   = Just [b]
          addButton b (Just bs) = Just $ b:bs

-- remove buttons that are the only ones relevant to a joltage
-- seems to be buggy; so omitted from final solution
clearFullyConstrained :: (S.Set Int, [S.Set Int], [Int], Int) -> (S.Set Int, [S.Set Int], [Int], Int)
clearFullyConstrained (targetLights, buttons, joltages, presses) =
    traceShow ("cfc", buttons, lockedButtons) $
    if not $ null lockedButtons
    then traceShow ("locked buttons", length buttons - length buttons', "remaining", length buttons') $ clearFullyConstrained (targetLights, buttons', joltages', presses')
    else (targetLights, buttons', joltages', presses')
    where buttons' = filter (not . (`S.member` lockedButtonS)) buttons
          presses' = (+ presses) $ sum $ M.elems lockedButtons
          lockedButtonS = M.keysSet lockedButtons
          joltages' = foldl (\js (btn, j) -> pressButton j js btn) joltages $ M.toList lockedButtons
          lockedButtons :: M.Map (S.Set Int) Int
          lockedButtons = M.fromList $ map (\(j, [btn]) -> (btn, j)) lockedJBs
          lockedJBs :: [(Int, [S.Set Int])]
          lockedJBs = filter ((== 1) . length . snd) $ map (\(j, (_, bs)) -> (j, bs)) joltageButtons
          joltageButtons :: [(Int, (Int, [S.Set Int]))]
          joltageButtons = zip joltages $ M.toAscList $ relevantButtons buttons


-- naïve simple iteration - not used
processMachine :: (S.Set Int, [S.Set Int], [Int], Int) -> Int
processMachine (targetLights, buttons, joltages, presses) =
    presses + process [(joltages, 0)] S.empty
    where initialLights = S.empty
          process ((js, d):qs) seen
            | all (== 0) js      = traceShow (buttons, joltages, d) d
            | any (< 0)  js      = process qs seen
            | js `S.member` seen = process qs seen
            | otherwise = process qs' seen'
                where qs' = qs ++ map ((\v -> (v, d+1)) . pressButton 1 js) buttons
                      seen' = S.insert js seen

constructSBVModel (buttons, joltages) = do
    -- buttons, must be pressed 
    bVars <- mapM V.sInt64 bVarNames
    mapM_ (\v -> V.constrain $ v .>= 0 .&& v .<= fromIntegral clickBound) bVars

    -- joltages
    jVars <- mapM V.sInt64 jVarNames
    mapM_ (\(v, jv) -> V.constrain $ v .== fromIntegral jv) $ zip jVars joltages

    let buttonToBvar = M.fromList $ zip buttons bVars
    let joltageButtons = zip3 joltages jVars (map snd $ M.toAscList $ relevantButtons buttons)
    mapM_ (constrainButtonToJoltage buttonToBvar) joltageButtons

    clicks <- V.sInt64 "clicks"
    V.constrain $ clicks .>= 0
    V.constrain $ clicks .<= fromIntegral clickBound
    V.constrain $ sum bVars .== clicks
    return clicks

    where clickBound = sum joltages
        
          iButtons = zip [0..] buttons
          bVarNames = map ((\i -> "button_" ++ show i) . fst) iButtons
          iJolts = zip [0..] joltages
          jVarNames = map ((\i -> "joltage_" ++ show i) . fst) iJolts
          
          constrainButtonToJoltage btb (targetJolt, joltVar, bs) = do
            V.constrain $ sum bvs .== joltVar
            V.constrain $ joltVar .== fromIntegral targetJolt
            where bvs = map (btb M.!) bs

processSBV :: (S.Set Int, [S.Set Int], [Int], Int) -> IO (Maybe Int)
processSBV (_, [], _, presses) = return $ Just presses
processSBV (targetLights, buttons, joltages, presses) | traceShow ("sbv", buttons) True = do
    let goal = V.minimize "number of button presses" =<< constructSBVModel (buttons, joltages)
    V.LexicographicResult res <- V.optimize V.Lexicographic goal 
    
    let (v :: Maybe V.Int64) = V.getModelValue "clicks" res
    return $ fmap ((+presses) . fromIntegral) v

extendWithPresses (targetLights, buttons, joltages) =  (targetLights, buttons, joltages, 0)

main :: IO ()
main = do
    machines <- fmap (map (runParser (parseInput <* eof)) . lines) getContents
    
    totals <- mapM (processSBV . extendWithPresses) machines
    print totals
    print $ sum $ map fromJust totals