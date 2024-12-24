{-# LANGUAGE TemplateHaskell #-}
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Bits ((.&.), (.|.), xor)
import Data.Char (isAlphaNum, isDigit)
import Data.List (partition, permutations, sortOn)
import Data.Maybe (fromMaybe, fromJust, isNothing)
import System.Random (initStdGen, randoms, split, StdGen())
import Text.ParserCombinators.ReadP

import Debug.Trace (traceShow)

data Value = Const Int | GAnd String String | GOr String String | GXor String String
    deriving (Eq, Ord, Show)

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- munch1 isDigit
    return $ read $ sign : digits

parseVarName :: ReadP String
parseVarName = many1 $ satisfy isAlphaNum

parseConst :: ReadP (M.Map String Value)
parseConst = do
    name <- parseVarName 
    string ": "
    value <- parseInt
    return $ M.singleton name $ Const value

parseConsts :: ReadP (M.Map String Value)
parseConsts = fmap M.unions $ sepBy parseConst (char '\n')

parseGateOp :: ReadP (String -> String -> Value)
parseGateOp =
    (string "AND" >> return GAnd) <|>
    (string "OR"  >> return GOr ) <|>
    (string "XOR" >> return GXor)

parseGate :: ReadP (M.Map String Value)
parseGate = do
    v1 <- parseVarName
    char ' '
    op <- parseGateOp
    char ' '
    v2 <- parseVarName
    string " -> "
    target <- parseVarName
    return $ M.singleton target $ op v1 v2

parseGates :: ReadP (M.Map String Value)
parseGates = fmap M.unions $ sepBy parseGate (char '\n')

parseInput :: ReadP (M.Map String Value)
parseInput = do
    consts <- parseConsts
    string "\n\n"
    gates <- parseGates
    skipSpaces
    eof
    return $ M.union consts gates

runParser p = fst . head . filter (null . snd) . readP_to_S p

evaluateGates :: M.Map String Value -> Maybe (M.Map String Int)
evaluateGates m = if M.null $ M.filter isNothing m' then Just $ M.map fromJust m' else Nothing
    where m' = M.map (evaluate S.empty) m
          evaluate seen expr
            | S.member expr seen = Nothing
            | otherwise =
                let seen' = S.insert expr seen in
                case expr of
                    (Const n)    -> Just n
                    (GAnd v1 v2) -> do
                        v1' <- evaluate seen' (m M.! v1)
                        v2' <- evaluate seen' (m M.! v2)
                        return $ v1' .&. v2'
                    (GOr v1 v2) -> do
                        v1' <- evaluate seen' (m M.! v1)
                        v2' <- evaluate seen' (m M.! v2)
                        return $ v1' .|. v2'
                    (GXor v1 v2) -> do
                        v1' <- evaluate seen' (m M.! v1)
                        v2' <- evaluate seen' (m M.! v2)
                        return $ v1' `xor` v2'

extractVar :: Char -> M.Map String Int -> [Int]
extractVar c m = map snd $ reverse $ sortOn fst $ M.toList $ M.filterWithKey (\k _ -> head k == c) m

fromBinary :: [Int] -> Int
fromBinary = foldl (\a b -> a*2+b) 0

toBinary :: Int -> [Int]
toBinary n = remLeading0 $ reverse $ toBinary' n
    where toBinary' n | n <= 1 = [n]
                      | otherwise = (n `mod` 2) : toBinary' (n `div` 2)
          remLeading0 [0] = [0]
          remLeading0 (0:r) = r
          remLeading0 r = r

findFaults :: M.Map String Value -> Maybe [String]
findFaults m = do
    m' <- evaluateGates m
    let x = extractVar 'x' m'
    let y = extractVar 'y' m'
    let z = extractVar 'z' m'
    let z' = toBinary $ fromBinary x + fromBinary y
    let incorrect = map (\(i, _, _) -> toRegister 'z' i) $ filter (\(_, a, b) -> a /= b) $ zip3 [0..] (reverse z) (reverse z')
    let recIncorrect = S.toList $ S.fromList $ concatMap (recFindFaults m') incorrect

    return recIncorrect
    where toRegister c i | i < 10 = c:'0':show i
                         | otherwise = c:show i
          recFindFaults m' r = r :
              case (m M.! r, m' M.! r) of
                  (GAnd v1 v2, 0) ->
                    (if m' M.! v1 == 0 then recFindFaults m' v1 else []) ++ (if m' M.! v2 == 0 then recFindFaults m' v2 else [])
                  (GAnd v1 v2, 1) ->
                    recFindFaults m' v1 ++ recFindFaults m' v2
                  (GOr v1 v2, 0) ->
                    recFindFaults m' v1 ++ recFindFaults m' v2
                  (GOr v1 v2, 1) ->
                    (if m' M.! v1 == 1 then recFindFaults m' v1 else []) ++ (if m' M.! v2 == 0 then recFindFaults m' v2 else [])
                  (GXor v1 v2, _) ->
                    recFindFaults m' v1 ++ recFindFaults m' v2
                  (Const _, _) -> []
                    
choose 0 ls = [[]]
choose _ [] = []
choose n (l:ls) = (map (l:) $ choose (n-1) ls) ++ choose n ls

randomizeInputs :: (StdGen, M.Map String Value) -> (StdGen, M.Map String Value)
randomizeInputs (rng, m) = (rng', m')
    where (vars, vals) = unzip $ M.toList $ M.filterWithKey (\k _ -> isInputVar k) m
          (rng', rng'') = split rng
          m' = M.union (M.fromList $ zip vars $ map (Const . (`mod` 2)) $ randoms rng'') m

          isInputVar (c:_) = c == 'x' || c == 'y'

increaseBy m Nothing  = Just m
increaseBy m (Just n) = Just $ n+m

swapGates :: M.Map String Value -> [(String, String)] -> M.Map String Value
swapGates m ((s, t):ss) = swapGates m' ss
    where m' = M.insert s (m M.! t) $ M.insert t (m M.! s) m
swapGates m [] = m

testSwaps :: M.Map String Value -> [String] -> [String] -> Bool
testSwaps m as bs = any id $ map testSwap $ permutations bs
    where testSwap bs | traceShow (as, bs) False = undefined
          testSwap bs = null $ fromMaybe ["dummy"] $ findFaults $ swapGates m $ zip as bs 

main :: IO ()
main = do
    input <- fmap (runParser parseInput) getContents
    let Just input' = evaluateGates input

    rng <- initStdGen
    let randomizedInputs = map snd $ iterate randomizeInputs (rng, input)

    let mostFaulty = foldr (M.alter (increaseBy 1)) M.empty $ concat $ map (fromJust . findFaults) $ take 500 randomizedInputs
    --print $ reverse $ sortOn snd $ M.toList mostFaulty

    let top8 = take 8 $ map fst $ sortOn snd $ M.toList mostFaulty
    print $ filter snd $ map ((\(p1, p2) -> ((p1, p2), testSwaps input p1 p2)) . flip partition top8 . flip S.member . S.fromList) $ choose 4 top8

    --let (possWrong0, possWrong1) = partition ((== 0) . (input' M.!) . fst) $ reverse $ sortOn snd $ M.toList mostFaulty


    --print $ testSwaps input (map fst $ take 4 possWrong0) (map fst $ take 4 possWrong1)
