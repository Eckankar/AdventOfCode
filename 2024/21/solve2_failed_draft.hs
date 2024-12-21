import qualified Data.Map as M

import Data.Char (isDigit)

import Debug.Trace (traceShowId, traceShow)

type Point = (Int, Int)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((j, i), c)) $ zip [0..] r) . zip [0..]

add (x, y) (x', y') = (x+x', y+y')
neighbors = [ (0,1), (0,-1), (1,0), (-1,0) ]

toDir ( 0,  1) = 'v'
toDir ( 0, -1) = '^'
toDir ( 1,  0) = '>'
toDir (-1,  0) = '<'

bfs :: M.Map Point Char -> M.Map Point [String] -> M.Map Point [String] -> M.Map Point [String]
bfs m seen fs | M.null fs = M.map (map (reverse . ('A':))) seen
              | otherwise = bfs m seen' fs'
    where fs' = M.unionsWith (++) $ map (uncurry step) $ M.toList fs

          step p ss = 
                flip M.intersection m $ flip M.difference seen $ M.unionsWith (++) $
                [ M.singleton p' [ds:s]
                | s <- ss
                , d <- neighbors
                ,  let p' = add p d
                ,  let ds = toDir d
                ]

          seen' = M.union seen fs'

allPaths :: M.Map Point Char -> M.Map (Char, Char) [String]
allPaths m = M.unions $ map findPaths $ M.elems m
    where findPaths c = M.mapKeys (\p -> (c, m M.! p)) $ bfs m (M.singleton cPos [""]) (M.singleton cPos [""])
            where [cPos] = M.keys $ M.filter (== c) m

shortestWaysToEnter :: M.Map (Char, Char) [String] -> String -> [String]
shortestWaysToEnter m [_] = [""]
shortestWaysToEnter m (c1:c2:cs) =
    [ p ++ "A" ++ rp
    | p  <- m M.! (c1, c2)
    , rp <- shortestWaysToEnter m (c2:cs)
    ]

dirDist dirPaths s = sum $ map (dDists M.!) $ zip s $ tail s
    where dDists = M.fromList [ ((from, to), l)
                              | from <- "<>^vA"
                              , to   <- "<>^vA"
                              , let l = length $ head $ shortestWaysToEnter dirPaths [from, to]
                              ]

shortestOnly :: (Ord b, Eq b) => (a -> b) -> [a] -> [a]
shortestOnly lf ss = map snd $ filter ((== minL) . fst) ss'
    where ss' = map (\s -> (lf s, s)) ss
          minL = minimum $ map fst ss'

enterCode :: M.Map (Char, Char) [String] -> String -> (Int, Int) 
enterCode pms s = (length $ head ps, num) 
    where ps = shortestOnly length $ combine $ map (pms M.!) $ zip ('A':s) s 
          combine [ss] = ss
          combine (ss:ss':sss) = combine $ [ s++s' | s <- ss, s' <- ss' ] : sss

          num = (read :: String -> Int) $ takeWhile isDigit s 

filterToShortest :: (String -> Int) -> M.Map (Char, Char) [String] -> M.Map (Char, Char) [String]
filterToShortest dd m = M.map (shortestOnly dd) m

compose :: (String -> Int) -> M.Map (Char, Char) [String] -> M.Map (Char, Char) [String] -> M.Map (Char, Char) [String]
compose dd m1 m2 | traceShow (M.map (\ss -> (length ss, length $ head ss)) m2) False = undefined
compose dd m1 m2 = filterToShortest dd $ M.map combinedCode m2
    where combinedCode :: [String] -> [String]
          combinedCode ps = shortestOnly length $ concatMap (\s -> combinedCode' $ 'A':s) ps
        
          combinedCode' :: String -> [String]
          combinedCode' (c:c':cs) =
            take 1 $ shortestOnly dd $
            [ p ++ p'
            | p <- m1 M.! (c, c')
            , p' <- combinedCode' (c':cs)
            ]
          combinedCode' [c] = [""]

increaseBy m Nothing  = Just m
increaseBy m (Just n) = Just $ n+m

toFreqMap :: M.Map (Char, Char) [String] -> M.Map (Char, Char) [M.Map Char Int]
toFreqMap m = M.map (map strToFreq) m

strToFreq :: String -> M.Map Char Int
strToFreq s = foldr (M.alter (increaseBy 1)) M.empty s

--minimumsOn :: (Ord b, Eq b) => (a->b) -> [a] -> [a]
--minimumsOn f ls = map snd $ filter ((== minV) . fst) ls'
--    where ls' = map (\v -> (f v, v)) ls
--          minV = minimum $ map fst ls'


--composeFreq :: M.Map (Char, Char) [M.Map (Char, Char) Int]
--            -> M.Map (Char, Char) [M.Map (Char, Char) Int]
--            -> M.Map (Char, Char) [M.Map (Char, Char) Int]
--composeFreq m1 m2 = M.map combinedFreq m2
--    where combinedFreq :: [M.Map (Char, Char) Int] -> [M.Map (Char, Char) Int]
--          combinedFreq fs = minimumsOn (sum . M.elems) $ concatMap (combine) fs
--          combine :: M.Map (Char, Char) Int -> [M.Map (Char, Char) Int]
--          combine f = combine' $ M.toList f
--          combine' :: [((Char, Char), Int)] -> [M.Map (Char, Char) Int]
--          combine' [] = [M.empty]
--          combine' ((k,v):kvs) = 
--              [ M.unionWith (+) f' frs
--              | f' <- m1 M.! k
--              , frs <- combine' kvs
--              , let fv = M.map (*v) f'
--              ]

allCombinations :: [[a]] -> [[a]]
allCombinations [] = [[]]
allCombinations (ts:rs) = concatMap (\t -> map (t:) acr) ts
    where acr = allCombinations rs

applyFreq :: M.Map (Char, Char) [M.Map Char Int] -> [M.Map Char Int] -> [M.Map Char Int]
applyFreq new bases = concatMap applyFreq' bases
    where applyFreq' base = shortestOnly (sum . M.elems) bases'
            where bases'' :: [[M.Map Char Int]]
                  bases'' = map (uncurry procStep) $ M.toList base
                  bases' = map (M.unionsWith (+)) $ allCombinations bases''
          procStep :: Char -> Int -> [M.Map Char Int]
          procStep k v = [ M.map (*v) $ M.unionWith (+) pOut pIn | pOut <- new M.! ('A', k), pIn <- new M.! (k, 'A') ]

--bases = [fromList [('<',1),('>',1),('A',4),('^',3),('v',3)],fromList [('<',1),('>',1),('A',4),('^',3),('v',3)]]
--base = fromList [('<',1),('>',1),('A',4),('^',3),('v',3)]
--

main :: IO ()
main = do
    codes <- fmap lines getContents

    let codePad = M.filter (/= ' ') $ toMap ["789", "456", "123", " 0A"]
    let dirPad  = M.filter (/= ' ') $ toMap [" ^A", "<v>"]

    let codePaths = allPaths codePad
    let dirPaths = allPaths dirPad

    let codePaths' = filterToShortest (dirDist dirPaths) codePaths
    let dirPaths' = filterToShortest (dirDist dirPaths) dirPaths

    print dirPaths'
    
    --let codeFreqs = toFreqMap codePaths'
    let dirFreqs = toFreqMap $ dirPaths'

    --let compPath = composeFreq dirFreqs codeFreqs
    --print compPath
    --print $ codeFreqs M.! ('A', '0')
    --print $ dirFreqs M.! ('A', '<')
    --print $ map (\v -> (v,codeFreqs M.! v)) $ zip "A029A" "029A"
    --print $ map (sum . map (sum . M.elems) . (codeFreqs M.!)) $ zip "A029A" "029A"
    -- print $ sum $ map (sum . map (sum . M.elems) . (compPath M.!)) $ zip "A029A" "029A"
    let codeInputPaths = map (codePaths' M.!) $ zip "A029A" "029A"
    let codeInputFreqs = map (strToFreq . concat) $ allCombinations codeInputPaths
    
    let steps = iterate (applyFreq dirFreqs) codeInputFreqs
    print $ steps !! 1
    --print $ map (sum . map (sum . M.elems) . composeFreq dirFreqs) codeInputFreqs

    -- let dd = dirDist dirPaths'
    -- print ("dirPaths", dirPaths')
    -- print ("dirFreqs", dirFreqs)
    -- let !dir2 = composeFreq dirFreqs dirFreqs
    -- print ("dir2", dir2)
    -- let !dir4 = composeFreq dir2 dir2
    -- --print ("dir4", dir4)
    -- let !dir8 = composeFreq dir4 dir4
    -- --print ("dir8", dir8)
    -- let !dir16 = composeFreq dir8 dir8
    -- --print ("dir16", dir16)
    -- let !dir24 = composeFreq dir8 dir16
    -- --print ("dir24", dir24)
    -- let !dir25 = composeFreq dirFreqs dir25
    -- --print ("dir25", dir25)

    --let compPath = composeFreq dir25 codeFreqs

    --print compPath
    --print $ sum $ map (uncurry (*)) $ map (enterCode compPath) codes
