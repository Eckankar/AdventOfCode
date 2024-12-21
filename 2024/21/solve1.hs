import qualified Data.Map as M

import Data.Char (isDigit)

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

enterCode :: M.Map (Char, Char) [String] -> String -> (Int, Int) 
enterCode pms s = (minL, num) 
    where ps = map (\s -> (length s, s)) $ combine $ map (pms M.!) $ zip ('A':s) s 
          combine [ss] = ss
          combine (ss:ss':sss) = combine $ [ s++s' | s <- ss, s' <- ss' ] : sss
          minL = minimum $ map fst ps
          ps' = map snd $ filter ((== minL) . fst) ps

          num = (read :: String -> Int) $ takeWhile isDigit s 

filterToShortest :: (String -> Int) -> M.Map (Char, Char) [String] -> M.Map (Char, Char) [String]
filterToShortest dd m = M.map filterToShortest' m
    where filterToShortest' ps = map snd $ filter ((== minD) . fst) ps'
              where ps' = [ (dd p, p) | p <- ps ]
                    minD = minimum $ map fst ps'

compose dd m1 m2 = filterToShortest dd $ M.map combinedCode m2
    where combinedCode ps = concatMap (\s -> combinedCode' $ 'A':s) ps
        
          combinedCode' :: String -> [String]
          combinedCode' (c:c':cs) =
            [ p ++ p'
            | p <- m1 M.! (c, c')
            , p' <- combinedCode' (c':cs)
            ]
          combinedCode' [c] = [""]

main :: IO ()
main = do
    codes <- fmap lines getContents

    let codePad = M.filter (/= ' ') $ toMap ["789", "456", "123", " 0A"]
    let dirPad  = M.filter (/= ' ') $ toMap [" ^A", "<v>"]

    let codePaths = allPaths codePad
    let dirPaths = allPaths dirPad

    let codePaths' = filterToShortest (dirDist dirPaths) codePaths
    let dirPaths' = filterToShortest (dirDist dirPaths) dirPaths

    let compPath = foldr1 (compose (dirDist dirPaths')) [dirPaths', dirPaths', codePaths']

    print $ sum $ map (uncurry (*)) $ map (enterCode compPath) codes
