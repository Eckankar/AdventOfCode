import qualified Data.Map as M
import qualified Data.Set as S

import Debug.Trace (trace)

type Point = (Int, Int)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((j, i), c)) $ zip [0..] r) . zip [0..]

preProcMap = map (concatMap fixup)
    where fixup '#' = "##"
          fixup 'O' = "[]"
          fixup '.' = ".."
          fixup '@' = "@."

toDir '^' = ( 0, -1)
toDir 'v' = ( 0,  1)
toDir '<' = (-1,  0)
toDir '>' = ( 1,  0)

add (x, y) (x', y') = (x+x', y+y')

findRobot m = (p, m')
    where [p] = M.keys $ M.filter (== '@') m
          m' = M.insert p '.' m

handleBoulder r dir m | dir == (-1, 0) || dir == (1, 0) =
    case m M.! n' of
        '#' -> (r, m)
        '.' -> (add dir r, m')
    where ns = takeWhile ((\b -> b == '[' || b == ']') . (m M.!)) $ tail $ iterate (add dir) r
          n' = add dir (last ns)
          m'' = M.restrictKeys m $ S.fromList ns
          m' = M.insert (add dir r) '.' $ M.union (M.mapKeys (add dir) m'') m
handleBoulder r dir m =
    case cb of
        Nothing -> (r, m)
        Just rs -> (add dir r, m')
            where m'' = M.restrictKeys m rs
                  m' = M.unions [M.mapKeys (add dir) m'', M.map (const '.') m'', m]
    where cb = checkBoulders $ S.singleton $ add r dir
          checkBoulders bs =
            if S.null bs''
            then Just bs'
            else if not $ S.null $ S.filter ((== '#') . (m M.!)) bs''
            then Nothing
            else case checkBoulders bs'' of
                     Nothing -> Nothing
                     Just bss -> Just $ S.union bss bs'
              where bs' = S.unions $ S.map addPartner bs
                    bs'' = S.filter ((/= '.') . (m M.!)) $ S.map (add dir) bs'

          addPartner bp = S.fromList [bp,bp2]
              where bp2 = case m M.! bp of
                            '[' -> add bp $ toDir '>'
                            ']' -> add bp $ toDir '<'


executeCommands r m _ | trace (renderMap r m) False = undefined
executeCommands _ m [] = m
executeCommands r m (c:cs) =
    trace [c] $
    case m M.! (add r dir) of
        '.' -> executeCommands (add r dir) m cs
        '#' -> executeCommands r m cs
        '[' -> executeCommands r' m' cs
        ']' -> executeCommands r' m' cs
    where dir = toDir c
          (r', m') = handleBoulder r dir m

scoreMap m = sum $ map (\(x, y) -> 100 * y + x) $ M.keys $ M.filter (== '[') m

renderMap r m = unlines $ draw $ M.insert r '@' m

bounds es = ((minX, minY), (maxX, maxY))
    where (minX, maxX) = minMax $ map fst es
          (minY, maxY) = minMax $ map snd es
          minMax (e:es) = foldr (\a (lo, hi) -> (min a lo, max a hi)) (e,e) es

draw :: M.Map Point Char -> [String]
draw m = [ [ m M.! (x, y) | x <- [minX .. maxX] ] | y <- [minY .. maxY] ]
    where ((minX, minY), (maxX, maxY)) = bounds $ M.keys m

main :: IO ()
main = do
    (mapInput, "":cmds') <- fmap (span (/= "") . lines) getContents

    let m = toMap $ preProcMap mapInput
    let cmds = concat cmds'

    let (robot, m') = findRobot m

    let m'' = executeCommands robot m' cmds

    print $ scoreMap m''
