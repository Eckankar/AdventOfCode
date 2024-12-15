import qualified Data.Map as M

type Point = (Int, Int)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((j, i), c)) $ zip [0..] r) . zip [0..]

toDir '^' = ( 0, -1)
toDir 'v' = ( 0,  1)
toDir '<' = (-1,  0)
toDir '>' = ( 1,  0)

add (x, y) (x', y') = (x+x', y+y')

findRobot m = (p, m')
    where [p] = M.keys $ M.filter (== '@') m
          m' = M.insert p '.' m

handleBoulder r dir m =
    case m M.! n' of
        '#' -> (r, m)
        '.' -> (add dir r, m')
    where ns = takeWhile ((== 'O') . (m M.!)) $ tail $ iterate (add dir) r
          n' = add dir (last ns)
          m' = M.insert n' 'O' $ M.insert (add dir r) '.' m

executeCommands _ m [] = m
executeCommands r m (c:cs) =
    case m M.! (add r dir) of
        '.' -> executeCommands (add r dir) m cs
        '#' -> executeCommands r m cs
        'O' -> executeCommands r' m' cs
                where (r', m') = handleBoulder r dir m
    where dir = toDir c

scoreMap m = sum $ map (\(x, y) -> 100 * y + x) $ M.keys $ M.filter (== 'O') m

main :: IO ()
main = do
    (mapInput, "":cmds') <- fmap (span (/= "") . lines) getContents

    let m = toMap mapInput
    let cmds = concat cmds'

    let (robot, m') = findRobot m

    let m'' = executeCommands robot m' cmds

    print $ scoreMap m''
