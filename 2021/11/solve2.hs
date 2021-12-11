import qualified Data.Array as A
import Data.List (intercalate)
import qualified Data.Map as M
import GHC.Exts (groupWith)

data Octopus = Unflashed Int | Flashed
    deriving (Show, Eq)

neighbors (x,y) = filter (/= (x,y)) $ zip (map (x+) $ cycle [-1, 0, 1]) $ map (y+) $ concatMap (replicate 3) [-1, 0, 1]

bumpAll (m, c) = (M.map (+1) m, c)

setUnflashed (m, c) = (M.map Unflashed m, c)

flash (m, c) = if m == m'' then (m, c) else flash (m'', c')
    where (m', c', ns) = doFlashes m c
          m'' = foldr (M.adjust bump) m' $ filter (`M.member` m') ns

          bump (Unflashed n) = Unflashed (n+1)
          bump Flashed       = Flashed

          doFlashes m c = M.foldrWithKey flashCell (m, c, []) m

          flashCell k (Unflashed n) (m, c, ns) | n > 9 =
                ( M.insert k Flashed m
                , c+1
                , neighbors k ++ ns
                )
          flashCell k _             (m, c, ns) = (m, c, ns)

clearMarks (m, c) = (M.map clearMark m, c)
    where clearMark (Unflashed n) = n
          clearMark Flashed       = 0

printMap :: M.Map (Int, Int) Int -> IO ()
printMap m = putStrLn $ (++ "\n") $ intercalate "\n" $ map (concatMap (show . snd)) $ groupWith (\((a,_),_) -> a) $ M.toList m

main :: IO ()
main = do
    input <- fmap (map (map ((0+) . read . (:[]))) . lines) getContents
    let (height, width) = (length input, length $ head input)
    let inputMap = M.fromList $ A.assocs $ A.listArray ((0, 0), (height-1, width-1)) $ concat input

    let iterations = iterate (clearMarks . flash . setUnflashed . bumpAll) (inputMap, 0)
    print $ length $ takeWhile (any (>0) . fst) iterations
