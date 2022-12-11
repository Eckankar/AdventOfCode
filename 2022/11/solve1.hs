import           Data.Char (isSpace)
import           Data.List (sort, stripPrefix)
import           Data.List.Split (splitOn, wordsBy)
import qualified Data.Map as M

data Monkey = Monkey Int [Int] (Int -> (Int, Int))

parseMonkey is = Monkey n items mFun
    where [mline, itemline, opline, testline, trueline, falseline] =
               map (dropWhile isSpace) is
          Just n     = fmap (read . init) $ stripPrefix "Monkey " mline
          Just items = fmap (map read . splitOn ", " ) $ stripPrefix "Starting items: " itemline

          mFun x = (if x' `mod` d == 0 then t else f, x')
            where x' = opF (realize opM x) (realize opN x) `div` 3

          Just [opM, op, opN] = fmap words $ stripPrefix "Operation: new = " opline
          opF = case op of
                  "*" -> (*)
                  "+" -> (+)

          realize "old" x = x
          realize n     _ = read n

          Just d = fmap read $ stripPrefix "Test: divisible by " testline
          Just t = fmap read $ stripPrefix "If true: throw to monkey " trueline
          Just f = fmap read $ stripPrefix "If false: throw to monkey " falseline

monkeyN (Monkey n _ _) = n

runMonkies :: ([Monkey], M.Map Int Int) -> ([Monkey], M.Map Int Int)
runMonkies (ms, mc) = (map (mmap' M.!) monkeyNs, mc')
    where mmap = M.fromList $ map (\m -> (monkeyN m, m)) ms
          monkeyNs = map monkeyN ms
          (mmap', mc') = foldl runMonkey (mmap, mc) monkeyNs

          runMonkey :: (M.Map Int Monkey, M.Map Int Int) ->
                       Int ->
                       (M.Map Int Monkey, M.Map Int Int)
          runMonkey (mmap, mc) n = (foldl updateItem mmap' items, mc')
              where Monkey _ items mFun = mmap M.! n
                    mmap' = M.insert n (Monkey n [] mFun) mmap
                    mc' = M.insertWith (+) n (length items) mc
                    updateItem mmap i = M.adjust addToMonkey targetN mmap
                        where (targetN, newVal) = mFun i
                              addToMonkey (Monkey n' items' mFun') = Monkey n' (items' ++ [newVal]) mFun'


main :: IO ()
main = do
    input <- fmap (wordsBy null . lines) getContents
    let monkies = map parseMonkey input
    let monkeyCounts = M.fromList $ map (\m -> (monkeyN m, 0)) monkies
    let monkeyRuns = iterate runMonkies (monkies, monkeyCounts)
    print $ foldl (*) 1 $ take 2 $ reverse $ sort $ M.elems $ snd $ monkeyRuns !! 20
