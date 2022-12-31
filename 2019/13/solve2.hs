import qualified Data.Map as M
import qualified Data.Set as S

import Control.Concurrent (threadDelay)
import Data.List.Split (chunksOf, wordsBy)
import System.Console.ANSI (clearScreen, setCursorPosition)

type Program = M.Map Int Int

data ProgramRun =
      Run ([Int] -> (Bool, ProgramRun, [Int]))
    | Done

revDigits 0 _ = []
revDigits k n = d : revDigits (k-1) n'
  where (n', d) = n `divMod` 10

runProgram :: [Int] -> Int -> Int -> (Program, [Int]) -> (Bool, ProgramRun, [Int])
runProgram rvs pc rb (p, d) =
    case op of
        1  -> cont rvs 4 $ binOp (+)
        2  -> cont rvs 4 $ binOp (*)
        3  -> opRead
        4  -> cont rvs 2 $ opWrite
        5  -> opJmp (/= 0)
        6  -> opJmp (== 0)
        7  -> cont rvs 4 $ binOp (bInt (<))
        8  -> cont rvs 4 $ binOp (bInt (==))
        9  -> opAdjustRB
        99 -> (True, Done, reverse d)
    where (op1:op2:ms) = revDigits 5 $ p M.! pc
          op = op2*10 + op1
          (rv:rvs') = rvs

          cont rvs n = runProgram rvs (pc+n) rb

          argA o = rebase (ms !! o) $ M.findWithDefault 0 (pc + o + 1) p
          arg o = imm (ms !! o) $ argA o

          rebase 2 n = n + rb
          rebase _ n = n

          imm 1 n = n
          imm _ n = M.findWithDefault 0 n p

          binOp f = (M.insert (argA 2) (f (arg 0) (arg 1)) p, d)

          opRead  =
            if null rvs
            then (False, Run $ \rvs' -> runProgram rvs' pc rb (p, []), reverse d)
            else cont rvs' 2 $ (M.insert (argA 0) rv p, d)
          opWrite = (p, arg 0 : d)

          opJmp pred = if pred $ arg 0 then runProgram rvs (arg 1) rb (p, d) else cont rvs 3 (p, d)

          opAdjustRB = runProgram rvs (pc + 2) (rb + arg 0) (p, d)

          bInt pred a b = if pred a b then 1 else 0

sym 0 = '.'
sym 1 = '#'
sym 2 = '□'
sym 3 = '='
sym 4 = 'o'

toTriplets = map toTriplet . chunksOf 3
    where toTriplet [x, y, t] = ((x,y), t)

simulate :: M.Map (Int, Int) Int -> (Bool, ProgramRun, [Int]) -> IO Int
simulate m (True, Done, ts) = return $ snd $ head $ filter ((== (-1, 0)) . fst) $ toTriplets ts
simulate m (False, Run pf, ts) = do
    let m' = foldl (\m (p, v) -> M.insert p v m) m $ toTriplets ts
    clearScreen
    setCursorPosition 0 0
    mapM_ putStrLn $ draw m'
    let [(xp,yp)] = M.keys $ M.filter (== 3) m'
    let [(xb,yb)] = M.keys $ M.filter (== 4) m'
    threadDelay 10000--00
    simulate m' $ pf [ signum (xb-xp) ]

bounds es = ((minX, minY), (maxX, maxY))
    where (minX, maxX) = minMax $ map fst es
          (minY, maxY) = minMax $ map snd es
          minMax (e:es) = foldr (\a (lo, hi) -> (min a lo, max a hi)) (e,e) es

draw :: M.Map (Int, Int) Int -> [String]
draw m = [ [ sym $ M.findWithDefault 0 (x, y) m | x <- [minX .. maxX] ] | y <- [minY .. maxY] ]
    where ((minX, minY), (maxX, maxY)) = bounds $ filter (/= (-1, 0)) $ M.keys m

main :: IO ()
main = do
    input <- fmap (map read . wordsBy (== ',')) getContents
    let initProgram = M.insert 0 2 $ M.fromList $ zip [0..] input

    score <- simulate M.empty $ runProgram [] 0 0 (initProgram, [])
    print score
