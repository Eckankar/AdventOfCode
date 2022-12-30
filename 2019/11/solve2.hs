import qualified Data.Map as M
import qualified Data.Set as S

import Data.List.Split (wordsBy)

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

rotateLeft (x, y) = (-y, x)
rotateRight (x, y) = (y, -x)

add (x, y) (x', y') = (x+x', y+y')

runRobot p m = step 0 S.empty m (0, 0) (-1, 0) $ runProgram [lookupColor m (0, 0)] 0 0 (p, [])
    where step lc s m pos dir (True, _, _) = m
          -- if we haven't changed anything in 1000 steps, assume we're done
          step lc _ m _ _ _ | lc > 1000 = m
          step lc s m pos dir (False, Run pf, [pp, rot]) =
            step lc' s' m' pos' dir' $ pf [color]
            where changesColor = lookupColor m pos /= pp
                  s'    = if changesColor then S.insert pos s else s
                  m'    = if changesColor then M.insert pos pp m else m
                  lc'   = if S.size s' > S.size s then 0 else lc+1
                  dir'  = if rot == 0 then rotateLeft dir else rotateRight dir
                  pos'  = add pos dir'
                  color = lookupColor m' pos'

          lookupColor m pos = M.findWithDefault 0 pos m

bounds es = ((minX, minY), (maxX, maxY))
    where (minX, maxX) = minMax $ map fst es
          (minY, maxY) = minMax $ map snd es
          minMax (e:es) = foldr (\a (lo, hi) -> (min a lo, max a hi)) (e,e) es

draw es = [ [ if (x, y) `S.member` ess then '#' else '.' | y <- [minY .. maxY] ] | x <- [minX .. maxX ] ]
    where ess = S.fromList es
          ((minX, minY), (maxX, maxY)) = bounds es

main :: IO ()
main = do
    input <- fmap (map read . wordsBy (== ',')) getContents
    let initProgram = M.fromList $ zip [0..] input
    let initMap = M.fromList [ ((0, 0), 1) ]
    let resultMap = runRobot initProgram initMap

    let ps = M.keys $ M.filter (== 1) resultMap
    mapM_ putStrLn $ draw ps

