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

runRobot p = step 0 S.empty M.empty (0, 0) (-1, 0) $ runProgram [0] 0 0 (p, [])
    where step lc s m pos dir (True, _, _) = s
          -- if we haven't changed anything in 1000 steps, assume we're done
          step lc s _ _ _ _ | lc > 1000 = s
          step lc s m pos dir (False, Run pf, [pp, rot]) =
            step lc' s' m' pos' dir' $ pf [color]
            where changesColor = lookupColor pos /= pp
                  s'    = if changesColor then S.insert pos s else s
                  m'    = if changesColor then M.insert pos pp m else m
                  lc'   = if S.size s' > S.size s then 0 else lc+1
                  dir'  = if rot == 0 then rotateLeft dir else rotateRight dir
                  pos'  = add pos dir'
                  color = lookupColor pos'

                  lookupColor pos = M.findWithDefault 0 pos m


main :: IO ()
main = do
    input <- fmap (map read . wordsBy (== ',')) getContents
    let initProgram = M.fromList $ zip [0..] input
    print $ S.size $ runRobot initProgram
