import qualified Data.Map as M
import qualified Data.Sequence as Seq

import Data.List.Split (chunksOf, wordsBy)
import Data.Tuple.Extra (snd3, thd3)

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

addresses = [0..49]

runMachines fs im =
    if Seq.null tm
    then runMachines fs' im'
    else seqHead tm
    where is = map (seqHead . (im M.!)) addresses
          os = zipWith ($) fs is
          fs' = map (deRun . snd3) os
          ps = chunksOf 3 $ concatMap thd3 os

          im' = foldl queueUp (M.map seqTail im) ps

          tm = im' M.! 255

          deRun (Run f) = f

          seqHead (h Seq.:<| _) = h
          seqHead _ = [-1]

          seqTail (_ Seq.:<| s) = s
          seqTail _ = Seq.empty

          queueUp im [a,x,y] = M.insertWith (Seq.><) a (Seq.singleton [x, y]) im

main :: IO ()
main = do
    input <- fmap (map read . wordsBy (== ',')) getContents
    let initProgram = M.fromList $ zip [0..] input

    let pf i = runProgram i 0 0 (initProgram, [])

    let im = M.fromList [ (i, Seq.singleton [i]) | i <- 255 : addresses ]
    print $ (!! 1) $ runMachines (replicate (length addresses) pf) im
