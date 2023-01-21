import qualified Data.Map as M

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

data Range = Range Int Int
    deriving (Eq, Show)

deployDrone p (x, y) = output
    where (_, _, [output]) = runProgram [x, y] 0 0 (p, [])

stepRange deploy (y, Range lo hi) = (y', Range lo' hi')
    where y' = y + 1
          (lo':_) = [ x | x <- [lo..], deploy (x, y') == 1 ]
          (hi':_) = [ x-1 | x <- [hi..], deploy (x, y') == 0 ]

findOverlap (y, Range lo hi) (y', Range lo' hi') = (y, Range lo' hi)

main :: IO ()
main = do
    input <- fmap (map read . wordsBy (== ',')) getContents
    let initProgram = M.fromList $ zip [0..] input
    let deployDrone' = deployDrone initProgram

    -- putStrLn $ unlines [ [ if deployDrone' (x, y) == 1 then '#' else '.' | x <- [0..49] ] | y <- [0..49] ]
    --
    -- The inital bit of the ray phases in and out.
    -- Skip past that, to row 49 where we should have a solid beam.
    -- (In my data, it stabilizes on row 8; 49 should be more than safe.)
    -- Pretend the range is 0-0 before then; doesn't matter for the end result
    let r49 = [ x | x <- [0..49], deployDrone' (x, 49) == 1 ]
    let range49 = Range (minimum r49) (maximum r49)

    let ranges = zip [0..48] (replicate 49 (Range 0 0)) ++ iterate (stepRange deployDrone') (49, range49)
    let ((y, Range x _):_) = filter (\(_, Range lo hi) -> hi-lo+1 >= 100) $ zipWith findOverlap ranges (drop 99 ranges)
    print $ 10000*x + y
