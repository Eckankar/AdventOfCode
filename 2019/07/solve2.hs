import qualified Data.Map as M
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Data.List (permutations)
import Data.List.Split (wordsBy)

type Program = M.Map Int Int

data ProgramRun =
      Run ([Int] -> (Bool, ProgramRun, [Int]))
    | Done

revDigits 0 _ = []
revDigits k n = d : revDigits (k-1) n'
  where (n', d) = n `divMod` 10

runProgram :: [Int] -> Int -> (Program, [Int]) -> (Bool, ProgramRun, [Int])
runProgram rvs pc (p, d) =
    case op of
        1  -> cont rvs 4 $ binOp (+)
        2  -> cont rvs 4 $ binOp (*)
        3  -> opRead
        4  -> cont rvs 2 $ opWrite
        5  -> opJmp (/= 0)
        6  -> opJmp (== 0)
        7  -> cont rvs 4 $ binOp (bInt (<))
        8  -> cont rvs 4 $ binOp (bInt (==))
        99 -> (True, Done, reverse d)
    where (op1:op2:ms) = revDigits 5 $ p M.! pc
          op = op2*10 + op1
          (rv:rvs') = rvs

          cont rvs n = runProgram rvs (pc+n)
          argA o = p M.! (pc + o + 1)
          arg o = imm (ms !! o) $ argA o

          imm 0 = (p M.!)
          imm 1 = id

          binOp f = (M.insert (argA 2) (f (arg 0) (arg 1)) p, d)

          opRead  =
            if null rvs
            then (False, Run $ \rvs' -> runProgram rvs' pc (p, []), reverse d)
            else cont rvs' 2 $ (M.insert (argA 0) rv p, d)
          opWrite = (p, arg 0 : d)

          opJmp pred = if pred $ arg 0 then runProgram rvs (arg 1) (p, d) else cont rvs 3 (p, d)

          bInt pred a b = if pred a b then 1 else 0

thrusterCode p cs = sim initSs
    where initSs = Seq.fromList $ zip [1..5] $ map (\i -> runProgram i 0 (p, [])) $ zipWith (:) cs $ [0] : replicate 4 []

          sim ((5, (True, Done, o)) Seq.:<| ss) = last o
          sim (f Seq.:<| f2@(_, (True, _, _)) Seq.:<| ss) = sim (f2 Seq.<| (ss Seq.|> f))
          sim (f@(_, (_, _, o)) Seq.:<| (i, (False, Run fp, _)) Seq.:<| ss) = sim ((i, fp o) Seq.<| (ss Seq.|> f))

main :: IO ()
main = do
    input <- fmap (map read . wordsBy (== ',')) getContents
    let initProgram = M.fromList $ zip [0..] input

    print $ maximum $ map (thrusterCode initProgram) $ permutations [5..9]

