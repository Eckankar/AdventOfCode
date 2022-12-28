import qualified Data.Map as M
import Data.List.Split (wordsBy)

revDigits 0 _ = []
revDigits k n = d : revDigits (k-1) n'
  where (n', d) = n `divMod` 10

runProgram rv pc (p, d) =
    case op of
        1  -> cont 4 $ binOp (+)
        2  -> cont 4 $ binOp (*)
        3  -> cont 2 $ opRead
        4  -> cont 2 $ opWrite
        5  -> opJmp (/= 0)
        6  -> opJmp (== 0)
        7  -> cont 4 $ binOp (bInt (<))
        8  -> cont 4 $ binOp (bInt (==))
        99 -> (p, reverse d)
    where (op1:op2:ms) = revDigits 5 $ p M.! pc
          op = op2*10 + op1

          cont n = runProgram rv (pc+n)
          argA o = p M.! (pc + o + 1)
          arg o = imm (ms !! o) $ argA o

          imm 0 n = (p M.! n )
          imm 1 n = id n

          binOp f = (M.insert (argA 2) (f (arg 0) (arg 1)) p, d)

          opRead  = (M.insert (argA 0) rv p, d)
          opWrite = (p, arg 0 : d)

          opJmp pred = if pred $ arg 0 then runProgram rv (arg 1) (p, d) else cont 3 (p, d)

          bInt pred a b = if pred a b then 1 else 0


main :: IO ()
main = do
    input <- fmap (map read . wordsBy (== ',')) getContents
    let initProgram = M.fromList $ zip [0..] input

    let (program, diag) = runProgram 5 0 (initProgram, [])
    print diag

