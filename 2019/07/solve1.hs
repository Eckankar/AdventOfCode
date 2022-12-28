import qualified Data.Map as M
import Data.List (permutations)
import Data.List.Split (wordsBy)

revDigits 0 _ = []
revDigits k n = d : revDigits (k-1) n'
  where (n', d) = n `divMod` 10

runProgram rvs pc (p, d) =
    case op of
        1  -> cont rvs 4 $ binOp (+)
        2  -> cont rvs 4 $ binOp (*)
        3  -> cont rvs' 2 $ opRead
        4  -> cont rvs 2 $ opWrite
        5  -> opJmp (/= 0)
        6  -> opJmp (== 0)
        7  -> cont rvs 4 $ binOp (bInt (<))
        8  -> cont rvs 4 $ binOp (bInt (==))
        99 -> (p, reverse d)
    where (op1:op2:ms) = revDigits 5 $ p M.! pc
          op = op2*10 + op1
          (rv:rvs') = rvs

          cont rvs n = runProgram rvs (pc+n)
          argA o = p M.! (pc + o + 1)
          arg o = imm (ms !! o) $ argA o

          imm 0 n = (p M.! n )
          imm 1 n = id n

          binOp f = (M.insert (argA 2) (f (arg 0) (arg 1)) p, d)

          opRead  = (M.insert (argA 0) rv p, d)
          opWrite = (p, arg 0 : d)

          opJmp pred = if pred $ arg 0 then runProgram rvs (arg 1) (p, d) else cont rvs 3 (p, d)

          bInt pred a b = if pred a b then 1 else 0

thrusterCode p cs = foldl tryDigit 0 cs
    where tryDigit thrust phase = thrust'
            where (_, [thrust']) = runProgram [phase, thrust] 0 (p, [])

main :: IO ()
main = do
    input <- fmap (map read . wordsBy (== ',')) getContents
    let initProgram = M.fromList $ zip [0..] input

    print $ maximum $ map (thrusterCode initProgram) $ permutations [0..4]

