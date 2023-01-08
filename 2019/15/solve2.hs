import qualified Data.Map as M
import qualified Data.Set as S

import Data.List.Split (wordsBy)
import Data.Maybe (isJust)

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

neighbors (x,y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

bfs :: ([Int] -> (Bool, ProgramRun, [Int])) ->
       M.Map (Int, Int) [Int] ->
       S.Set (Int, Int) ->
       Int ->
       Maybe (Int, Int) ->
       (S.Set (Int, Int), (Int, Int))
bfs _  aps ts _ (Just g) | M.null aps = (ts, g)
bfs pf aps ts dist g = bfs pf aps' ts' (dist+1) g'
    where (aps', g') = M.foldrWithKey insertNeighbors (M.empty, g) aps
          insertNeighbors p path (m, g) = foldr insertNeighbor (m, g) $ zip [1..4] $ neighbors p
            where insertNeighbor (n, p') (m, g) = (m', g')
                    where (_, _, os) = pf $ reverse path'
                          out = last os
                          m' = if p' `M.member` m || p' `S.member` ts' || out == 0
                               then m
                               else M.insert p' path' m
                          g' = if isJust g then g else if out == 2 then Just p' else Nothing
                          path' = n:path

          ts' = S.union ts $ M.keysSet aps

bfs2 :: S.Set (Int, Int) -> S.Set (Int, Int) -> M.Map (Int, Int) Int -> Int -> M.Map (Int, Int) Int
bfs2 m aps dm dist = if S.null aps then dm' else bfs2 m aps' dm' (dist+1)
        where aps' = S.intersection m $ S.difference (S.fromList $ concatMap neighbors $ S.toList aps) $ M.keysSet dm'
              dm' = M.union dm $ M.fromList $ S.toList $ S.map (\p -> (p, dist)) aps

main :: IO ()
main = do
    input <- fmap (map read . wordsBy (== ',')) getContents
    let initProgram = M.fromList $ zip [0..] input

    let fp = \i -> runProgram i 0 0 (initProgram, [])
    let (ps, g) = bfs fp (M.singleton (0, 0) []) S.empty 0 Nothing
    print (g, S.size ps)
    print $ maximum $ M.elems $ bfs2 ps (S.singleton g) M.empty 0
