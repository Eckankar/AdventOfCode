import qualified Data.Map as M
import Data.List.Split (wordsBy)

runProgram pc p =
    case p M.! pc of
        1  -> binOp (+)
        2  -> binOp (*)
        99 -> p
    where op = p M.! pc
          binOp f = runProgram (pc+4) p'
            where [aa,ba,t] = map (p M.!) [pc + 1 .. pc + 3]
                  [a, b] = map (p M.!) [aa,ba]
                  p' = M.insert t (f a b) p

main :: IO ()
main = do
    input <- fmap (map read . wordsBy (== ',')) getContents
    let program = M.fromList $ zip [0..] input

    let [(noun, verb)] =
            [ (noun, verb) | noun <- M.keys program,
                             verb <- M.keys program,
                             let program' = M.insert 1 noun $ M.insert 2 verb program,
                             let res = runProgram 0 program',
                             res M.! 0 == 19690720
            ]

    print $ 100*noun + verb

