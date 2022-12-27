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
    let initProgram = M.fromList $ zip [0..] input

    let initProgram' = M.insert 1 12 $ M.insert 2 2 initProgram
    let program = runProgram 0 initProgram'
    print $ program M.! 0

