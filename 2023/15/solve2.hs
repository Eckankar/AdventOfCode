import qualified Data.Map as M

import Data.Char (ord)
import Data.List.Extra (wordsBy)

parse s = (hash letters, letters, op)
    where (letters, op) = break (\c -> c == '-' || c == '=') s

hash s = foldl update 0 s
    where update hv c = (hv + ord c) * 17 `mod` 256

initialize ls = foldl runOp M.empty ls
    where runOp :: M.Map Int [(String, Int)] -> (Int, String, String) -> M.Map Int [(String, Int)]
          runOp m (hv, letters, op) = M.insert hv nls m
            where ols = M.findWithDefault [] hv m
                  nls = (case op of
                           "-"      -> filter ((/= letters) . fst) ols
                           ('=':nv) -> if any ((== letters) . fst) ols
                                       then nls' else ols ++ [(letters, nv')]
                            where nls' = map (updateIfChanged nv') ols
                                  nv' = read nv)

                  updateIfChanged nv' (letters', ov) =
                    if letters == letters' then (letters, nv') else (letters', ov)

focusingPower hv ls = ((hv + 1) *) $ sum $ zipWith focusLens ls [1..]
    where focusLens (_, v) n = v*n

main :: IO ()
main = do
    input <- fmap (map parse . wordsBy (== ',')) getLine
    print $ sum $ map (uncurry focusingPower) $ M.toList $ initialize input
