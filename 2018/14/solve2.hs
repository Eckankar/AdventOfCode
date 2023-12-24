import qualified Data.Map as M

import Data.Char (digitToInt)
import Data.List.Utils (subIndex)
import Data.Maybe (fromJust)

digits n = map digitToInt $ show n

createRecipes st slen tslen (e1, e2) = eNums ++ createRecipes st' slen' tslen (e1', e2')
        where e1v = st M.! e1
              e2v = st M.! e2
              eNums = digits $ e1v + e2v
              st' = M.union st $ M.fromList $ zip [slen..] eNums
              slen' = slen + length eNums
              e1' = (e1 + e1v + 1) `mod` slen'
              e2' = (e2 + e2v + 1) `mod` slen'

main :: IO ()
main = do
    input <- getLine
    let inputN = read input

    let initState = M.fromList [ (0, 3), (1, 7) ] 

    let recipes = 3:7:createRecipes initState 2 (inputN+10) (0, 1) 

    print $ fromJust $ subIndex (map digitToInt input) recipes

