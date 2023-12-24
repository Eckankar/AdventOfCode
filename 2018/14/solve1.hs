import qualified Data.Map as M

import Data.Char (digitToInt)

createRecipes st slen tslen (e1, e2)
    | slen >= tslen = st
    | otherwise = createRecipes st' slen' tslen (e1', e2')
        where e1v = st M.! e1
              e2v = st M.! e2
              eNums = map digitToInt $ show $ e1v + e2v
              st' = M.union st $ M.fromList $ zip [slen..] eNums
              slen' = slen + length eNums
              e1' = (e1 + e1v + 1) `mod` slen'
              e2' = (e2 + e2v + 1) `mod` slen'

main :: IO ()
main = do
    input <- fmap (read :: String -> Int) getContents

    let initState = M.fromList [ (0, 3), (1, 7) ] 

    let finalState = createRecipes initState 2 (input+10) (0, 1) 
    putStrLn $ foldl (++) "" $ map (show . (finalState M.!)) [input .. (input+9)]
