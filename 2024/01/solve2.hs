import qualified Data.Map as M

import Data.List (transpose)

increaseBy m Nothing  = Just m
increaseBy m (Just n) = Just $ n+m

count = foldr (M.alter (increaseBy 1)) M.empty

main :: IO ()
main = do
    [l1, l2] <- fmap (transpose . map (map (read :: String -> Int) . words) . lines) getContents
    let cl2 = count l2
    print $ sum $ map (\n -> n * M.findWithDefault 0 n cl2) l1
