{-# LANGUAGE TupleSections #-}

import qualified Data.Map as M

main :: IO ()
main = do
    nums <- fmap (map read . words . map deComma) getLine
    let initialState = M.fromListWith (+) $ map (,1) nums
    print $ M.foldl (+) 0 $ iterate nextGen initialState !! 256

        where deComma ',' = ' '
              deComma c   = c

              nextGen m = M.fromListWith (+) $ concatMap updateKey $ M.toList m

              updateKey (0, n) = [(6, n), (8, n)]
              updateKey (i, n) = [(i-1, n)]
