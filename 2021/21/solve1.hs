import Data.List.Split (chunksOf)

playGame (a, b) (as, bs) c (r:rs)
    | bs >= 1000  = (as, bs, c)
    | otherwise   = playGame (b, a') (bs, as') (c+1) rs
        where a'  = (a + r) `mod` 10
              as' = as + (a' + 1)
playGame _ _ _ [] = error "List empty?"

main :: IO ()
main = do
    [a, b] <- fmap (map (read . (!! 4) . words) . lines) getContents
    let rolls = map sum $ chunksOf 3 $ cycle [1..100]
    let (a', b', c) = playGame (a-1, b-1) (0, 0) 0 rolls
    print $ a' * 3 * c
