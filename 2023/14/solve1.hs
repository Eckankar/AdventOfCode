import Data.List (transpose)

data Direction = North | South | East | West

moveBoulders :: [[Char]] -> Direction -> [[Char]]
moveBoulders ls West = map fixRow ls
    where fixRow r = bs' ++ rs'
            where (bs', rs') = foldr fixRow' ([], []) r

          fixRow' 'O' (bs, rs) = ('O':bs, rs)
          fixRow' '.' (bs, rs) = (bs, '.':rs)
          fixRow' '#' (bs, rs) = ([], '#': bs ++ rs)
moveBoulders ls North = transpose $ moveBoulders (transpose ls) West
moveBoulders _ _ = undefined

scoreBoard ls = sum $ zipWith (*) [1..] $ reverse $ map (sum . map scoreBoulder) ls
    where scoreBoulder 'O' = 1
          scoreBoulder _   = 0

main :: IO ()
main = do
    input <- fmap lines getContents

    print $ scoreBoard $ moveBoulders input North


