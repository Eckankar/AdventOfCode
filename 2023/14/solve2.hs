import qualified Data.Map as M

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
moveBoulders ls East = map reverse $ moveBoulders (map reverse ls) West
moveBoulders ls South = reverse $ moveBoulders (reverse ls) North

scoreBoard ls = sum $ zipWith (*) [1..] $ reverse $ map (sum . map scoreBoulder) ls
    where scoreBoulder 'O' = 1
          scoreBoulder _   = 0

cycleBoulders ls = foldl moveBoulders ls [North, West, South, East]

findScore :: [[[Char]]] -> Int -> Int
findScore rounds n =
    if n < lStart
    then scoreBoard (rounds !! n)
    else scoreBoard (rounds !! n')
    where (m, rep, lStart) = findCycle rounds M.empty 0
          n' = ((n - lStart) `mod` (rep - lStart)) + lStart
          findCycle :: [[[Char]]] -> M.Map [[Char]] Int -> Int -> (M.Map [[Char]] Int, Int, Int)
          findCycle (ls:lss) m p =
            if M.member ls m
            then (m, p, m M.! ls)
            else findCycle lss (M.insert ls p m) (p+1)

main :: IO ()
main = do
    input <- fmap lines getContents

    let states = iterate cycleBoulders input
    print $ findScore states 1000000000

