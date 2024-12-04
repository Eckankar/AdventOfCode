import qualified Data.Map as M

type Point = (Int, Int)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [0..] r) . zip [0..]

neighborDirs = [ (dx, dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0) ]

add (x, y) (x', y') = (x+x', y+y')

main :: IO ()
main = do
    input <- fmap (toMap . lines) getContents

    let xs = M.keys $ M.filter (== 'X') input

    let words = [ (xp, mp, ap, sp) |
                  xp <- xs, d <- neighborDirs,
                  let mp = add xp d, M.findWithDefault '.' mp input == 'M',
                  let ap = add mp d, M.findWithDefault '.' ap input == 'A',
                  let sp = add ap d, M.findWithDefault '.' sp input == 'S'
                ]

    print $ length words
