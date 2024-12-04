import qualified Data.Map as M
import qualified Data.Set as S

type Point = (Int, Int)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [0..] r) . zip [0..]

neighborDirs = [ (dx, dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0) ]

add (x, y) (x', y') = (x+x', y+y')

main :: IO ()
main = do
    input <- fmap (toMap . lines) getContents

    let as = M.keys $ M.filter (== 'A') input
    let msSet = S.fromList ['M', 'S']

    let words = [ ap |
                  ap <- as,
                  let s1 = S.fromList [ add ap (1,1), add ap (-1,-1) ],
                  let s2 = S.fromList [ add ap (1,-1), add ap (-1,1) ],

                  let s1' = S.map (\p -> M.findWithDefault '.' p input) s1,
                  let s2' = S.map (\p -> M.findWithDefault '.' p input) s2,
                  s1' == msSet && s2' == msSet
                ]

    print $ length words
