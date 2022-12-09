import qualified Data.Set as S

type Point = (Int, Int)

data Direction = U | D | L | R
    deriving (Show, Eq, Read)

add :: Point -> Point -> Point
add (a, b) (c, d) = (a+c, b+d)

dirVec :: Direction -> Point
dirVec U = ( 0, -1)
dirVec D = ( 0,  1)
dirVec L = (-1,  0)
dirVec R = ( 1,  0)

dist :: Point -> Point -> Int
dist (a, b) (c, d) = max (abs (a-c)) (abs (b-d))

follow :: Point -> Point -> Point
follow p1 p2 | dist p1 p2 < 2 = p1
follow (a, b) (c, d) = (diff a c, diff b d)
    where diff x y = x + signum (y-x)

simulate :: Point -> Point -> [Direction] -> (Point, Point, S.Set Point)
simulate head tail = foldl sim' init
    where init = (head, tail, S.singleton tail)
          sim' (head, tail, trail) dir = (head', tail', trail')
            where head' = add head $ dirVec dir
                  tail' = follow tail head'
                  trail' = S.insert tail' trail

parseLine :: String -> [Direction]
parseLine (dir : ' ' : n) = replicate (read n) $ read [dir]

main :: IO ()
main = do
    input <- fmap (concat . map parseLine . lines) getContents

    let (head, tail, positions) = simulate (0, 0) (0, 0) input
    print $ S.size positions
