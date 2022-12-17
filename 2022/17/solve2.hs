import qualified Data.Set as S

import Debug.Trace (traceShow, traceShowId)

type Point = (Int, Int)
type Piece = S.Set Point

pieces :: [(Int, Piece)]
pieces = cycle $ zip [0..] $ map S.fromList [
        [ (0,0), (1,0), (2,0), (3,0) ],
        [ (1,0), (0,1), (1,1), (2,1), (1,2) ],
        [ (0,0), (1,0), (2,0), (2,1), (2,2) ],
        [ (0,0), (0,1), (0,2), (0,3) ],
        [ (0,0), (0,1), (1,0), (1,1) ]
    ]
numPieces = 5

add :: Point -> Point -> Point
add (a, b) (a', b') = (a+a', b+b')

sub :: Point -> Point -> Point
sub (a, b) (a', b') = (a-a', b-b')

direction '<' = (-1, 0)
direction '>' = ( 1, 0)

simulate :: [(Int, Piece)] -> [(Int, Char)] -> [(S.Set Point, Int, (Int, Int))]
simulate ps ds = simulate' ps Nothing ds 0 initMap
    where simulate' :: [(Int, Piece)] -> Maybe (Int, Piece) -> [(Int, Char)] -> Int -> S.Set Point -> [(S.Set Point, Int, (Int, Int))]
          simulate' ((pi, p):ps) Nothing ds top m = simulate' ps (Just $ (pi, realizePiece top p)) ds top m
          simulate' ps (Just (pi, p)) ((di, d):ds) top m =
            if pieceOverlaps p3
            then (m', top', (pi, di)) : simulate' ps Nothing ds top' m'
            else simulate' ps (Just (pi, p3)) ds top m
              where p1 = S.map (add $ direction d) p
                    p2 = if pieceInBounds p1 && not (pieceOverlaps p1) then p1 else p
                    p3 = S.map (add (0, -1)) p2
                    pieceOverlaps = not . S.null . S.intersection m
                    m' = S.union m p2
                    top' = max top $ S.findMax $ S.map snd p2

          initMap = S.fromList [ (x, 0) | x <- [0..6] ]

          realizePiece top = S.map (add (2, top+4))
          inBounds (x, y) = x >= 0 && x < 7
          pieceInBounds = S.foldr (\e a -> a && inBounds e) True

findResult n s dh l ds = s + m * dh + ds !! r
    where (m, r) = traceShowId $ n `divMod` l


main :: IO ()
main = do
    input <- getLine
    let moves = cycle $ zip [0..] input

    let sim = simulate pieces moves

    -- skip a litle of the beginning to let the simulation stabilize
    let skip = 1000
    let (e1@(_,h1,d1)):sim1 = drop skip sim

    -- then verify that we've got a stable recurring structure
    let (a1, (e2@(_,h2,d2)):sim2)  = span (\(_,_,d') -> d' /= d1) sim1
    print $ length a1
    let (a2, (e3@(_,h3,d3)):sim3)  = span (\(_,_,d') -> d' /= d1) sim2
    print $ length a2
    let (a3, (e4@(_,h4,d4)):sim4)  = span (\(_,_,d') -> d' /= d1) sim3
    print $ length a3

    let target = 1000000000000

    if S.size (S.fromList [h2-h1, h3-h2, h4-h3]) == 1 &&
       S.size (S.fromList $ map length [a1, a2, a3]) == 1
    then print $ findResult (target-skip-1) h1 (h2-h1) (length a1 + 1) $ map ((-h1) +) $ map (\(_,h,_) -> h) $ e1:a1
    else error "We have a problem. :("
