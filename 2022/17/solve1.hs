import qualified Data.Set as S

import Debug.Trace (traceShow, traceShowId)

type Point = (Int, Int)
type Piece = S.Set Point

pieces :: [Piece]
pieces = cycle $ map S.fromList [
        [ (0,0), (1,0), (2,0), (3,0) ],
        [ (1,0), (0,1), (1,1), (2,1), (1,2) ],
        [ (0,0), (1,0), (2,0), (2,1), (2,2) ],
        [ (0,0), (0,1), (0,2), (0,3) ],
        [ (0,0), (0,1), (1,0), (1,1) ]
    ]

add :: Point -> Point -> Point
add (a, b) (a', b') = (a+a', b+b')

direction '<' = (-1, 0)
direction '>' = ( 1, 0)

simulate :: [Piece] -> [Char] -> [(S.Set Point, Int)]
simulate ps ds = simulate' ps Nothing ds 0 initMap
    where simulate' :: [Piece] -> Maybe Piece -> [Char] -> Int -> S.Set Point -> [(S.Set Point, Int)]
          simulate' (p:ps) Nothing ds top m = simulate' ps (Just $ realizePiece top p) ds top m
          simulate' ps (Just p) (d:ds) top m =
            if pieceOverlaps p3
            then (m', top') : simulate' ps Nothing ds top' m'
            else simulate' ps (Just p3) ds top m
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


main :: IO ()
main = do
    input <- getLine
    let moves = cycle input

    let sim = simulate pieces moves
    print $ snd $ sim !! 2021
