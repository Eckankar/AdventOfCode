{-# LANGUAGE DataKinds #-}

import Data.Ext ((:+)(..), ext)
import Data.Geometry.Boundary (PointLocationResult(Outside))
import Data.Geometry.Box (box, inBox, insideBox)
import Data.Geometry.LineSegment (LineSegment(..), EndPoint(Open))
import Data.Geometry.Point (Point(Point2))
import Data.Geometry.Polygon (simpleFromPoints, inPolygon, listEdges)
import Data.Intersection (intersects)
import Data.List (sort)
import Data.Ratio (numerator)

import Control.Parallel.Strategies

import Debug.Trace (traceShow)

checkPots poly (p:ps) cur pc total | cur `mod` 100 == 0 && traceShow ((cur, total), pc) False = undefined
checkPots poly (((x,y), (x',y')):ps) cur pc total =
    if inPolygon (Point2 x y)   poly /= Outside &&
       inPolygon (Point2 x' y') poly /= Outside
    then ((x,y), (x',y')) : checkPots poly ps (cur+1) (pc+1) total
    else                    checkPots poly ps (cur+1) pc     total
checkPots _ [] _ _ _ = []

checkPot poly (ps@((x,y), (x',y'))) = (ps, v)
    where v = inPolygon (Point2 x y)   poly /= Outside &&
              inPolygon (Point2 x' y') poly /= Outside

checkEdges polyLines (ps@((x,y), (x',y'))) = (ps, inBound)
    where xmin = min x x'
          xmax = max x x'
          ymin = min y y'
          ymax = max y y'
          inBound = all notInBox polyLines
          
          -- exclude edges; then if we intersect, we know it's wrong.
          bboxInner = box (ext $ Point2 (xmin+1) (ymin+1)) (ext $ Point2 (xmax-1) (ymax-1))

          notInBox ls = not $ ls `intersects` bboxInner

displayPolyBox poly box = undefined

main :: IO ()
main = do
    redTiles <- fmap (map ((read :: String -> (Int, Int)) . ("(" ++) . (++ ")")) . lines) getContents

    let redRatTiles = [(toRational x, toRational y) | (x,y) <- redTiles]

    let poly = simpleFromPoints $ map (ext . uncurry Point2) redRatTiles
    
    let pots = [ ( (x,y'), (x',y) ) | (x,  y ) <- redRatTiles , (x', y') <- redRatTiles ]

    putStrLn (show (length pots) ++ " potentials")

    let reducedPots' = map fst $ filter snd $ withStrategy (parListChunk 1000 rdeepseq) $ map (checkPot poly) pots
    putStrLn (show (length reducedPots') ++ " reduced potentials")

    let polyLines = listEdges poly
    let reducedPots = map fst $ filter snd $ withStrategy (parListChunk 200 rdeepseq) $ map (checkEdges polyLines) reducedPots'
    putStrLn (show (length reducedPots) ++ " reduced reduced potentials")

    print $ maximum
        [ numerator $ (abs (x-x')+1) * (abs (y-y')+1) 
        | ((x, y), (x', y')) <- reducedPots
        ]