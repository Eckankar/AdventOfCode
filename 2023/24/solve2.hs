import qualified Data.SBV as Z
import qualified Data.SBV.Control as ZC
import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import Text.ParserCombinators.ReadP

import Debug.Trace (traceShow, traceShowId)

type Point = (Int, Int, Int)

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- many (satisfy isDigit)
    return $ read $ sign : digits

parsePoint :: ReadP Point
parsePoint = do
    skipSpaces
    x <- parseInt
    skipSpaces
    char ','
    skipSpaces
    y <- parseInt
    skipSpaces
    char ','
    skipSpaces
    z <- parseInt
    skipSpaces
    return (x, y, z)

parseHail :: ReadP (Point, Point)
parseHail = do
    pos <- parsePoint
    char '@'
    vel <- parsePoint
    skipSpaces
    return (pos, vel)

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p
    
buildRock = do
    x <- Z.free "stone_x"
    y <- Z.free "stone_y"
    z <- Z.free "stone_z"
    dx <- Z.free "stone_dx"
    dy <- Z.free "stone_dy"
    dz <- Z.free "stone_dz"
    return $ ((x,y,z), (dx,dy,dz))

buildHail ((rx,ry,rz),(rdx,rdy,rdz)) i ((x,y,z), (dx,dy,dz)) = do
    [x,y,z,dx,dy,dz] <- mapM (uncurry makeVar) [("x",x),("y",y),("z",z),("dx",dx),("dy",dy),("dz",dz)]
    t <- Z.free $ "hail_" ++ show i ++ "t"
    Z.constrain $ t Z..> 0
    Z.constrain $ x + t*dx Z..== rx + t*rdx
    Z.constrain $ y + t*dy Z..== ry + t*rdy
    Z.constrain $ z + t*dz Z..== rz + t*rdz
    return ()

    where name s = "hail_" ++ s
          makeVar s val = do
            var <- Z.sInt64 (name s)
            Z.constrain $ var Z..== (fromIntegral val)
            return var

buildModel :: [(Point,Point)] -> Z.ConstraintSet
buildModel input = do
    rock@((rx,ry,rz),_) <- buildRock
    sequence_ $ zipWith (buildHail rock) [1..] input

extract :: String -> Z.SatResult -> Integer
extract s = fromJust . Z.getModelValue @Z.SatResult @Integer s

main :: IO ()
main = do
    input <- fmap (map (runParser (parseHail <* eof)) . lines) getContents

    -- we have 6 unknowns to solve for
    -- each input provides 3 contraining equations; but also add a new t
    -- using 3 inputs turned out not to be enough, but it worked with 4
    res <- Z.sat $ buildModel $ take 4 input

    print $ sum $ map ((flip extract res) . ("stone_" ++) . (:[])) "xyz"
