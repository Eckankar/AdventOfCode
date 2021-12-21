import qualified Data.Map as M
import qualified Data.PQueue.Prio.Min as P
import qualified Data.Set as S

import Data.List.Extra (minimumOn)

data Player = PlayerOne | PlayerTwo
    deriving (Ord, Eq, Show)

baseRolls = [ a+b+c | a <- [1..3], b <- [1..3], c <- [1..3] ]

playGame gs (wa, wb)
    | M.null gs = (wa, wb)
    | a >= 21   = playGame gs'  (wa + s, wb)
    | b >= 21   = playGame gs'  (wa, wb + s)
    | otherwise = playGame gs'' (wa, wb)
    where (a, b) = minimumOn makePrio $ M.keys gs
          m      = gs M.! (a, b)
          gs'    = M.delete (a, b) gs
          s      = sum $ M.elems m

          gs''  = foldl (\gsa (k, v) -> M.alter (addInnerValue v) k gsa) gs' rolls
          rolls = concatMap (uncurry roll) $ M.toList m

          roll (pa, pb, PlayerOne) c = map (\i -> let pa' = add pa i in ((a + pa' + 1, b), ((pa', pb, PlayerTwo), c))) baseRolls
          roll (pa, pb, PlayerTwo) c = map (\i -> let pb' = add pb i in ((a, b + pb' + 1), ((pa, pb', PlayerOne), c))) baseRolls

          makePrio (a, b) | a <= b = (a, b)
                          | a >  b = (b, a)

          add a b = (a + b) `mod` 10

          addInnerValue (k, c) Nothing   = Just $ M.singleton k c
          addInnerValue (k, c) (Just pm) = Just $ M.insertWith (+) k c pm

main :: IO ()
main = do
    [a, b] <- fmap (map (read . (!! 4) . words) . lines) getContents
    let initialGame = M.fromList [ ( (0, 0), M.fromList [ ((a-1, b-1, PlayerOne), 1) ] ) ]
    print $ uncurry max $ playGame initialGame (0, 0)
