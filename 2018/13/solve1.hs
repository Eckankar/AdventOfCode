import qualified Data.Map as M
import qualified Data.Set as S

import Data.List (sortOn)
import Data.Tuple.Extra (fst3)

data Direction = DLeft | DStraight | DRight
    deriving (Eq, Show)

type Point = (Int, Int)

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [0..] r) . zip [0..]

add (x, y) (x', y') = (x+x', y+y')

findCarts m = (map addTurns $ carts, m')
    where carts = M.toList $ M.map toVector cartM
          m' = foldl (\m (p,v) -> M.insert p (toSymb v) m) m carts

          cartM = M.filter (`elem` "<>^v") m
          toVector '<' = ( 0, -1)
          toVector '>' = ( 0,  1)
          toVector '^' = (-1,  0)
          toVector 'v' = ( 1,  0)
          toSymb (x, y) = if x /= 0 then '|' else '-'

          addTurns (p, v) = (p, v, cycle [DLeft, DStraight, DRight])

update :: M.Map Point Char -> [(Point, Point, [Direction])] -> [[(Point, Point, [Direction])]]
update m cs = steps ++ update m cs'
    where (cs', steps) = updateEach (sortOn fst3 cs) [] []
          updateEach []      pcs steps = (pcs, steps)
          updateEach (c:cs') pcs steps = updateEach cs' (c':pcs) (steps ++ [c':pcs])
            where c' = update' c

          update' (p, v, t) = (p', v', t')
            where p' = add p v
                  (v', t') = updateV s v t
                  s  = m M.! p'

          updateV '/'  (x, y) ts             = ((-y, -x), ts)
          updateV '\\' (x, y) ts             = (( y,  x), ts)
          updateV '+'  (x, y) (DLeft:ts)     = ((-y,  x), ts)
          updateV '+'  (x, y) (DStraight:ts) = (( x,  y), ts)
          updateV '+'  (x, y) (DRight:ts)    = (( y, -x), ts)
          updateV _    v      ts             = (v, ts)

noCrash cs = length csps == S.size (S.fromList csps)
    where csps = map fst3 cs

findCrash :: [(Point, Point, [Direction])] -> Point
findCrash cs = p
    where (_, Just p) = foldl checkCart (S.empty, Nothing) cs
          checkCart (seen, mv) (p, _, _) = (seen', mv')
            where seen' = S.insert p seen
                  mv' = if S.member p seen then Just p else mv

main :: IO ()
main = do
    input <- fmap (toMap . lines) getContents

    let (carts, input') = findCarts input

    let (firstCrash:_) = dropWhile noCrash $ update input' carts
    let (y, x) = findCrash firstCrash
    putStrLn $ show x ++ "," ++ show y

