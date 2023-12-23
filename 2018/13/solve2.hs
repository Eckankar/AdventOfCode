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
    where ps = S.fromList $ map fst3 cs
          (cs', steps) = updateEach (sortOn fst3 cs) [] [] ps
          updateEach []      pcs steps ps = (pcs, steps)
          updateEach (c:cs') pcs steps ps =
            if S.member p' ps
            then let remCrash = filter ((/= p') . fst3)
                     cs'' = remCrash cs'
                     pcs' = remCrash pcs
                     ps' = S.delete p' $ S.delete (fst3 c) ps
                 in updateEach cs'' pcs' (steps ++ [cs'' ++ pcs']) ps'
            else let ps' = S.insert p' $ S.delete (fst3 c) ps
                 in updateEach cs' (c':pcs) (steps ++ [cs ++ (c':pcs)]) ps'
            where c'@(p',_,_) = update' c

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

main :: IO ()
main = do
    input <- fmap (toMap . lines) getContents

    let (carts, input') = findCarts input
    let (y, x) = fst3 $ head $ head $ dropWhile (not . null . tail) $ update input' carts

    putStrLn $ show x ++ "," ++ show y

