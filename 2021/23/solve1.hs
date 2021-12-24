{-# LANGUAGE TupleSections #-}

import qualified Data.Array as A
import qualified Data.Map as M
import qualified Data.PQueue.Prio.Min as P
import qualified Data.Set as S

import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import GHC.Exts (groupWith)

import Debug.Trace (traceShow)

data Amphipod = Amber | Bronze | Copper | Desert
    deriving (Show, Ord, Eq)

data Cell = Cell CellType (Maybe Amphipod)
    deriving (Show, Eq)

data CellType = Sideroom Amphipod | Hallway | Exit
    deriving (Show, Eq)

newtype Display = Display (M.Map (Int, Int) Cell)

instance (Show Display) where
    show (Display m) = ("\n" ++ ) $ (++ "\n") $ intercalate "\n" $ map (concatMap ((:[]) . snd)) $ groupWith (\((a,_),_) -> a) $ M.toList m'
        where m' = M.union (M.map showCell m) emptyMap
              showCell (Cell _ Nothing) = '.'
              showCell (Cell _ (Just amphi)) = showAmbi amphi
              showAmbi Amber  = 'A'
              showAmbi Bronze = 'B'
              showAmbi Copper = 'C'
              showAmbi Desert = 'D'

emptyMap = M.fromList $ A.assocs $ A.listArray ((0, 0), (height-1, width-1)) $ concat m
    where m = [
            "#############",
            "#...........#",
            "###.#.#.#.###",
            "  #.#.#.#.#  ",
            "  #########  "
            ]
          (height, width) = (length m, length $ head m)

parseAmphipod 'A' = Amber
parseAmphipod 'B' = Bronze
parseAmphipod 'C' = Copper
parseAmphipod 'D' = Desert
parseAmphipod _   = error "Invalid amphipod"

amphipodCost Amber  = 1
amphipodCost Bronze = 10
amphipodCost Copper = 100
amphipodCost Desert = 1000

homeCols = [(Amber, 3), (Bronze, 5), (Copper, 7), (Desert, 9)]

parseInput :: IO (M.Map (Int, Int) Cell)
parseInput = do
    getLine
    getLine
    [_,_,_,a11,_,a12,_,a13,_,a14,_,_,_] <- getLine
    [_,_,_,a21,_,a22,_,a23,_,a24,_,_,_] <- getLine
    let amphipods = map parseAmphipod [a11,a12,a13,a14,a21,a22,a23,a24]
    let initialRooms = map makeRoom $ zip3 amphipods roomOwners roomCoords
    return $ M.fromList $ hallway ++ initialRooms
        where exits = S.fromList [3,5,7,9]
              hallway = [ ((1,x), Cell (cellType x) Nothing) | x <- [1..11] ]
              cellType x | x `S.member` exits = Exit
                         | otherwise          = Hallway
              roomOwners = cycle [Amber, Bronze, Copper, Desert]
              roomCoords = [(2,3),(2,5),(2,7),(2,9),(3,3),(3,5),(3,7),(3,9)]
              makeRoom (inhabitant, owner, coord) = (coord, Cell (Sideroom owner) $ Just inhabitant)

neighbors (x,y) = [ (x-1,y), (x+1,y), (x,y-1), (x,y+1) ]

reachable :: (Int, Int) -> M.Map (Int, Int) Cell -> [((Int, Int), Int)]
reachable sCoord m = reachable' initQueue initSeen
    where initSeen = S.singleton sCoord
          initQueue = map (,1) $ neighbors sCoord
          Cell sType (Just sAmphi) = m M.! sCoord

          reachable' []          seen = []
          reachable' ((c, d):cs) seen
            | c `S.member` seen = reachable' cs seen
            | otherwise         =
                case M.lookup c m of
                    Nothing                     -> reachable' cs seen'
                    Just (Cell _ (Just _))      -> reachable' cs seen'
                    Just (Cell Exit Nothing)    -> reachable' cs' seen'
                    Just (Cell Hallway Nothing)
                        | sType == Hallway -> reachable' cs' seen'
                        | otherwise        -> (c, d) : reachable' cs' seen'
                    Just (Cell (Sideroom tAmphi) Nothing)
                        | tAmphi /= sAmphi                                     -> reachable' cs' seen'
                        | not $ M.null $ M.filterWithKey (wrongRoomsBelow c) m -> reachable' cs' seen'
                        | otherwise                                            -> (c, d) : reachable' cs' seen'

            where seen' = S.insert c seen
                  cs' = cs ++ map (,d+1) (neighbors c)
                  wrongRoomsBelow (y, x) (y', x') cell = x == x' && y' > y && checkCell cell
                  checkCell (Cell (Sideroom a) (Just a')) = a /= a'
                  checkCell _ = True

amphipodPositions = S.fromList . M.foldlWithKey extract []
    where extract acc k (Cell _ (Just v)) = (v, k) : acc
          extract acc _ _                 = acc

isSolved = M.foldlWithKey verify True
    where verify acc k (Cell (Sideroom v) (Just v')) = acc && v == v'
          verify acc k (Cell _ (Just _))             = False
          verify acc _ _                             = acc

activePositions = removeInactive . amphipodPositions
    where removeWhile []     s = s
          removeWhile (e:es) s | e `S.member` s = removeWhile es $ S.delete e s
                               | otherwise      = s

          removeGrps = [ [ (a, (x, y)) | x <- [3, 2] ] | (a, y) <- homeCols ]

          removeInactive s = foldr removeWhile s removeGrps

makeMove m p (q, d) = (d * amphipodCost pAmphi, m')
    where Cell _ (Just pAmphi) = m M.! p
          m' = M.adjust (\(Cell qType _) -> Cell qType (Just pAmphi)) q $
               M.adjust (\(Cell pType _) -> Cell pType Nothing) p m

numActive = length . activePositions

solve pq seen | isSolved m = (cost, t)
              | otherwise  = solve pq' seen'
    where ((cost, (m, t)), pq'') = P.deleteFindMin pq
          moves = [
            (cost + cost', (m', (cost + cost', Display m') : t))
                | p <- S.toList $ S.map snd $ activePositions m,
                  q <- reachable p m,
                  let (cost', m') = makeMove m p q,
                  cheaper (cost + cost') $ M.lookup (amphipodPositions m') seen
            ]

          pq' = P.union pq'' $ P.fromList moves
          seen' = M.union (M.fromList $ map (\(v, (m,_)) -> (amphipodPositions m, v)) moves) seen

          cheaper n Nothing  = True
          cheaper n (Just m) = n < m

checkMoveChain m s [] = ["done"]
checkMoveChain m s ((p,q):ps) =
    (show p ++ "->" ++ show q ++ ": " ++ rtext ++ "; score: " ++ show s' ++ "(" ++ show (s+s') ++ ") " ++ show rs) : checkMoveChain m' (s+s') ps
        where (s',m') = makeMove m p (q, fromMaybe 0 $ lookup q rs)
              rs = reachable p m
              rtext = if q `elem` map fst rs then "reachable" else "unreachable"

main :: IO ()
main = do
    input <- parseInput
--    putStrLn $ unlines $ checkMoveChain input 0 $ [ ((2,7), (1,4)), ((2,5),(2,7)), ((3,5),(1,6)), ((1,4),(3,5)), ((2,3),(2,5)), ((2,9),(1,8)), ((3,9),(1,10)), ((1,8),(3,9)), ((1,6),(2,9)), ((1,10),(2,3)) ]
    let initialPQ = P.singleton 0 (input, [(0, Display input)])
    let initialSeen = M.singleton (amphipodPositions input) 0
    print $ fst $ solve initialPQ initialSeen
