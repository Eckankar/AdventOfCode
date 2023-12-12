import qualified Data.Map as M

import Data.List (intercalate)
import Data.List.Extra (wordsBy)

data Field = Unknown | Empty | Marked
    deriving (Eq)

instance Show Field where
    show Unknown = "?"
    show Empty   = "."
    show Marked  = "#"

    showList fs s = "[" ++ concatMap show fs ++ "]" ++ s

toField '?' = Unknown
toField '.' = Empty
toField '#' = Marked

parseLine :: String -> ([Field], [Int])
parseLine s = (map toField line, map read $ wordsBy (== ',') hints)
    where [line, hints] = words s

extend :: Int -> ([Field], [Int]) -> ([Field], [Int])
extend n (line, hints) = (intercalate [Unknown] $ replicate n line, concat $ replicate n hints)

increaseBy m Nothing  = Just m
increaseBy m (Just n) = Just $ n+m

countCompatiblePlacements :: [Field] -> [Int] -> Int
countCompatiblePlacements line hints = sum $ M.elems $ M.filterWithKey isFinal countMap
    where countMap :: M.Map ([Int], Int) Int
          countMap = foldl countCompatible initialState line
          initialState = M.singleton (hints, 0) 1

          isFinal ([], 0) _     = True
          isFinal ([h], curH) _ = h == curH
          isFinal _ _           = False

          countCompatible :: M.Map ([Int], Int) Int -> Field -> M.Map ([Int], Int) Int
          countCompatible m l = M.foldrWithKey (countCompatible' l) M.empty m

          countCompatible' :: Field -> ([Int], Int) -> Int -> M.Map ([Int], Int) Int -> M.Map ([Int], Int) Int
          countCompatible' Marked ([], _) _ m = m
          countCompatible' _      ([], _) v m = M.alter (increaseBy v) ([], 0) m
          countCompatible' Empty (h:hs, curH) v m
                | curH == h = M.alter (increaseBy v) (hs, 0) m
                | curH > 0  = m
                | otherwise = M.alter (increaseBy v) (h:hs, 0) m
          countCompatible' Marked (h:hs, curH) v m
                | curH == h = m
                | otherwise = M.alter (increaseBy v) (h:hs, curH+1) m
          countCompatible' Unknown (hs, curH) v m =
                countCompatible' Empty  (hs, curH) v $
                countCompatible' Marked (hs, curH) v m


main :: IO ()
main = do
    input <- fmap (map (extend 5 . parseLine) . lines) getContents
    print $ sum $ map (uncurry countCompatiblePlacements) input
