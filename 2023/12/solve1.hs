import Data.List.Extra (wordsBy)

-- Bits yoinked from https://github.com/Eckankar/picross-solver

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

-- What are all possible ways we can place the given hints in a row of size n?
allPossiblePlacements :: [Int] -> Int -> [[Field]]
allPossiblePlacements [] n = [ replicate n Empty ]
allPossiblePlacements (h:hs) n
    | h > n             = []
    | h == n && null hs = [ replicate h Marked ]
    | otherwise         =  map (Empty :) (allPossiblePlacements (h:hs) (n-1)) ++
                           map ((replicate h Marked ++ [Empty]) ++) (allPossiblePlacements hs (n-h-1))

-- Are two rows compatible with each other?
isCompatiblePlacement :: [Field] -> [Field] -> Bool
isCompatiblePlacement a b = and $ zipWith (\x y -> x == y || x == Unknown || y == Unknown) a b

parseLine :: String -> ([Field], [Int])
parseLine s = (map toField line, map read $ wordsBy (== ',') hints)
    where [line, hints] = words s

compatiblePlacements line hints = filter (isCompatiblePlacement line) allPlacements
    where allPlacements = allPossiblePlacements hints (length line)

main :: IO ()
main = do
    input <- fmap (map parseLine . lines) getContents
    print $ sum $ map (length . uncurry compatiblePlacements) input
