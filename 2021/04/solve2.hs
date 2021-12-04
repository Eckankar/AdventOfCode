import Data.List (intercalate, partition, transpose)
import Debug.Trace (traceShowId)

data Bingo = Unmarked Int | Marked Int
    deriving (Eq)
data BingoBoard = Board [[Bingo]]

padNum n = (if n < 10 then " " else "") ++ show n
instance Show Bingo where
    show (Unmarked v) = " " ++ padNum v ++ " "
    show (Marked v) = "[" ++ padNum v ++ "]"

instance Show BingoBoard where
    show (Board bs) = "\n" ++ (intercalate "\n" $ map (intercalate " " . map show) bs) ++ "\n"

readBoards :: IO [BingoBoard]
readBoards = do
    boards <- fmap (map (map read . words) . lines) getContents
    let (bs, b) = foldl builder ([], []) boards
    return $ map Board $ b : bs
        where builder (bs, b) [] = (b : bs, [])
              builder (bs, b) rs = (bs, map Unmarked rs : b)

replace from to = map (\e -> if e == from then to else e)

simulate bs (n:ns) = if null unsolvedBoards then n * score else simulate unsolvedBoards ns
    where bs' = map placeNumber bs
          placeNumber (Board b) = Board $ map (replace (Unmarked n) (Marked n)) b
          solved (Board b) = any (all isMarked) $ b ++ transpose b
          (solvedBoards, unsolvedBoards) = partition solved bs'

          (Board winner) : _ = solvedBoards
          score = sum $ map deBingo $ concatMap (filter (not . isMarked)) winner

          isMarked (Marked _) = True
          isMarked _ = False

          deBingo (Marked v) = v
          deBingo (Unmarked v) = v

main :: IO ()
main = do
    bingoNums <- fmap (map read . words . replace ',' ' ') getLine
    _ <- getLine
    boards <- readBoards
    print $ simulate boards bingoNums
