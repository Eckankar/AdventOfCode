import Control.Arrow (second)
import qualified Data.Set as S

parseCoord :: String -> (Int, Int)
parseCoord s = (read a, read b)
    where (a, _:b) = break (== ',') s

data Fold = FoldX Int | FoldY Int
    deriving (Show, Eq)

parseFold :: String -> Fold
parseFold s = parse' $ words s !! 2
    where parse' ('x':'=':v) = FoldX $ read v
          parse' ('y':'=':v) = FoldY $ read v
          parse' _ = error "Invalid fold instruction."

applyFold :: S.Set (Int, Int) -> Fold -> S.Set (Int, Int)
applyFold s f = S.map (foldBy f) s
    where foldBy (FoldX n) (x, y) | x > n = (2*n - x, y)
          foldBy (FoldY n) (x, y) | y > n = (x, 2*n - y)
          foldBy _         (x, y)         = (x, y)

drawPoints :: S.Set (Int, Int) -> IO ()
drawPoints s =
    putStrLn $ unlines [ [ if (x, y) `S.member` s then '#' else '.' | x <- [0 .. w] ] | y <- [0 .. h] ]
    where w = maximum $ S.map fst s
          h = maximum $ S.map snd s

main :: IO ()
main = do
    (coordInput, _:foldInput) <- fmap (break (== "") . lines) getContents
    let initialSet = S.fromList $ map parseCoord coordInput
    let folds = map parseFold foldInput
    let finalPoints = foldl applyFold initialSet folds
    drawPoints finalPoints
