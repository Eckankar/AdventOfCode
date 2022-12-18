import qualified Data.Set as S
import Data.List.Split (splitOn)

type Point = (Int, Int, Int)

parseLine :: [Int] -> Point
parseLine [x, y, z] = (x, y, z)

neighbors (x, y, z) = [
        (x+1, y, z), (x-1, y, z),
        (x, y+1, z), (x, y-1, z),
        (x, y, z+1), (x, y, z-1)
    ]

main :: IO ()
main = do
    input <- fmap (map (parseLine . map read . splitOn ",") . lines) getContents
    let pointSet = S.fromList input
    print $ sum $ map (S.size . flip S.difference pointSet . S.fromList . neighbors) input
