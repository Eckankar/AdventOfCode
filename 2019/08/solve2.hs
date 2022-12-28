import Data.List (transpose)
import Data.List.Split (chunksOf)

shade '0' = ' '
shade '1' = '#'

main = do
    let (w, h) = (25, 6)
    input <- fmap (chunksOf (w*h)) getLine

    let img = chunksOf w $ map (shade . head . dropWhile (== '2')) $ transpose input
    mapM_ putStrLn img
