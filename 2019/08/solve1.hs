import Data.List (sortOn)
import Data.List.Split (chunksOf)

bInt False = 0
bInt True  = 1

main = do
    let (w, h) = (25, 6)
    input <- fmap (chunksOf (w*h) . map (read . (:[]))) getLine

    let (layer:_) = sortOn ((* (-1)) . length . filter (/= 0)) input
    let (oneCount, twoCount) = foldl (\(o, t) n -> (o + bInt (n == 1), t + bInt (n == 2))) (0, 0) layer
    print $ oneCount * twoCount
