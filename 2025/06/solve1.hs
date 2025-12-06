import Data.List (transpose)

calcRow (sop:sns) = foldl1 (toOp sop) ns
    where ns = map (read :: String -> Int) sns
          toOp "+" = (+)
          toOp "*" = (*)

main :: IO ()
main = do
    input <- fmap (map (\r -> last r : init r) . transpose . map words . lines) getContents
    print $ sum $ map calcRow input