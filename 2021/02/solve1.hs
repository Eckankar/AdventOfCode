convert :: [String] -> (Int, Int)
convert ["forward", n] = (read n, 0)
convert ["down", n]    = (0, read n)
convert ["up", n]      = (0, -read n)

add (a, b) (c, d) = (a+c, b+d)

main :: IO ()
main = do
    input <- fmap (map words . lines) getContents
    let (horz, vert) = foldl add (0, 0) $ map convert input
    print $ horz * vert
