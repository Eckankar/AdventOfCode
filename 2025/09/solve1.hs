
main :: IO ()
main = do
    redTiles <- fmap (map ((read :: String -> (Int, Int)) . ("(" ++) . (++ ")")) . lines) getContents

    print $ maximum [ (abs (x-x')+1) * (abs (y-y')+1) | (x, y) <- redTiles, (x', y') <- redTiles, (x,y) < (x', y')]