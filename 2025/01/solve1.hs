parseInput :: String -> (Char, Int)
parseInput (c:ns) = (c, read ns)

applyTurn :: Int -> (Char, Int) -> Int
applyTurn v ('L', t) = (v - t) `mod` 100
applyTurn v ('R', t) = (v + t) `mod` 100

main :: IO ()
main = do
    inputMoves <- fmap (map parseInput . lines) getContents
    
    let intermediates = scanl applyTurn 50 inputMoves
    print $ length $ filter (== 0) intermediates