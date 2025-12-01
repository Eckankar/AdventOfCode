parseInput :: String -> (Char, Int)
parseInput (c:ns) = (c, read ns)

applyTurn :: (Int, Int) -> (Char, Int) -> (Int, Int)
applyTurn (_, v) (md, t) = (rs, t')
    where (rs', t') = applyTurn' (md, t)
          rs'' = if v  == 0 && md == 'L' then abs rs' - 1 else abs rs'
          rs   = if t' == 0 && md == 'L' then rs'' + 1 else rs''

          applyTurn' :: (Char, Int) -> (Int, Int)
          applyTurn' ('L', t) = (v - t) `divMod` 100
          applyTurn' ('R', t) = (v + t) `divMod` 100


main :: IO ()
main = do
    inputMoves <- fmap (map parseInput . lines) getContents
    
    let intermediates = scanl applyTurn (0, 50) inputMoves
    print $ sum $ map fst intermediates