import qualified Deque.Lazy as DQ

readInt :: String -> Int
readInt = read

playGame :: DQ.Deque Int -> DQ.Deque Int -> Int -> Int -> Int
playGame board players currentMarble lastMarble
    | currentMarble > lastMarble = maximum players
    | currentMarble `mod` 23 == 0 =
        let Just (extraMarble, board') = DQ.uncons $ (!! 7) $ iterate DQ.shiftRight board
            Just (playerScore, players'') = DQ.uncons players
            players' = DQ.cons (playerScore + currentMarble + extraMarble) players''
        in playGame board' (DQ.shiftLeft players') (currentMarble+1) lastMarble
    | otherwise = playGame board' (DQ.shiftLeft players) (currentMarble+1) lastMarble
        where board' = DQ.cons currentMarble $ DQ.shiftLeft $ DQ.shiftLeft board

main :: IO ()
main = do
    (nPlayers, lastMarble) <- fmap ((\s -> (readInt $ s !! 0, readInt $ s !! 6)) . words) getContents

    print $ playGame (pure 0) (DQ.fromConsAndSnocLists (replicate nPlayers 0) []) 1 $ 100 * lastMarble

