data RPS = Rock | Paper | Scissors
    deriving (Eq, Show)

parseRPS :: Char -> Char -> Char -> Char -> RPS
parseRPS r p s v | v == r = Rock
                 | v == p = Paper
                 | v == s = Scissors

parseLine :: String -> (RPS, RPS)
parseLine [opp, ' ', you] = (parseRPS 'A' 'B' 'C' opp, parseRPS 'X' 'Y' 'Z' you)

scoreChoice :: RPS -> Int
scoreChoice Rock     = 1
scoreChoice Paper    = 2
scoreChoice Scissors = 3

scoreOutcome :: RPS -> RPS -> Int
scoreOutcome Rock Scissors   = 6
scoreOutcome Scissors Paper  = 6
scoreOutcome Paper Rock      = 6
scoreOutcome x y | x == y    = 3
                 | otherwise = 0

scoreMatch :: RPS -> RPS -> Int
scoreMatch opp you = scoreChoice you + scoreOutcome you opp

main :: IO ()
main = do
    input <- fmap (map parseLine . lines) getContents
    print $ sum $ map (uncurry scoreMatch) input
