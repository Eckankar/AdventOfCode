data RPS = Rock | Paper | Scissors
    deriving (Eq, Show)

data LDW = Loss | Draw | Win
    deriving (Eq, Show)

parseRPS :: Char -> Char -> Char -> Char -> RPS
parseRPS r p s v | v == r = Rock
                 | v == p = Paper
                 | v == s = Scissors

parseLDW :: Char -> Char -> Char -> Char -> LDW
parseLDW l d w v | v == l = Loss
                 | v == d = Draw
                 | v == w = Win

parseLine :: String -> (RPS, LDW)
parseLine [opp, ' ', you] = (parseRPS 'A' 'B' 'C' opp, parseLDW 'X' 'Y' 'Z' you)

scoreChoice :: RPS -> Int
scoreChoice Rock     = 1
scoreChoice Paper    = 2
scoreChoice Scissors = 3

determineGame :: RPS -> RPS -> LDW
determineGame Rock Scissors   = Win
determineGame Scissors Paper  = Win
determineGame Paper Rock      = Win
determineGame x y | x == y    = Draw
                  | otherwise = Loss

scoreResult :: LDW -> Int
scoreResult Win  = 6
scoreResult Draw = 3
scoreResult Loss = 0

scoreMatch :: RPS -> LDW -> Int
scoreMatch opp res = scoreChoice you + scoreResult (determineGame you opp)
    where [you] = filter (\v -> determineGame v opp == res) [Rock, Paper, Scissors]

main :: IO ()
main = do
    input <- fmap (map parseLine . lines) getContents
    print $ sum $ map (uncurry scoreMatch) input
