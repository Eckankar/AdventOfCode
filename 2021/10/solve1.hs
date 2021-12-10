import Data.Maybe (isJust, fromJust)

data SyntaxError = Corrupted Char | Missing [Char]
    deriving Show

closer '(' = Just ')'
closer '[' = Just ']'
closer '{' = Just '}'
closer '<' = Just '>'
closer _   = Nothing

isOpener = isJust . closer

syntaxCheck [] []                      = error "Data isn't supposed to be valid, silly."
syntaxCheck qs []                      = Missing qs
syntaxCheck qs (c:cs)     | isOpener c = syntaxCheck (fromJust (closer c) : qs) cs
syntaxCheck (q:qs) (c:cs) | q == c     = syntaxCheck qs cs
syntaxCheck (q:qs) (c:cs) | otherwise  = Corrupted c

scoreCorrupted ')' = 3
scoreCorrupted ']' = 57
scoreCorrupted '}' = 1197
scoreCorrupted '>' = 25137

score (Missing _) = 0
score (Corrupted c) = scoreCorrupted c

main :: IO ()
main = do
    inputs <- fmap lines getContents
    print $ foldl (+) 0 $ map (score . syntaxCheck []) inputs
