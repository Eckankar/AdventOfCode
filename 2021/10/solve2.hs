import Data.List (sort)
import Data.Maybe (catMaybes, isJust, fromJust)

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

score (Corrupted _) = Nothing
score (Missing m)   = Just $ foldl update 0 m
    where update s c = 5 * s + scoreChar c

          scoreChar ')' = 1
          scoreChar ']' = 2
          scoreChar '}' = 3
          scoreChar '>' = 4

main :: IO ()
main = do
    inputs <- fmap lines getContents
    let missing = sort $ catMaybes $ map (score . syntaxCheck []) inputs
    print $ missing !! ((length missing - 1) `div` 2)
