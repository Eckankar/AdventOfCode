import qualified Data.Map as M

elementCounts s = foldr (M.alter increase) M.empty s
    where increase Nothing  = Just 1
          increase (Just n) = Just $ n+1

applyRules rs s = head s : foldr (\(a,b) acc -> (rs M.! (a,b)) : b : acc) "" pairs
    where pairs = zip s (tail s)

parseRule (a:b:' ':'-':'>':' ':c:[]) = ((a,b), c)
parseRule _ = error "Invalid rule"

main :: IO ()
main = do
    startingPolymer <- getLine
    _ <- getLine
    rules <- fmap (M.fromList . map parseRule . lines) getContents
    let polymerSteps = iterate (applyRules rules) startingPolymer
    let counts = map snd $ M.toList $ elementCounts $ polymerSteps !! 10
    print $ maximum counts - minimum counts
