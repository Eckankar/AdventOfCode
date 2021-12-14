import qualified Data.Map as M

increaseBy m Nothing  = Just m
increaseBy m (Just n) = Just $ n+m

elementCounts s = foldr (M.alter (increaseBy 1)) M.empty s

applyRules rs ps = M.foldrWithKey apply M.empty ps
    where apply (a, b) v m = M.alter (increaseBy v) (a, c) $ M.alter (increaseBy v) (c, b) m
            where c = rs M.! (a, b)

parseRule (a:b:' ':'-':'>':' ':c:[]) = ((a,b), c)
parseRule _ = error "Invalid rule"

main :: IO ()
main = do
    startingPolymer <- getLine
    _ <- getLine
    rules <- fmap (M.fromList . map parseRule . lines) getContents
    let startingPolymerMap = elementCounts $ zip startingPolymer $ tail startingPolymer
    let polymerSteps = iterate (applyRules rules) startingPolymerMap
    let counts = M.toList $ M.alter (increaseBy 1) (head startingPolymer) $ M.mapKeysWith (+) snd $ polymerSteps !! 40
    let counts' = map snd counts
    print $ maximum counts' - minimum counts'
