import Data.Char (toLower)

reducePolymer :: String -> String
reducePolymer ps = if changed then reducePolymer ps' else ps
    where (ps', changed) = reducePolymer' ps ([], False)

          reducePolymer' :: String -> (String, Bool) -> (String, Bool)
          reducePolymer' (p:p':ps) (ps', b) =
            if p /= p' && (p == toLower p' || p' == toLower p)
            then reducePolymer' ps (ps', True)
            else reducePolymer' (p':ps) (p:ps', b)
          reducePolymer' [p] (ps', b) = reducePolymer' [] (p:ps', b)
          reducePolymer' [] (ps', b) = (reverse ps', b)

main :: IO ()
main = do
    input <- fmap (head . lines) getContents

    let reducedPolymer = reducePolymer input
    print $ length reducedPolymer
