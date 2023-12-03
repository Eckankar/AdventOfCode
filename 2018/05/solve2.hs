import qualified Data.Set as S

import Data.Char (toLower, toUpper)

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

removeUnit u ps = filter (\c -> c /= u && c /= u') ps
    where u' = toUpper u

main :: IO ()
main = do
    input <- fmap (head . lines) getContents

    let unitTypes = S.toList $ S.fromList $ map toLower input

    let reducedPolymers = map (length . reducePolymer . flip removeUnit input) unitTypes
    print $ minimum reducedPolymers
