import qualified Data.Set as S

import Data.List (sort)

resolveOrder es s = if S.null candidates then [] else chosen : resolveOrder es s'
    where all = S.difference (S.fromList $ concatMap (\(a, b) -> [a, b]) es) s
          candidates = foldr (\(f, t) cs -> if S.member f s then cs else S.delete t cs) all es
          chosen = head $ sort $ S.toList candidates
          s' = S.insert chosen s


main :: IO ()
main = do
    input <- fmap (map ((\s -> (s !! 1, s !! 7)) . map head . words) . lines) getContents
    putStrLn $ resolveOrder input S.empty
