import qualified Data.Map as M
import Data.List (tails)

getInput :: IO [String]
getInput = do c <- readFile "input.txt"
              return $ lines c

countLetters :: String -> M.Map Char Integer
countLetters =
    foldl (\m c -> M.insert c (M.findWithDefault 0 c m + 1) m) M.empty

checkSum :: [M.Map Char Integer] -> Int
checkSum ms = findWithCount 2 * findWithCount 3
    where findWithCount n = length $ filter (\m -> M.filter (== n) m /= M.empty) ms

add (a,b) (a', b') = (a+a', b++b')

similar :: String -> String -> (Int, String)
similar (a:as) (b:bs) | a == b = add (0, [b]) $ similar as bs
                      | a /= b = add (1, "")  $ similar as bs
similar [] [] = (0, "")

findSimilar :: [String] -> String
findSimilar ps = simStr
    where sims = concatMap (\(t:ts) -> map (similar t) ts) $ tails ps
          ((_,simStr):_) = filter ((==1) . fst) sims

main = do ps <- getInput
          let cs = map countLetters ps

          let part1 = checkSum cs
          print part1

          let part2 = findSimilar ps
          putStrLn part2
