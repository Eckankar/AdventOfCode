import qualified Data.Set as S

getInput :: IO [Integer]
getInput = do c <- readFile "input.txt"
              return $ map (read . removePlus) $ lines c
    where removePlus ('+':s) = s
          removePlus s       = s

findFirstDupe :: (Ord a, Eq a) => [a] -> a
findFirstDupe = findFirstDupe' S.empty
    where findFirstDupe' seen (n:ns) =
            if   S.member n seen
            then n
            else findFirstDupe' (S.insert n seen) ns

main = do ns <- getInput

          let part1 = sum ns
          putStrLn $ "Part 1: " ++ show part1

          let part2 = findFirstDupe $ scanl (+) 0 $ cycle ns
          putStrLn $ "Part 2: " ++ show part2
