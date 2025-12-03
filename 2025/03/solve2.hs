import Data.Function (on)
import Data.List (maximumBy)

digitize :: String -> [Int]
digitize s = map (read . (:[])) s

fromDigits = foldl1 (\a d -> a*10+d)

removeOne i ls = take i ls ++ drop (i+1) ls

findLargestJoltage ns = snd $ foldl update (initialDs, fromDigits initialDs) $ drop 12 ns
        where initialDs = take 12 ns
              update :: ([Int], Int) -> Int -> ([Int], Int)
              update (bestDs, s) d = maximumBy (compare `on` snd) $ (bestDs, s) : candidateDs
                where candidateDs = [ (newDs, fromDigits newDs) | i <- [0 .. length bestDs-1], let newDs = removeOne i bestDs ++ [d] ]

main = do
    input <- fmap (map digitize . lines) getContents
    
    print $ sum $ map findLargestJoltage input
