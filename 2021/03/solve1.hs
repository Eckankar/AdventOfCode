import Data.List (transpose)

mostCommon = select . foldl counter (0, 0)
    where counter (z, o) '0' = (z+1, o)
          counter (z, o) '1' = (z, o+1)
          select (z, o) = if z > o then '0' else '1'

dFlip '0' = '1'
dFlip '1' = '0'

fromBinary :: String -> Int
fromBinary = foldl (\a d -> 2 * a + read [d]) 0

main :: IO ()
main = do
    input <- fmap lines getContents
    let gammaRate = map mostCommon $ transpose input
    let epsilonRate = map dFlip gammaRate
    print $ (fromBinary gammaRate) * (fromBinary epsilonRate)
