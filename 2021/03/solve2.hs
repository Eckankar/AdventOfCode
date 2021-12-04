mostCommon = select . foldl counter (0, 0)
    where counter (z, o) '0' = (z+1, o)
          counter (z, o) '1' = (z, o+1)
          select (z, o) = if z > o then '0' else '1'

dFlip '0' = '1'
dFlip '1' = '0'

fromBinary :: String -> Int
fromBinary = foldl (\a d -> 2 * a + read [d]) 0

prefixMatch :: (Char -> Char) -> String -> [String] -> String
prefixMatch t us [v] = reverse us ++ v
prefixMatch t us vs = prefixMatch t (c:us) vs'
    where c   = t $ mostCommon $ map head vs
          vs' = map tail $ filter ((== c) . head) vs

main :: IO ()
main = do
    input <- fmap lines getContents
    let oxygenRate = prefixMatch id   [] input
    let co2Rate    = prefixMatch dFlip [] input
    print $ (fromBinary oxygenRate) * (fromBinary co2Rate)
