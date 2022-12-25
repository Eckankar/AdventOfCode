parseSnafu :: String -> Int
parseSnafu = foldl (\a n -> a*5+n) 0 . map deSnafu
    where deSnafu '-' = -1
          deSnafu '=' = -2
          deSnafu n = read [n]

encodeSnafu :: Int -> String
encodeSnafu 0 = "0"
encodeSnafu n = reverse $ encode n
    where encode 0 = []
          encode n = (digits !! rem) : encode (n' + if rem > 2 then 1 else 0)
            where (n', rem) = n `divMod` 5
                  digits = "012=-"

main :: IO ()
main = do
    input <- fmap (map parseSnafu . lines) getContents
    putStrLn $ encodeSnafu $ sum input
