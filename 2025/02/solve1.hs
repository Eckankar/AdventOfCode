import Data.List.Split (splitWhen)

parseRange :: String -> (Int, Int)
parseRange r = (read s, read e)
    where [s, e] = splitWhen (=='-') r

invalidInRange :: (Int, Int) -> [Int]
invalidInRange (start, end) =
    takeWhile (<= end) $ dropWhile (< start) [ n | o <- [oStart ..], let n = read (show o ++ show o) ]
    where opener :: Int -> String
          opener n = defVal $ take (length ns `div` 2) ns
            where ns = show n
                  defVal [] = "0"
                  defVal x = x
          oStart :: Int
          oStart = read $ opener start

main :: IO ()
main = do
    ranges <- fmap (map parseRange . splitWhen (==',')) getContents
    print $ sum $ concatMap invalidInRange ranges