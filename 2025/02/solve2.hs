import qualified Data.Set as S
import Data.List.Split (splitWhen)

parseRange :: String -> (Int, Int)
parseRange r = (read s, read e)
    where [s, e] = splitWhen (=='-') r

invalidInRange (start, end) = concat [ invalidInRange_ (start, end) i | i <- [2 .. length $ show end]]

invalidInRange_ :: (Int, Int) -> Int -> [Int]
invalidInRange_ (start, end) i =
    takeWhile (<= end) $ dropWhile (< start) [ n | o <- [oStart ..], let n = read (concat $ replicate i $ show o) ]
    where opener :: Int -> String
          opener n = defVal $ take (length ns `div` i) ns
            where ns = show n
                  defVal [] = "0"
                  defVal x = x
          oStart :: Int
          oStart = read $ opener start

main :: IO ()
main = do
    ranges <- fmap (map parseRange . splitWhen (==',')) getContents
    print $ sum $ S.toList $ S.fromList $ concatMap invalidInRange ranges