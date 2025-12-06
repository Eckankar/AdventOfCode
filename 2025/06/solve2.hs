import Data.Char (isDigit, isSpace)
import Data.List (transpose)
import Data.List.Split (wordsBy)

calcRow ins = foldl1 (toOp sop) ns
    where [sop] = filter (\c -> c == '+' || c == '*') $ map last ins
          sns = map (filter isDigit) ins
          ns = map (read :: String -> Int) sns
          toOp '+' = (+)
          toOp '*' = (*)

main :: IO ()
main = do
    input <- fmap (wordsBy (all isSpace) . transpose . lines) getContents

    print $ sum $ map calcRow input