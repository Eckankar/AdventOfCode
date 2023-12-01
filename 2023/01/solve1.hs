import Data.Char (isDigit)

takeEnds ls = [head ls, last ls]

main :: IO ()
main = do
    input <- fmap (map ((read :: String -> Int) . takeEnds . filter isDigit) . lines) getContents
    print $ sum input
