import Data.Char (isDigit)
import Data.List (elemIndex, isPrefixOf, tails)
import Data.Maybe (mapMaybe)

numVal (c:s) | isDigit c = Just $ read [c]
numVal s | not (null prefix) = elemIndex (head prefix) numbers
    where numbers = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
          prefix = filter (`isPrefixOf` s) numbers
numVal _ = Nothing

main :: IO ()
main = do
    input <- fmap (map ((\v -> 10 * head v + last v) . mapMaybe numVal . tails) . lines) getContents
    print $ sum input
