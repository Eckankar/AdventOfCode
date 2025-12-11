import qualified Data.Map as M
import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isLetter)
import Text.ParserCombinators.ReadP

import Data.Function.Memoize (memoize3)

parseInputLine :: ReadP (String, [String])
parseInputLine = do
    input <- munch1 isLetter
    string ": "
    outputs <- sepBy (munch1 isLetter) $ char ' '
    return (input, outputs)

runParser p = fst . head . filter (null . snd) . readP_to_S p

countPaths g t = countPathsMemo False False t
    where countPathsMemo = memoize3 countPaths'
          countPaths' seenFFT seenDAC t
            | t == "out"         = if seenFFT && seenDAC then 1 else 0
            | otherwise          = sum $ map (countPathsMemo seenFFT' seenDAC') $ g M.! t
                where seenFFT' = t == "fft" || seenFFT
                      seenDAC' = t == "dac" || seenDAC


main :: IO ()
main = do
    graph <- fmap (M.fromList . map (runParser (parseInputLine)) . lines) getContents
    
    print $ countPaths graph "svr"