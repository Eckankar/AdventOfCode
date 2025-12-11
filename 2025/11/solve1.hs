import qualified Data.Map as M

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isLetter)
import Text.ParserCombinators.ReadP

parseInputLine :: ReadP (String, [String])
parseInputLine = do
    input <- munch1 isLetter
    string ": "
    outputs <- sepBy (munch1 isLetter) $ char ' '
    return (input, outputs)

runParser p = fst . head . filter (null . snd) . readP_to_S p

countPaths _ "out" = 1
countPaths g t = sum $ map (countPaths g) $ g M.! t

main :: IO ()
main = do
    graph <- fmap (M.fromList . map (runParser (parseInputLine)) . lines) getContents

    print $ countPaths graph "you"