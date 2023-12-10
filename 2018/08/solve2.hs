import qualified Data.Map as M

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

data Tree = Tree [Tree] [Int]
    deriving (Show, Eq)

parseNumber :: ReadP Int
parseNumber = read <$> munch1 isDigit

parseTree :: ReadP Tree
parseTree = do
    nChildren <- parseNumber
    char ' '
    nMetadata <- parseNumber
    char ' '
    children <- count nChildren (parseTree <* skipSpaces)
    metadata <- count nMetadata (parseNumber <* skipSpaces)
    return $ Tree children metadata

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

nodeValue (Tree [] ms) = sum $ ms
nodeValue (Tree cs ms) = sum $ map (\i -> M.findWithDefault 0 i cm) ms
    where cm = M.fromList $ zip [1..] $ map nodeValue cs

main :: IO ()
main = do
    input <- fmap (runParser (parseTree <* eof)) getContents
    print $ nodeValue input
