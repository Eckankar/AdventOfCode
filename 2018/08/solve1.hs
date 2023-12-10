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

sumMetadata (Tree ts ms) = sum $ ms ++ map sumMetadata ts

main :: IO ()
main = do
    input <- fmap (runParser (parseTree <* eof)) getContents
    print $ sumMetadata input
