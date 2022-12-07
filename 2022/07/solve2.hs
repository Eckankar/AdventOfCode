import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit, isSpace)
import Data.List (sort)
import Data.Maybe (catMaybes)
import Text.ParserCombinators.ReadP

data FileSystem = File String Int | Folder String [FileSystem]
    deriving (Show, Eq)

parseIdentifier :: ReadP String
parseIdentifier = many1 $ satisfy (\c -> not (isSpace c) && c /= '.')

parseFilename :: ReadP String
parseFilename = many1 $ satisfy (not . isSpace)

parseNumber :: ReadP Int
parseNumber = do
    n <- many (satisfy isDigit)
    return $ read n

parseFolderContents :: ReadP (Maybe FileSystem)
parseFolderContents =
        (string "dir " >> parseIdentifier >> skipSpaces >> return Nothing)
    <|> (do fs <- parseNumber
            char ' '
            fn <- parseFilename
            skipSpaces
            return $ Just $ File fn fs)
    <|> fmap Just parseFolder

parseFolder :: ReadP FileSystem
parseFolder = do
    string "$ cd "
    fn <- parseIdentifier
    skipSpaces

    string "$ ls"
    skipSpaces

    fc <- fmap catMaybes $ many parseFolderContents

    (string "$ cd .." >> return ()) <|> eof
    skipSpaces

    return $ Folder fn fc

parseInput = do
    f <- parseFolder
    eof
    return f

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

findFileFolder :: (FileSystem -> Bool) -> FileSystem -> [FileSystem]
findFileFolder p folder@(Folder n fs) =
    (if p folder then [folder] else []) ++ concatMap (findFileFolder p) fs
findFileFolder p file =
     if p file then [file] else []

isFolder (Folder _ _) = True
isFolder _            = False

fileSize (Folder _ fs) = sum $ map fileSize fs
fileSize (File _ fs) = fs

main :: IO ()
main = do
    input <- getContents
    let tree = runParser parseInput input

    let totalSize = fileSize tree
    let requiredSize = totalSize - (70000000 - 30000000)
    print $ head $ sort $ filter (>= requiredSize) $ map fileSize $ findFileFolder isFolder tree
