import qualified Data.Map as M
import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit, isLetter)
import Text.ParserCombinators.ReadP

data Stmt = Op Char String String
          | Const Int
    deriving (Show, Eq)

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- many1 (satisfy isDigit)
    return $ read $ sign : digits

parseVar :: ReadP String
parseVar = many1 (satisfy isLetter)

parseStmt :: ReadP Stmt
parseStmt =
    (Const <$> parseInt) <|>
    (do var1 <- parseVar
        char ' '
        op <- satisfy (`S.member` ops)
        char ' '
        var2 <- parseVar
        return $ Op op var1 var2)
    where ops = S.fromList "+-/*"

parseLine :: ReadP (String, Stmt)
parseLine = do
    var <- parseVar
    string ": "
    expr <- parseStmt
    return (var, expr)

runParser p = fst . head . filter (null . snd) . readP_to_S p

eval :: M.Map String Stmt -> String -> Int
eval p var = eval' $ p M.! var
    where eval' (Const n) = n
          eval' (Op op var1 var2) = resolveOp op (eval p var1) (eval p var2)

          resolveOp '+' = (+)
          resolveOp '-' = (-)
          resolveOp '/' = div
          resolveOp '*' = (*)

main :: IO ()
main = do
    input <- fmap (map (runParser (parseLine <* eof)) . lines) getContents
    let program = M.fromList input
    print $ eval program "root"
