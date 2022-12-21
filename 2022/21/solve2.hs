import qualified Data.Map as M
import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit, isLetter)
import Text.ParserCombinators.ReadP

data Stmt = Op Char String String
          | Const Int
    deriving (Show, Eq)

data Expr =
      EOp Char Expr Expr
    | EConst Int
    | EHuman
    | ECmp Expr Expr
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

eval :: M.Map String Stmt -> String -> Expr
eval p "humn" = EHuman
eval p var = eval' $ p M.! var
    where eval' (Const n) = EConst n
          eval' (Op op var1 var2) = f (eval p var1) (eval p var2)
            where f = if var == "root" then ECmp else EOp op

resolveOp '+' = (+)
resolveOp '-' = (-)
resolveOp '/' = div
resolveOp '*' = (*)

reduce :: Expr -> Expr
reduce (EOp op (EConst n) (EConst m)) = EConst $ resolveOp op n m
reduce (EOp op e1 e2) =
    let e1' = reduce e1
        e2' = reduce e2 in
        if e1 /= e1' || e2 /= e2'
        then reduce (EOp op e1' e2')
        else EOp op e1 e2
reduce (ECmp e1 e2) = ECmp (reduce e1) (reduce e2)
reduce x = x

peelBack :: Expr -> Expr
peelBack (ECmp e1 e2) = peelBack' e1 e2
    where peelBack' (EOp '+' e (EConst n)) (EConst m) = peelBack' e (EConst (m - n))
          peelBack' (EOp '-' e (EConst n)) (EConst m) = peelBack' e (EConst (m + n))
          peelBack' (EOp '-' (EConst n) e) (EConst m) = peelBack' e (EConst (-m + n))
          peelBack' (EOp '*' e (EConst n)) (EConst m) = peelBack' e (EConst (m `div` n))
          peelBack' (EOp '/' e (EConst n)) (EConst m) = peelBack' e (EConst (m * n))
          peelBack' (EOp '/' (EConst n) e) _ = error "Let's hope this case doesn't come up"
          peelBack' (EOp op (EConst n) e) (EConst m)  = peelBack' (EOp op e (EConst n)) (EConst m)
          peelBack' e1 e2 = ECmp e1 e2

main :: IO ()
main = do
    input <- fmap (map (runParser (parseLine <* eof)) . lines) getContents
    let program = M.fromList input
    print $ peelBack $ reduce $ eval program "root"
