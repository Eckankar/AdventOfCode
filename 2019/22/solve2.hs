import qualified Data.Map as M

import Algebra.PrincipalIdealDomain (extendedGCD)
import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP
import Text.Pretty.Simple (pPrint)

data Instruction = Deal Integer Integer | Cut Integer | Reverse
    deriving (Show, Eq)

deckSize = 119315717514047

parseInt :: ReadP Integer
parseInt = do
    sign <- option '0' (char '-')
    digits <- munch1 isDigit
    return $ read $ sign : digits

parseInstruction :: ReadP Instruction
parseInstruction =
    (do string "deal with increment "
        n <- parseInt
        let nInv = (deckSize + (snd $ snd $ extendedGCD deckSize n)) `mod` deckSize
        return $ Deal n nInv
        )
    <|> Cut <$> (string "cut " >> parseInt)
    <|> (string "deal into new stack" >> return Reverse)

runParser p = fst . head . filter (null . snd) . readP_to_S p

norm n = ((n `mod` deckSize) + deckSize) `mod` deckSize

data Expr =
      Add Expr Expr
    | Neg Expr
    | Mul Expr Expr
    | Const Integer
    | Var String
    deriving (Show, Eq)

applyInstruction :: Instruction -> Expr -> Expr
applyInstruction (Deal n nInv) e = Mul e (Const nInv)
applyInstruction (Cut n)       e = Add e (Const n)
applyInstruction Reverse       e = Add (Const (deckSize-1)) (Neg e)

simplifyExpr :: Expr -> Expr
simplifyExpr e = if e == e' then e else simplifyExpr e'
    where e' = simplifyExpr' e

          simplifyExpr' (Const n) | n < 0 || n >= deckSize = Const (norm n)
          simplifyExpr' (Add (Const n1) (Const n2)) = Const (n1+n2)
          simplifyExpr' (Mul (Const n1) (Const n2)) = Const (n1*n2)
          simplifyExpr' (Neg (Const n)) = Const (-n)

          simplifyExpr' (Add (Const n1) e) = simplifyExpr' (Add e (Const n1))
          simplifyExpr' (Mul (Const n1) e) = simplifyExpr' (Mul e (Const n1))

          simplifyExpr' (Add (Add e (Const n1)) (Const n2)) = Add e (Const (n1+n2))
          simplifyExpr' (Mul (Add e1 e2) (Const n)) = Add (Mul e1 (Const n)) (Mul e2 (Const n))
          simplifyExpr' (Mul (Mul e1 (Const n1)) (Const n2)) = Mul e1 (Const (n1*n2))

          simplifyExpr' (Neg (Add e1 e2)) = Add (simplifyExpr' (Neg e1)) (simplifyExpr' (Neg e2))
          simplifyExpr' (Neg (Mul e1 (Const n))) = Mul e1 (Const (-n))

          simplifyExpr' (Add e1 e2) = Add (simplifyExpr' e1) (simplifyExpr' e2)
          simplifyExpr' (Mul e1 e2) = Mul (simplifyExpr' e1) (simplifyExpr' e2)
          simplifyExpr' (Neg e)     = Neg (simplifyExpr' e)
          simplifyExpr' e = e


evalExpr :: M.Map String Integer -> Expr -> Integer
evalExpr m (Var x)     = m M.! x
evalExpr m (Const n)   = n
evalExpr m (Add e1 e2) = norm $ evalExpr m e1 + evalExpr m e2
evalExpr m (Mul e1 e2) = norm $ evalExpr m e1 * evalExpr m e2
evalExpr m (Neg e)     = norm $ - evalExpr m e

repeatExpr :: Integer -> Expr -> Expr
repeatExpr 0 _ = Var "x"
repeatExpr n e =
    if r == 0
    then repeatExpr n' eSq
    else simplifyExpr $ combine $ repeatExpr n' eSq
    where eSq = simplifyExpr $ combine e
          (n', r) = divMod n 2
          combine (Var x) = e
          combine (Add e1 e2) = Add (combine e1) (combine e2)
          combine (Mul e1 e2) = Mul (combine e1) (combine e2)
          combine (Neg e)     = Neg (combine e)
          combine e           = e

main = do
    input <- fmap (map (runParser (parseInstruction <* eof)) . lines) getContents

    let e = simplifyExpr $ foldr applyInstruction (Var "x") input
    pPrint e

    let e' = simplifyExpr $ repeatExpr 101741582076661 e
    pPrint e'

    print $ evalExpr (M.singleton "x" 2020) e'
