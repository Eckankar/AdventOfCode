import qualified Data.Map as M

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Bits ((.&.), (.|.))
import Data.Char (isDigit, isLetter)
import Data.List.Extra ((!?))
import Text.ParserCombinators.ReadP

data OpType = Add | Mul | BAnd | BOr | Set | Gt | Eq
    deriving (Eq, Show)

data ValType = Reg | Imm
    deriving (Eq, Show)

data Operation = Op OpType (ValType, ValType)
    deriving (Eq, Show)

allOps = [
    Op Add (Reg, Reg), Op Add (Reg, Imm),
    Op Mul (Reg, Reg), Op Mul (Reg, Imm),
    Op BAnd (Reg, Reg), Op BAnd (Reg, Imm),
    Op BOr (Reg, Reg), Op BOr (Reg, Imm),
    Op Set (Reg, Imm), Op Set (Imm, Imm),
    Op Gt (Imm, Reg), Op Gt (Reg, Imm), Op Gt (Reg, Reg),
    Op Eq (Imm, Reg), Op Eq (Reg, Imm), Op Eq (Reg, Reg)
    ]

parseNumber :: ReadP Int
parseNumber = read <$> munch1 isDigit

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

parseInstruction = sepBy1 parseNumber (char ' ')

parseRegisters = between (char '[') (char ']') $ sepBy1 parseNumber (string ", ")

parseExample = do
    string "Before: "
    regBefore <- parseRegisters
    char '\n'
    instruction <- parseInstruction
    char '\n'
    string "After:  "
    regAfter <- parseRegisters

    return (instruction, regBefore, regAfter)

parseInput = do
    examples <- sepBy parseExample (string "\n\n")
    string "\n\n\n\n"
    instructions <- sepBy parseInstruction (string "\n")
    skipSpaces
    eof
    return (examples, instructions)

setAt n ls v = pre ++ (v:post)
    where (pre, _:post) = splitAt n ls

applyOp (Op opType (fromAType, fromBType)) fromA fromB to input =
    setAt to input <$>
        if opType == Set
        then readVal fromAType fromA
        else do
          valA <- readVal fromAType fromA
          valB <- readVal fromBType fromB
          return $ case opType of
            Add  -> valA  +  valB
            Mul  -> valA  *  valB
            BAnd -> valA .&. valB
            BOr  -> valA .|. valB
            Gt   -> if valA >  valB then 1 else 0
            Eq   -> if valA == valB then 1 else 0
    where readVal Reg a = input !? a
          readVal Imm a = Just a


narrowFromExample opMap ([opCode, fromA, fromB, to], before, after) = opMap'
    where candidateOps = M.findWithDefault allOps opCode opMap
          matchingOps = filter (\op -> applyOp op fromA fromB to before == Just after) candidateOps
          opMap' = M.insert opCode matchingOps opMap

reduceOpmap opMap =
    if M.null opMap
    then M.empty
    else M.union opUniq' $ reduceOpmap $ M.map (filter (not . (`elem` (M.elems opUniq')))) opAmbi
    where (opUniq, opAmbi) = M.partition ((== 1) . length) opMap
          opUniq' = M.map head opUniq

runOp opMap input [opCode, fromA, fromB, to] = output
    where op = opMap M.! opCode
          Just output = applyOp op fromA fromB to input

main = do
    (examples, instructions) <- fmap (runParser parseInput) getContents 

    let opMap = reduceOpmap $ foldl narrowFromExample M.empty examples

    let res = foldl (runOp opMap) [0,0,0,0] instructions
    print $ head res
