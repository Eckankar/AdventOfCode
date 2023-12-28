import qualified Data.Map as M

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Bits ((.&.), (.|.))
import Data.Char (isDigit, isLetter)
import Data.List.Extra ((!?))
import Text.ParserCombinators.ReadP

import Debug.Trace (traceShow, traceShowId)

data OpType = Add | Mul | BAnd | BOr | Set | Gt | Eq
    deriving (Eq, Show)

data ValType = Reg | Imm
    deriving (Eq, Show)

data Operation = Op OpType (ValType, ValType)
    deriving (Eq, Show)

data Instruction = Instr Operation Int Int Int
    deriving (Eq, Show)

parseNumber :: ReadP Int
parseNumber = read <$> munch1 isDigit

parseIp :: ReadP Int
parseIp = string "#ip " >> parseNumber

parseOpType :: ReadP OpType
parseOpType =
    (string "add" >> return Add) <|>
    (string "mul" >> return Mul) <|>
    (string "ban" >> return BAnd) <|>
    (string "bor" >> return BOr) <|>
--    (string "set" >> return Set) <|> -- set has a different type/name pattern, so handle it separately
    (string "gt"  >> return Gt) <|>
    (string "eq"  >> return Eq)

parseValTypes :: ReadP (ValType, ValType)
parseValTypes =
    ((string "r" <|> string "rr") >> return (Reg, Reg)) <|>
    ((string "i" <|> string "ri") >> return (Reg, Imm)) <|>
    (string "ir"                  >> return (Imm, Reg))

parseOperation :: ReadP Operation
parseOperation =
    (string "seti" >> return (Op Set (Imm, Imm))) <|>
    (string "setr" >> return (Op Set (Reg, Imm))) <|>
    (do opType <- parseOpType
        valTypes <- parseValTypes
        return $ Op opType valTypes)

parseInstruction :: ReadP Instruction
parseInstruction = do
    op <- parseOperation
    char ' '
    [fromA, fromB, to] <- sepBy1 parseNumber (char ' ')
    return $ Instr op fromA fromB to

parseInput :: ReadP (Int, [Instruction])
parseInput = do
    ip <- parseIp
    skipSpaces
    instrs <- sepBy parseInstruction (char '\n')
    skipSpaces
    eof
    return (ip, instrs)

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

setAt n ls v = pre ++ (v:post)
    where (pre, _:post) = splitAt n ls

applyOp (Instr (Op opType (fromAType, fromBType)) fromA fromB to) input =
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

runProgram instMap ipReg regs
    | not $ M.member ip instMap = regs
    | otherwise = runProgram instMap ipReg regs''
--    | otherwise = traceShow (ip, inst, regs) $ runProgram instMap ipReg regs''
    where ip  = regs  !! ipReg
          inst = instMap M.! ip
          Just regs' = applyOp inst regs
          ip' = regs' !! ipReg
          regs'' = setAt ipReg regs' (ip'+1)

findN :: Int -> Int -> Int
findN n c = if c >= 256 then findN n' c' else n'
    where n' = (((n + (c .&. 255)) .&. 16777215) * 65899) .&. 16777215
          c' = c `div` 256

main :: IO ()
main = do
    (ipReg, instrs) <- fmap (runParser parseInput) getContents

    print $ findN 4921097 65536

    let instMap = M.fromList $ zip [0..] instrs
    print $ head $ runProgram instMap ipReg [4797782, 0, 0, 0, 0, 0]

    -- see solve1.txt for decompilation
    --
    -- Basically resolves down to
    --
    -- n = 4921097
    -- c = 65536
    -- 
    -- do {
    --     n += c & 255;
    --     n = (65899 * n) & 16777215;
    --     c = c `div` 256  
    -- } while (c >= 256);
