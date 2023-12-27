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
    | otherwise = traceShow (ip, inst, regs) $ runProgram instMap ipReg regs''
    where ip  = regs  !! ipReg
          inst = instMap M.! ip
          Just regs' = applyOp inst regs
          ip' = regs' !! ipReg
          regs'' = setAt ipReg regs' (ip'+1)

main :: IO ()
main = do
    (ipReg, instrs) <- fmap (runParser parseInput) getContents

    let instMap = M.fromList $ zip [0..] instrs
    print $ head $ runProgram instMap ipReg [1, 0, 0, 0, 0, 0]

    -- Solved by hand.
    --
    -- Running the program gives the following trace:
    --
    -- (0,Instr (Op Add (Reg,Imm)) 4 16 4,[1,0,0,0,0,0])
    -- (17,Instr (Op Add (Reg,Imm)) 2 2 2,[1,0,0,0,17,0])
    -- (18,Instr (Op Mul (Reg,Reg)) 2 2 2,[1,0,2,0,18,0])
    -- (19,Instr (Op Mul (Reg,Reg)) 4 2 2,[1,0,4,0,19,0])
    -- (20,Instr (Op Mul (Reg,Imm)) 2 11 2,[1,0,76,0,20,0])
    -- (21,Instr (Op Add (Reg,Imm)) 1 6 1,[1,0,836,0,21,0])
    -- (22,Instr (Op Mul (Reg,Reg)) 1 4 1,[1,6,836,0,22,0])
    -- (23,Instr (Op Add (Reg,Imm)) 1 18 1,[1,132,836,0,23,0])
    -- (24,Instr (Op Add (Reg,Reg)) 2 1 2,[1,150,836,0,24,0])
    -- (25,Instr (Op Add (Reg,Reg)) 4 0 4,[1,150,986,0,25,0])
    -- (27,Instr (Op Set (Reg,Imm)) 4 5 1,[1,150,986,0,27,0])
    -- (28,Instr (Op Mul (Reg,Reg)) 1 4 1,[1,27,986,0,28,0])
    -- (29,Instr (Op Add (Reg,Reg)) 4 1 1,[1,756,986,0,29,0])
    -- (30,Instr (Op Mul (Reg,Reg)) 4 1 1,[1,785,986,0,30,0])
    -- (31,Instr (Op Mul (Reg,Imm)) 1 14 1,[1,23550,986,0,31,0])
    -- (32,Instr (Op Mul (Reg,Reg)) 1 4 1,[1,329700,986,0,32,0])
    -- (33,Instr (Op Add (Reg,Reg)) 2 1 2,[1,10550400,986,0,33,0])
    -- (34,Instr (Op Set (Imm,Imm)) 0 1 0,[1,10550400,10551386,0,34,0])
    -- (35,Instr (Op Set (Imm,Imm)) 0 4 4,[0,10550400,10551386,0,35,0])
    -- (1,Instr (Op Set (Imm,Imm)) 1 3 5,[0,10550400,10551386,0,1,0])
    -- (2,Instr (Op Set (Imm,Imm)) 1 1 3,[0,10550400,10551386,0,2,1])
    -- (3,Instr (Op Mul (Reg,Reg)) 5 3 1,[0,10550400,10551386,1,3,1])
    -- (4,Instr (Op Eq (Reg,Reg)) 1 2 1,[0,1,10551386,1,4,1])
    -- (5,Instr (Op Add (Reg,Reg)) 1 4 4,[0,0,10551386,1,5,1])
    -- (6,Instr (Op Add (Reg,Imm)) 4 1 4,[0,0,10551386,1,6,1])
    -- (8,Instr (Op Add (Reg,Imm)) 3 1 3,[0,0,10551386,1,8,1])
    -- (9,Instr (Op Gt (Reg,Reg)) 3 2 1,[0,0,10551386,2,9,1])
    -- (10,Instr (Op Add (Reg,Reg)) 4 1 4,[0,0,10551386,2,10,1])
    -- (11,Instr (Op Set (Imm,Imm)) 2 8 4,[0,0,10551386,2,11,1])
    -- (3,Instr (Op Mul (Reg,Reg)) 5 3 1,[0,0,10551386,2,3,1])
    -- (4,Instr (Op Eq (Reg,Reg)) 1 2 1,[0,2,10551386,2,4,1])
    -- (5,Instr (Op Add (Reg,Reg)) 1 4 4,[0,0,10551386,2,5,1])
    -- (6,Instr (Op Add (Reg,Imm)) 4 1 4,[0,0,10551386,2,6,1])
    -- (8,Instr (Op Add (Reg,Imm)) 3 1 3,[0,0,10551386,2,8,1])
    -- (9,Instr (Op Gt (Reg,Reg)) 3 2 1,[0,0,10551386,3,9,1])
    -- (10,Instr (Op Add (Reg,Reg)) 4 1 4,[0,0,10551386,3,10,1])
    -- (11,Instr (Op Set (Imm,Imm)) 2 8 4,[0,0,10551386,3,11,1])
    -- (3,Instr (Op Mul (Reg,Reg)) 5 3 1,[0,0,10551386,3,3,1])
    -- (4,Instr (Op Eq (Reg,Reg)) 1 2 1,[0,3,10551386,3,4,1])
    -- (5,Instr (Op Add (Reg,Reg)) 1 4 4,[0,0,10551386,3,5,1])
    -- (6,Instr (Op Add (Reg,Imm)) 4 1 4,[0,0,10551386,3,6,1])
    -- (8,Instr (Op Add (Reg,Imm)) 3 1 3,[0,0,10551386,3,8,1])
    -- (9,Instr (Op Gt (Reg,Reg)) 3 2 1,[0,0,10551386,4,9,1])
    -- (10,Instr (Op Add (Reg,Reg)) 4 1 4,[0,0,10551386,4,10,1])
    -- (11,Instr (Op Set (Imm,Imm)) 2 8 4,[0,0,10551386,4,11,1])
    -- (3,Instr (Op Mul (Reg,Reg)) 5 3 1,[0,0,10551386,4,3,1])
    -- (4,Instr (Op Eq (Reg,Reg)) 1 2 1,[0,4,10551386,4,4,1])
    -- (5,Instr (Op Add (Reg,Reg)) 1 4 4,[0,0,10551386,4,5,1])
    -- (6,Instr (Op Add (Reg,Imm)) 4 1 4,[0,0,10551386,4,6,1])
    -- (8,Instr (Op Add (Reg,Imm)) 3 1 3,[0,0,10551386,4,8,1])
    -- (9,Instr (Op Gt (Reg,Reg)) 3 2 1,[0,0,10551386,5,9,1])
    -- ...
    --
    --
    -- The program sets up a large number - in my case 10551386 - and then jumps to the code at address 1
    --
    -- seti 1 1 3
    -- mulr 5 3 1
    -- eqrr 1 2 1
    -- addr 1 4 4
    -- addi 4 1 4
    -- addr 5 0 0
    -- addi 3 1 3
    -- gtrr 3 2 1
    -- addr 4 1 4
    -- seti 2 8 4
    -- addi 5 1 5
    -- gtrr 5 2 1
    -- addr 1 4 4
    -- seti 1 3 4
    -- mulr 4 4 4
    --
    -- This code can be translated to:
    --
    -- r5 := 1
    -- while (r5 <= r2) {
    --   r3 := 1
    --   while (r3 <= r2) {
    --     if (r3*r5 == r2) {
    --       r0 += r5
    --     }
    --     r3 += 1
    --   }
    -- 
    --   r5 += 1
    -- }
    --
    -- Looking at that trace, we see that by the end, r0 is the sum of divisors of the input.
    -- In the case of 10551386, WolframAlpha gives us {1, 2, 5275693, 10551386}, which sum
    -- to 15827082 - the answer on my input.
