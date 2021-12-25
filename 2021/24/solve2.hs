{-# LANGUAGE TupleSections #-}

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit)
import Data.List.Split (whenElt, split, keepDelimsL)
import Data.Functor (($>))
import Text.ParserCombinators.ReadP

import Debug.Trace (traceShow)

data Register = RegW | RegX | RegY | RegZ
    deriving (Show, Eq)

data Value =  Reg Register | Imm Int
    deriving (Show, Eq)

data Instruction = Inp Register
                 | Add Register Value
                 | Mul Register Value
                 | Div Register Value
                 | Mod Register Value
                 | Eql Register Value
    deriving (Show, Eq)

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- many (satisfy isDigit)
    return $ read $ sign : digits

parseRegister = choice [ char 'w' $> RegW, char 'x' $> RegX, char 'y' $> RegY, char 'z' $> RegZ ]

parseValue = (Reg <$> parseRegister) <|> (Imm <$> parseInt)

parseInstruction1 :: String -> (Register -> Instruction) -> ReadP Instruction
parseInstruction1 s construct = string s *> skipSpaces *> (construct <$> parseRegister)

parseInstruction2 :: String -> (Register -> Value -> Instruction) -> ReadP Instruction
parseInstruction2 s construct = do
    string s
    skipSpaces
    r <- parseRegister
    skipSpaces
    v <- parseValue
    return $ construct r v

parseInstruction = choice [
        parseInstruction1 "inp" Inp,
        parseInstruction2 "add" Add,
        parseInstruction2 "mul" Mul,
        parseInstruction2 "div" Div,
        parseInstruction2 "mod" Mod,
        parseInstruction2 "eql" Eql
    ] <* skipSpaces

parseProgram = many parseInstruction <* eof

runParser p = fst . head . filter (null . snd) . readP_to_S p

isInput (Inp _) = True
isInput _       = False

getValue _  (Imm v) = v
getValue rs (Reg r) = getRegister rs r

getRegister (w,x,y,z) RegW = w
getRegister (w,x,y,z) RegX = x
getRegister (w,x,y,z) RegY = y
getRegister (w,x,y,z) RegZ = z

setRegister (w,x,y,z) RegW v = (v,x,y,z)
setRegister (w,x,y,z) RegX v = (w,v,y,z)
setRegister (w,x,y,z) RegY v = (w,x,v,z)
setRegister (w,x,y,z) RegZ v = (w,x,y,v)

runProgram ps = (M.! 0) $ foldl runOnChunk (M.singleton 0 0) $ zip chunks zCaps
    where chunks = findChunks ps

          zCaps = init $ foldr (\(a,_,_) (x:xs) -> (a*(x+1)-1) : x:xs) [0] $ map extractFromChunk chunks

          runOnChunk :: M.Map Int Int -> ([Instruction], Int) -> M.Map Int Int
          runOnChunk s (cs,zc) = M.filterWithKey (\k _ -> k <= zc) $ decreaseStateDim $ foldl runOnUniverse (increaseStateDim s) cs

          increaseStateDim = M.mapKeys (0,0,0,)
          decreaseStateDim = M.mapKeysWith min (\(_,_,_,z) -> z)

          runOnUniverse s i | traceShow (M.size s, i) False = undefined
          runOnUniverse s i = M.fromListWith min $ concatMap (runInstruction i) $ M.assocs s

          applyBinopIf (rs, i) r v p f = [ (setRegister rs r (f rv vv), i) | p rv vv ]
            where rv = getRegister rs r
                  vv = getValue rs v

          applyBinop (rs, i) r v f = applyBinopIf (rs, i) r v (\_ _ -> True) f

          runInstruction (Inp r)   (rs, i) = map (\d -> (setRegister rs r d, 10*i+d)) [1..9]
          runInstruction (Add r v) (rs, i) = applyBinop (rs, i) r v (+)
          runInstruction (Mul r v) (rs, i) = applyBinop (rs, i) r v (*)
          runInstruction (Div r v) (rs, i) = applyBinopIf (rs, i) r v (\rv vv -> vv /= 0) div
          runInstruction (Mod r v) (rs, i) =
            applyBinopIf (rs, i) r v (\rv vv -> rv >= 0 && vv > 0) mod
          runInstruction (Eql r v) (rs, i) = applyBinop (rs, i) r v (\rv vv -> if rv == vv then 1 else 0)

findChunks = tail . split (keepDelimsL $ whenElt isInput)

extractFromChunk [
    Inp RegW,
    Mul RegX (Imm 0),
    Add RegX (Reg RegZ),
    Mod RegX (Imm 26),
    Div RegZ (Imm a),
    Add RegX (Imm b),
    Eql RegX (Reg RegW),
    Eql RegX (Imm 0),
    Mul RegY (Imm 0),
    Add RegY (Imm 25),
    Mul RegY (Reg RegX),
    Add RegY (Imm 1),
    Mul RegZ (Reg RegY),
    Mul RegY (Imm 0),
    Add RegY (Reg RegW),
    Add RegY (Imm c),
    Mul RegY (Reg RegX),
    Add RegZ (Reg RegY)
    ] = (a, b, c)
extractFromChunk c = error $ show c

{-
Looking at the input data, it consists of 14 such chunks, for my input with (a,b,c) =
[(1,11,7),(1,14,8),(1,10,16),(1,14,8),(26,-8,3),(1,14,12),(26,-11,1),(1,10,8),(26,-6,8),(26,-9,14),(1,12,4),(26,-5,14),(26,-4,15),(26,-9,6)]

Analyzing it, it should be equivalent to

w  = input
x  = if (z mod 26) + B /= w then 1 else 0
z /= A
z  = z * (25*x + 1)
z += (w + C) * x

Notably, only z actually persists across chunk borders. So we can ignore any differences in non-z
registers when crossing a chunk border.

C is always positive, so z is always non-negative.
If we want z to be 0; x should be 0 (since w+C > 0)

We know that the incoming z should be at most A * targetZ)

-}

main :: IO ()
main = do
    program <- fmap (runParser parseProgram) getContents
    print $ runProgram program
