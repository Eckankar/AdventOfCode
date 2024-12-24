import qualified Data.Map as M

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Bits ((.&.), (.|.), xor)
import Data.Char (isAlphaNum, isDigit)
import Data.List (sortOn)
import Text.ParserCombinators.ReadP

data Value = Const Int | GAnd String String | GOr String String | GXor String String
    deriving (Eq, Show)

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- munch1 isDigit
    return $ read $ sign : digits

parseVarName :: ReadP String
parseVarName = many1 $ satisfy isAlphaNum

parseConst :: ReadP (M.Map String Value)
parseConst = do
    name <- parseVarName 
    string ": "
    value <- parseInt
    return $ M.singleton name $ Const value

parseConsts :: ReadP (M.Map String Value)
parseConsts = fmap M.unions $ sepBy parseConst (char '\n')

parseGateOp :: ReadP (String -> String -> Value)
parseGateOp =
    (string "AND" >> return GAnd) <|>
    (string "OR"  >> return GOr ) <|>
    (string "XOR" >> return GXor)

parseGate :: ReadP (M.Map String Value)
parseGate = do
    v1 <- parseVarName
    char ' '
    op <- parseGateOp
    char ' '
    v2 <- parseVarName
    string " -> "
    target <- parseVarName
    return $ M.singleton target $ op v1 v2

parseGates :: ReadP (M.Map String Value)
parseGates = fmap M.unions $ sepBy parseGate (char '\n')

parseInput :: ReadP (M.Map String Value)
parseInput = do
    consts <- parseConsts
    string "\n\n"
    gates <- parseGates
    skipSpaces
    eof
    return $ M.union consts gates

runParser p = fst . head . filter (null . snd) . readP_to_S p

evaluateGates :: M.Map String Value -> M.Map String Int
evaluateGates m = M.map evaluate m
    where evaluate (Const n) = n
          evaluate (GAnd v1 v2) = evaluate (m M.! v1) .&.   evaluate (m M.! v2)
          evaluate (GOr  v1 v2) = evaluate (m M.! v1) .|.   evaluate (m M.! v2)
          evaluate (GXor v1 v2) = evaluate (m M.! v1) `xor` evaluate (m M.! v2)

isXor (GXor _ _) = True
isXor _ = False

isOr (GOr _ _) = True
isOr _ = False

isAnd (GAnd _ _) = True
isAnd _ = False

isConst (Const _) = True
isConst _ = False

invalidXor m (GXor v1 v2) =
    not ( (isConst v1g && isConst v2g) || (isOr v1g && isXor v2g) || (isOr v2g && isXor v1g) )
    where v1g = m M.! v1
          v2g = m M.! v2
invalidXor _ _ = False

subvals m (GOr v1 v2) = [m M.! v1, m M.! v2]
subvals m (GXor v1 v2) = [m M.! v1, m M.! v2]
subvals m (GAnd v1 v2) = [m M.! v1, m M.! v2]
subvals m _ = []

main :: IO ()
main = do
    input <- fmap (runParser parseInput) getContents

    -- Validate, assuming we've got a ripple-carry adder
    -- https://en.wikipedia.org/wiki/Adder_(electronics)#Ripple-carry_adder
    -- https://en.wikipedia.org/wiki/Adder_(electronics)#Full_adder
    let outputs = M.filterWithKey (\(k:_) _ -> k == 'z') input
    let outputsNotFromXor = M.keys $ M.filter (not . isXor) outputs
    print ("outputsNotFromXor", map (\v -> (v, input M.! v)) outputsNotFromXor)

    -- ("outputsNotFromXor",[
    --      ("z16",GAnd "y16" "x16"),
    --      ("z20",GAnd "pns" "tsc"),
    --      ("z33",GOr "wkw" "jgr"),
    --      ("z45",GOr "kvm" "svd")
    -- ])
    -- The input is only 44 long - so it makes sense that z45 is an Or.
    --
    -- Sus gates:
    --      ("z16",GAnd "y16" "x16"),
    --      ("z20",GAnd "pns" "tsc"),
    --      ("z33",GOr "wkw" "jgr")
    --
    -- Good start; gives 3 gates - 1 or, 2 and; so let's try to find the xors that're supposed
    -- to go in those xor slots
    --
    -- Each xor should input from either 2 inputs, or an xor and a carry (which is an or)
    let invalidXors = M.keys $ M.filter (invalidXor input) input
    print ("invalidXors", map (\v -> (v, input M.! v, subvals input (input M.! v))) invalidXors)

    -- ("invalidXors",[
    --      ("z01",GXor "bdk" "rsq",[GAnd "x00" "y00",GXor "y01" "x01"]),
    --      ("z27",GXor "rvf" "dkb",[GAnd "x27" "y27",GOr "ttw" "ggw"]),
    --      ("z34",GXor "fcd" "mgc",[GXor "smf" "rfd",GXor "x34" "y34"])
    -- ])

    -- z01:
    -- rsq checks out
    -- Looks like bdk is the and gate that is supposed to go into carry0
    -- We're probably starting without a carry; so it would make sense to omit that or gate
    -- That one's probably fine...
    --
    -- z27:
    -- dkb checks out; but rvf is supposed to be an xor between x27 and y27 
    -- y27 XOR x27 -> tpc
    -- !!!!!!! swap tpc and rfv !!!!!!! 
    --
    -- z34:
    -- mgc looks correct; so fcd must be wrong - should be an or
    -- Let's see what fcd is
    --      x33 XOR y33 -> smf
    --      bmh OR stm -> rfd
    --      x32 AND y32 -> bmh
    --
    -- That looks like the output gate for z33 - we have an invalid or gate there; so it checks out!
    -- !!!!!!! swap fcd and z33 !!!!!!! 

    -- Now we just have 2 sus and gates.
    --
    --      ("z16",GAnd "y16" "x16"),
    -- clearly a carry and - should go into an or - that is xor'ed into z17
    --
    -- fmr XOR vtd -> z17
    --      y17 XOR x17 -> vtd
    --      hmk OR ndj -> fmr
    -- Must be one of the arguments to fmr - those should both be ands
    --      vmr XOR bnc -> hmk
    --      bnc AND vmr -> ndj
    -- Aha!
    -- !!!!!!! swap z16 and hmk !!!!!!! 
    --
    --
    -- ("z20",GAnd "pns" "tsc"),
    -- Doesn't take arguments directly from x/y; so must be xor/carry and
    --      x20 XOR y20 -> pns
    --      qgq OR qpp -> tsc
    --
    -- Aha, so it's z20's own carry and that has swapped with it's xor
    -- That xor should also take pns as input
    --      tsc XOR pns -> fhp
    -- !!!!!!! swap z20 and fhp !!!!!!! 

    -- swaps: fcd,fhp,hmk,rvf,tpc,z16,z20,z33
