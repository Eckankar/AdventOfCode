{-# LANGUAGE TupleSections #-}

import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (chr, ord)
import Data.List (isInfixOf, isPrefixOf, permutations, sortOn)
import Data.List.Split (wordsBy)
import Data.Maybe (mapMaybe)
import Text.ParserCombinators.ReadP

import Debug.Trace (traceShow, traceShowId)

type Program = M.Map Int Int

data ProgramRun =
      Run ([Int] -> (Bool, ProgramRun, [Int]))
    | Done

revDigits 0 _ = []
revDigits k n = d : revDigits (k-1) n'
  where (n', d) = n `divMod` 10

runProgram :: [Int] -> Int -> Int -> (Program, [Int]) -> (Bool, ProgramRun, [Int])
runProgram rvs pc rb (p, d) =
    case op of
        1  -> cont rvs 4 $ binOp (+)
        2  -> cont rvs 4 $ binOp (*)
        3  -> opRead
        4  -> cont rvs 2 $ opWrite
        5  -> opJmp (/= 0)
        6  -> opJmp (== 0)
        7  -> cont rvs 4 $ binOp (bInt (<))
        8  -> cont rvs 4 $ binOp (bInt (==))
        9  -> opAdjustRB
        99 -> (True, Done, reverse d)
    where (op1:op2:ms) = revDigits 5 $ p M.! pc
          op = op2*10 + op1
          (rv:rvs') = rvs

          cont rvs n = runProgram rvs (pc+n) rb

          argA o = rebase (ms !! o) $ M.findWithDefault 0 (pc + o + 1) p
          arg o = imm (ms !! o) $ argA o

          rebase 2 n = n + rb
          rebase _ n = n

          imm 1 n = n
          imm _ n = M.findWithDefault 0 n p

          binOp f = (M.insert (argA 2) (f (arg 0) (arg 1)) p, d)

          opRead  =
            if null rvs
            then (False, Run $ \rvs' -> runProgram rvs' pc rb (p, []), reverse d)
            else cont rvs' 2 $ (M.insert (argA 0) rv p, d)
          opWrite = (p, arg 0 : d)

          opJmp pred = if pred $ arg 0 then runProgram rvs (arg 1) rb (p, d) else cont rvs 3 (p, d)

          opAdjustRB = runProgram rvs (pc + 2) (rb + arg 0) (p, d)

          bInt pred a b = if pred a b then 1 else 0

data Room = Room String String [String] [String]
    deriving (Show, Eq)

parseList :: String -> ReadP [String]
parseList s = do
    string s
    char '\n'

    items <- many1 $ between (string "- ") (char '\n') $ many1 $ satisfy (/= '\n')
    return items

parseRoom :: ReadP Room
parseRoom = do
    roomName <- between (string "== ") (string " ==") $ many1 $ satisfy (/= '\n')
    skipSpaces

    description <- munch1 (/= '\n')
    skipSpaces

    doors <- option [] $ parseList "Doors here lead:"
    skipSpaces

    items <- option [] $ parseList "Items here:"
    skipSpaces

    return $ Room roomName description doors items

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

stringMachine :: ([Int] -> (Bool, ProgramRun, [Int])) -> [String] -> String
stringMachine pf is = map chr res
    where is' = map ord $ unlines is
          (_, _, res) = pf is'

reducePath :: [String] -> [String]
reducePath ps = if ps /= ps' then reducePath ps' else ps
    where ps' = reducePath' ps
          reducePath' ("north":"south":ps) = reducePath' ps
          reducePath' ("south":"north":ps) = reducePath' ps
          reducePath' ("east":"west":ps)   = reducePath' ps
          reducePath' ("west":"east":ps)   = reducePath' ps
          reducePath' (p:ps) = p : reducePath' ps
          reducePath' [] = []

invertPath = reverse . map invert
    where invert "north" = "south"
          invert "south" = "north"
          invert "west"  = "east"
          invert "east"  = "west"

bfs pf active seen items rooms =
    if S.null active
    then (items, rooms)
    else bfs pf ns' seen' items' rooms'
    where rs = map getRoom $ S.toList active
          items' = foldl addItems items rs
          ns = S.fromList $ concatMap getNeighbors rs
          ns' = S.difference ns seen
          seen' = S.union seen ns'
          rooms' = foldl addRoom rooms rs

          getNeighbors (p, Room "Security Checkpoint" _ _ _) = []
          getNeighbors (p, Room _ _ es _) = map (reducePath . (p ++) . (:[])) es

          addItems items (p, Room _ _ _ is) = M.union items $ M.fromList $ map (, p) is
          addRoom rooms (p, Room n@"Security Checkpoint" _ es _) =
            M.insert n p $ M.insert "Pressure-Sensitive Floor" p' rooms
            where [p'] = filter ((> length p) . length) $ map (reducePath . (p ++) . (:[])) es
          addRoom rooms (p, Room n _ _ _) = M.insert n p rooms

          getRoom p = (p, r)
            where os = pf p
                  r = runParser (parseRoom <* eof) $ lastRoom os

          lastRoom os = unlines $ o : reverse os'
            where (os', o:_) = span (not . ("== " `isPrefixOf`)) $ tail $ reverse $ lines os

badItems = S.fromList ["escape pod", "giant electromagnet", "infinite loop", "molten lava", "photons"]

autoPlay :: ([String] -> String) -> (S.Set String, String)
autoPlay pf = head $ mapMaybe checkItems $ S.toList $ S.powerSet $ M.keysSet items'
    where (items, rooms) = bfs pf (S.singleton []) S.empty M.empty M.empty
          items' = items `M.withoutKeys` badItems
          itemPaths = map pickup $ M.toList items'

          (bestPath:_) = sortOn length $ map (reducePath . concat) $ permutations itemPaths
          bestPath' = reducePath $ bestPath ++ (rooms M.! "Security Checkpoint")
          dropAll = map ("drop " ++) $ M.keys items'

          tryCheckpoint = [last (rooms M.! "Pressure-Sensitive Floor")]
          pickup (n, p) = p ++ ["take " ++ n] ++ invertPath p

          checkItems is =
            if "Alert! Droids on this ship are" `isInfixOf` (traceShow (is, status) status)
            then Nothing
            else Just (is, os)
            where dropUnused = map ("drop " ++) $ M.keys $ items' `M.withoutKeys` is
                  os = pf $ bestPath' ++ dropUnused ++ tryCheckpoint
                  status = getReply os

          getReply os = s
            where (s:_) = dropWhile null $ tail $ dropWhile (not . ("== " `isPrefixOf`)) $ reverse $ lines os

play :: (Bool, ProgramRun, [Int]) -> IO ()
play (_, Done, os) = putStrLn $ map chr os
play (_, Run pf, os) = do
    putStrLn $ map chr os
    cmd <- fmap (map ord) getLine
    play $ pf (cmd ++ [10])

main :: IO ()
main = do
    input <- fmap (map read . wordsBy (== ',')) $ readFile "input.txt"
    let initProgram = M.fromList $ zip [0..] input

    -- play $ runProgram [] 0 0 (initProgram, [])

    let pf i = runProgram i 0 0 (initProgram, [])
    let (items, output) = autoPlay (stringMachine pf)

    print items
    putStrLn output


