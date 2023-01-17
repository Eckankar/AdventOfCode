import qualified Data.Map as M
import qualified Data.Set as S

import Data.Char (chr, ord)
import Data.List (inits, isPrefixOf, sortOn)
import Data.List.Split (wordsBy)
import Data.Maybe (isJust)
import Data.Tuple.Extra (fst3)

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

type Point = (Integer, Integer)
data Direction = DLeft | DRight
    deriving (Eq)
data Path = Rotate Direction | Move Int | Call Char
    deriving (Eq)

instance Show Direction where
    show DLeft  = "L"
    show DRight = "R"

instance Show Path where
    show (Rotate d) = show d
    show (Move n)   = show n
    show (Call c)   = [c]

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [0..] r) . zip [0..]

add (x, y) (x', y') = (x+x', y+y')
neighbors (x,y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

rotate DLeft  (x, y) = (-y, x)
rotate DRight (x, y) = (y, -x)

dir2vec '<' = Just (0, -1)
dir2vec '>' = Just (0,  1)
dir2vec '^' = Just (-1, 0)
dir2vec 'v' = Just ( 1, 0)
dir2vec _   = Nothing

findPath m p d = findPath' p d S.empty
    where scafs = M.keysSet $ M.filter (== '#') m
          findPath' p d s
            | S.null $ S.difference scafs s = []
            | otherwise = Rotate rd : Move n : findPath' p' d' s'
                where [rd] = filter ((== Just '#') . flip M.lookup m . add p . flip rotate d) [DLeft, DRight]
                      d' = rotate rd d
                      dp = takeWhile ((== Just '#') . flip M.lookup m) $ tail $ iterate (add d') p
                      n = length dp
                      s' = S.union s $ S.fromList dp
                      p' = last dp

compress ((Call c):ps) rs vs vm = compress ps ((Call c):rs) vs vm
compress [] rs [] vm | length (show rs) - 2 <= 20 = [(reverse rs, vm)]
                     | otherwise              = []
compress _  _  [] _ = []
compress ps rs (v:vs) vm = concatMap compressWith rvs
    where hs = takeWhile ((\n -> n <= 20) . (\n -> n-2) . length . show) $ tail $ inits $ takeWhile (not . isCall) ps
          rvs = sortOn fst3 $ map replace hs
            where replace h = (rvl, h, rv)
                    where rv = replaceAll ps h v
                          rvl = length (show rv) - 2

          compressWith (_, h, rv) = compress rv rs vs $ M.insert v h vm

          replaceAll [] _ _ = []
          replaceAll ps h v | h `isPrefixOf` ps = Call v : replaceAll (drop (length h) ps) h v
          replaceAll (p:ps) h v = p : replaceAll ps h v

          isCall (Call _) = True
          isCall _        = False

inputify :: [Path] -> String
inputify = init . tail . show

main :: IO ()
main = do
    input <- fmap (map read . wordsBy (== ',')) getContents
    let initProgram = M.fromList $ zip [0..] input

    let (_, _, output) = runProgram [] 0 0 (initProgram, [])
    let mapStr = map chr output
    putStrLn mapStr
    let m = toMap $ lines mapStr

    let [(startPos, Just dir)] = M.toList $ M.filter isJust $ M.map dir2vec m
    print (startPos, dir)

    let path = findPath m startPos dir
    print path

    let ((p, m):_) = compress path [] "ABC" M.empty

    print (p, m)
    let initProgram' = M.insert 0 2 initProgram
    let logic = map ord $ concatMap (++ "\n") $ (++ ["n"]) $ map inputify (p : map (m M.!) "ABC")
    let (_, _, output') = runProgram logic 0 0 (initProgram', [])
    print $ last output'
