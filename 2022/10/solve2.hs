import Data.List (intersperse)
import Data.List.Split (chunksOf)

data Instruction = Noop | AddX Int
    deriving (Show, Eq)

parseInstruction "noop" = Noop
parseInstruction ('a':'d':'d':'x':' ':n) = AddX $ read n

evaluateProgram = zip [1..] . reverse . snd . foldl evaluateInstruction (1, [])
    where evaluateInstruction (x, ss) Noop     = (x,     x:ss)
          evaluateInstruction (x, ss) (AddX n) = (x+n, x:x:ss)

toPixel (i, x) = if abs (iPos - x) <= 1 then '#' else '.'
    where iPos = (i-1) `mod` 40

main :: IO ()
main = do
    program <- fmap (map parseInstruction . lines) getContents
    putStrLn $ concat $ intersperse "\n" $ chunksOf 40 $ map toPixel $ evaluateProgram program
