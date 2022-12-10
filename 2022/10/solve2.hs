import Data.List (intersperse)
import Data.List.Split (chunksOf)

data Instruction = Noop | AddX Int
    deriving (Show, Eq)

parseInstruction "noop" = Noop
parseInstruction ('a':'d':'d':'x':' ':n) = AddX $ read n

evaluateProgram = reverse . snd . foldl evaluateInstruction init
    where init = ((0, 1), [])
          evaluateInstruction ((ic, x), ss) i =
            case i of
                Noop   -> ((ic+1, x), (ic+1, x):ss)
                AddX n -> ((ic+2, x+n), (ic+2, x):(ic+1, x):ss)

toPixel (i, x) = if abs (iPos - x) <= 1 then '#' else '.'
    where iPos = (i-1) `mod` 40

main :: IO ()
main = do
    program <- fmap (map parseInstruction . lines) getContents
    putStrLn $ concat $ intersperse "\n" $ chunksOf 40 $ map toPixel $ evaluateProgram program
