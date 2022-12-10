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

main :: IO ()
main = do
    program <- fmap (map parseInstruction . lines) getContents
    print $ sum $ map (uncurry (*)) $ filter ((== 20) . (`mod` 40) . fst) $ evaluateProgram program
