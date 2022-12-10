data Instruction = Noop | AddX Int
    deriving (Show, Eq)

parseInstruction "noop" = Noop
parseInstruction ('a':'d':'d':'x':' ':n) = AddX $ read n

evaluateProgram = zip [1..] . reverse . snd . foldl evaluateInstruction (1, [])
    where evaluateInstruction (x, ss) Noop     = (x,     x:ss)
          evaluateInstruction (x, ss) (AddX n) = (x+n, x:x:ss)

main :: IO ()
main = do
    program <- fmap (map parseInstruction . lines) getContents
    print $ sum $ map (uncurry (*)) $ filter ((== 20) . (`mod` 40) . fst) $ evaluateProgram program
