data Instruction = Forward Int | Down Int

convert :: [String] -> Instruction
convert ["forward", n] = Forward $ read n
convert ["down", n]    = Down $  read n
convert ["up", n]      = Down $ -read n

update (h, v, a) (Down n) = (h, v, a + n)
update (h, v, a) (Forward n) = (h + n, v + n * a, a)

main :: IO ()
main = do
    input <- fmap (map words . lines) getContents
    let (horz, vert, aim) = foldl update (0, 0, 0) $ map convert input
    print $ horz * vert
