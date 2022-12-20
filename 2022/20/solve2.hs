import qualified Data.Sequence as Seq

rotateToPos :: Show a => Seq.Seq a -> (a -> Bool) -> Seq.Seq a
rotateToPos s p = n Seq.<| post <> pre
    where (pre, n Seq.:<| post) = Seq.spanl (not . p) s


mix :: Seq.Seq (Int, Int) -> Int -> Seq.Seq (Int, Int)
mix s i = (i, n) Seq.<| post <> pre
    where (pre, post) = Seq.splitAt pos s'
          ((_, n) Seq.:<| s') = rotateToPos s ((== i) . fst)
          l = Seq.length s'
          pos = ((n `mod` l) + l) `mod` l

main :: IO ()
main = do
    input <- fmap (map (read :: String -> Int) . lines) getContents

    let input' = Seq.fromList $ zip [0..] $ map (* 811589153) input

    let mixeds = iterate (\input -> foldl mix input [0 .. Seq.length input-1]) input'
    let mixed' = Seq.cycleTaking 3001 $ rotateToPos (mixeds !! 10) ((== 0) . snd)
    print $ sum $ map (snd . Seq.index mixed') [1000,2000,3000]
