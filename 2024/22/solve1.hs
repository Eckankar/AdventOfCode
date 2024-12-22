import Control.Parallel.Strategies
import Data.Bits (xor, shiftL)

prng :: Int -> Int
prng seed = s'''
    where s' = ((seed * 64) `xor` seed) `mod` 16777216
          s'' = (s' `xor` (s' `div` 32)) `mod` 16777216
          s''' = (s'' `xor` (s'' * 2048))  `mod` 16777216

main :: IO ()
main = do
    input <- fmap (map (read :: String -> Int) . lines) getContents

    print $ sum $ parMap rdeepseq ((!! 2000) . iterate prng) input
