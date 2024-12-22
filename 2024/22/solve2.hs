import qualified Data.Map as M
import qualified Data.Set as S

import Control.Parallel.Strategies
import Data.Bits (xor, shiftL)

prng :: Int -> Int
prng seed = s'''
    where s' = ((seed * 64) `xor` seed) `mod` 16777216
          s'' = (s' `xor` (s' `div` 32)) `mod` 16777216
          s''' = (s'' `xor` (s'' * 2048))  `mod` 16777216

computeChangeMap :: [Int] -> M.Map (Int, Int, Int, Int) Int
computeChangeMap nums = computeChangeMap' nums M.empty
    where computeChangeMap' (x1:x2:x3:x4:x5:xs) m = computeChangeMap' (x2:x3:x4:x5:xs) m'
              where (d1, d2, d3, d4) = (x2-x1, x3-x2, x4-x3, x5-x4)
                    m' = M.alter addIfNew (d1, d2, d3, d4) m
                    addIfNew Nothing = Just x5
                    addIfNew (Just v) = Just v
          computeChangeMap' _ m = m

main :: IO ()
main = do
    input <- fmap (map (read :: String -> Int) . lines) getContents

    let cms = parMap rdeepseq (computeChangeMap . map (`mod` 10) . take 2001 . iterate prng) input

    let keySets = S.toList $ S.unions $ map M.keysSet cms
    let prices = [ sum $ map (M.findWithDefault 0 k) cms | k <- keySets ]
    print $ maximum prices
