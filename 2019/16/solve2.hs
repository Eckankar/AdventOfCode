import qualified Data.Array as A
import Control.DeepSeq (force)
import Debug.Trace (traceShow)

fft :: ([Int], Int) -> ([Int], Int)
fft (vs, i) = traceShow i (map applyKey [1 .. l], i+1)
    where l = length vs
          csum = A.listArray (0, l) $ scanl (+) 0 vs
          applyKey i = applyKey' 1 i 0
              where applyKey' m o v
                      | o > l = abs (v `rem` 10)
                      | otherwise = applyKey' (-m) (o + 2*i) $ v + m * rangeSum o o'
                        where o' = min l $ o+i-1
          rangeSum i j = (csum A.! j) - (csum A.! (i-1))

iter 0 _ v = v
iter n f v = iter (n-1) f v'
    where v' = force $ f v

main :: IO ()
main = do
    input <- fmap (map ((read :: String -> Int) . (:[]))) getLine
    let (rv, _) = iter 100 fft $ (\v -> (v, 0)) $ concat $ replicate 10000 input

    let getAt i d v = foldl1 (\a d -> 10*a+d) $ take d $ drop i v
    let offset = getAt 0 7 input

    print $ getAt offset 8 rv
