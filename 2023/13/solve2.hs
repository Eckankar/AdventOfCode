import Data.List (transpose)
import Data.List.Extra (wordsBy)
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)

data MirrorPos = Vertical Int | Horizontal Int
    deriving (Show, Eq)

findReflection ls = refl
    where [refl] = catMaybes [ fmap Vertical   $ findReflection' ls
                             , fmap Horizontal $ findReflection' $ transpose ls ]

          findReflection' :: [[Char]] -> Maybe Int
          findReflection' ls = listToMaybe $ mapMaybe checkReflect [ 1 .. length ls - 1 ]
            where checkReflect n =
                    if sum (zipWith (\a b -> length $ filter id $ zipWith (/=) a b) end $ reverse start) == 1
                    then Just n
                    else Nothing
                    where (start, end) = splitAt n ls

score (Vertical n)   = 100 * n
score (Horizontal n) = n

main :: IO ()
main = do
    input <- fmap (wordsBy null . lines) getContents
    print $ sum $ map (score . findReflection) input
