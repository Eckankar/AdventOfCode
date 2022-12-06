import qualified Data.Set as S

findStartMarker :: Int -> [Char] -> Int
findStartMarker l s = findStartMarker' l s
    where findStartMarker' n rs =
            if length (S.fromList $ take l rs) == l
            then n
            else findStartMarker' (n+1) $ tail rs

main :: IO ()
main = do
    input <- getContents
    print $ findStartMarker 14 input
