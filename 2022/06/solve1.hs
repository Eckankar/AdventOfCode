import qualified Data.Set as S

findStartMarker :: [Char] -> Int
findStartMarker s = findStartMarker' 4 s
    where findStartMarker' n (a:b:c:d:rs) =
            if length (S.fromList [a,b,c,d]) == 4
            then n
            else findStartMarker' (n+1) (b:c:d:rs)

main :: IO ()
main = do
    input <- getContents
    print $ findStartMarker input
