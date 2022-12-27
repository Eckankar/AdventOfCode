import qualified Data.Set as S

nonDec = nonDec' 0
    where nonDec' _ 0 = [[]]
          nonDec' i n = concatMap (\k -> map (k:) $ nonDec' k (n-1)) [i..9]

main = do
    let cs = nonDec 6
    let cs' = filter ((<= 5) . S.size . S.fromList) cs
    let cs'' = filter ((\n -> n >= 272091 && n <= 815432) . read . concatMap show) cs'
    print $ length cs''
