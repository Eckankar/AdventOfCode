
main :: IO ()
main = do
    nums <- fmap (map read . words . map deComma) getLine
    print $ length $ iterate nextGen nums !! 80

        where deComma ',' = ' '
              deComma c   = c

              nextGen = concatMap next
              next 0 = [6, 8]
              next n = [n-1]
