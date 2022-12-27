main :: IO ()
main = fmap (sum . map ((\n -> n `div` 3 - 2) . read) . lines) getContents >>= print
