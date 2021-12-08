import qualified Data.Map as M
import qualified Data.Set as S

data SevenSegmentReq = Length Int | SegmentsOn (S.Set Char) | NotSegmentsOn (S.Set Char)
    deriving (Show, Eq)

findDigit :: [S.Set Char] -> [SevenSegmentReq] -> S.Set Char
findDigit digits reqs = res
    where [res] = foldl (\ds r -> filter (checkReq r) ds) digits reqs
          checkReq (Length n)         digit = S.size digit == n
          checkReq (SegmentsOn ss)    digit = S.intersection digit ss == ss
          checkReq (NotSegmentsOn ss) digit = S.intersection digit ss /= ss

findDigits digits = M.fromList allDigits
    where one   = findDigit digits [Length 2]
          seven = findDigit digits [Length 3]
          four  = findDigit digits [Length 4]
          eight = findDigit digits [Length 7]
          segmentsBD = S.difference four one
          five  = findDigit digits [Length 5, SegmentsOn segmentsBD]
          three = findDigit digits [Length 5, SegmentsOn seven]
          two   = findDigit digits [Length 5, NotSegmentsOn five, NotSegmentsOn three]
          nine  = findDigit digits [Length 6, SegmentsOn seven, SegmentsOn four]
          zero  = findDigit digits [Length 6, SegmentsOn seven, NotSegmentsOn four]
          six   = findDigit digits [Length 6, NotSegmentsOn seven]

          allDigits = [(one, 1), (two, 2), (three, 3), (four, 4), (five, 5),
                       (six, 6), (seven, 7), (eight, 8), (nine, 9), (zero, 0)]


solveLine (allDigits, output) = map (digitMap M.!) output
    where digitMap = findDigits allDigits

parseLine l = (map digitfy allDigits, map digitfy output)
    where (allDigits, (_:output)) = span (/= "|") l
          digitfy = S.fromList

main :: IO ()
main = do
    lines <- fmap (map words . lines) getContents
    let desiredDigits = S.fromList [1, 4, 7, 8]
    print $ length $ filter (flip S.member desiredDigits) $ concatMap (solveLine . parseLine) lines
