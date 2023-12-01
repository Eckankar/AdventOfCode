import Data.Char (isDigit)

replaceDigits [] = []
replaceDigits ('o':'n':'e':s) = '1' : replaceDigits s
replaceDigits ('t':'w':'o':s) = '2' : replaceDigits s
replaceDigits ('t':'h':'r':'e':'e':s) = '3' : replaceDigits s
replaceDigits ('f':'o':'u':'r':s) = '4' : replaceDigits s
replaceDigits ('f':'i':'v':'e':s) = '5' : replaceDigits s
replaceDigits ('s':'i':'x':s) = '6' : replaceDigits s
replaceDigits ('s':'e':'v':'e':'n':s) = '7' : replaceDigits s
replaceDigits ('e':'i':'g':'h':'t':s) = '8' : replaceDigits s
replaceDigits ('n':'i':'n':'e':s) = '9' : replaceDigits s
replaceDigits (c:s) = c: replaceDigits s

replaceDigitsRev [] = []
replaceDigitsRev ('e':'n':'o':s) = '1' : replaceDigitsRev s
replaceDigitsRev ('o':'w':'t':s) = '2' : replaceDigitsRev s
replaceDigitsRev ('e':'e':'r':'h':'t':s) = '3' : replaceDigitsRev s
replaceDigitsRev ('r':'u':'o':'f':s) = '4' : replaceDigitsRev s
replaceDigitsRev ('e':'v':'i':'f':s) = '5' : replaceDigitsRev s
replaceDigitsRev ('x':'i':'s':s) = '6' : replaceDigitsRev s
replaceDigitsRev ('n':'e':'v':'e':'s':s) = '7' : replaceDigitsRev s
replaceDigitsRev ('t':'h':'g':'i':'e':s) = '8' : replaceDigitsRev s
replaceDigitsRev ('e':'n':'i':'n':s) = '9' : replaceDigitsRev s
replaceDigitsRev (c:s) = c: replaceDigitsRev s

main :: IO ()
main = do
    input <- fmap (lines) getContents
    let cvFirst = map (head . filter isDigit . replaceDigits) input
    let cvLast = map (head . filter isDigit . replaceDigitsRev . reverse) input

    let cvs = zipWith (\a b -> (read :: String -> Int) [a, b]) cvFirst cvLast
    print $ sum cvs
