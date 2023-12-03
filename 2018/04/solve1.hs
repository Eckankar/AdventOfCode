import qualified Data.Map as M

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit)
import Data.List (maximumBy, sort)
import Text.ParserCombinators.ReadP

type Date = (Int, Int, Int)
type Time = (Int, Int)
type Timestamp = (Date, Time)

data Event = ShiftStart Int | WakeUp | FallAsleep
    deriving (Eq, Show)

parseNumber :: ReadP Int
parseNumber = read <$> munch1 isDigit

parseTimestamp :: ReadP Timestamp
parseTimestamp = do
    char '['
    y <- parseNumber
    char '-'
    m <- parseNumber
    char '-'
    d <- parseNumber
    char ' '
    hr <- parseNumber
    char ':'
    min <- parseNumber
    char ']'
    return ((y, m, d), (hr, min))

parseShiftStart = do
    string "Guard #"
    id <- parseNumber
    string " begins shift"
    return $ ShiftStart id

parseWakeUp = string "wakes up" >> return WakeUp
parseFallAsleep = string "falls asleep" >> return FallAsleep

parseInputLine = do
    timestamp <- parseTimestamp
    char ' '
    event <- parseShiftStart <|> parseWakeUp <|> parseFallAsleep
    eof
    return (timestamp, event)

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

processEvents :: [(Timestamp, Event)] -> (M.Map Int Int, M.Map (Int, Int) Int)
processEvents evs = (wm, mm)
    where (wm, mm, _, _) = foldl (flip processEvent) (M.empty, M.empty, 0, Nothing) evs
          processEvent (_, ShiftStart id)          (wm, mm, _, Nothing)    = (wm,  mm,  id, Nothing)
          processEvent ((_, (_, min)), FallAsleep) (wm, mm, id, Nothing)   = (wm,  mm,  id, Just min)
          processEvent ((_, (_, min)), WakeUp)     (wm, mm, id, Just min') = (wm', mm', id, Nothing)
            where wm' = M.alter (increaseBy (min - min')) id wm
                  mm' = foldr (M.alter (increaseBy 1)) mm [ (id, m) | m <- [min' .. (min-1)] ]

                  increaseBy m Nothing  = Just m
                  increaseBy m (Just n) = Just $ n+m

main :: IO ()
main = do
    input <- fmap (map (runParser parseInputLine) . sort . lines) getContents
    let (wakeMap, minuteMap) = processEvents input

    let mostSleepy = fst $ maximumBy (\(_, w) (_, w') -> compare w w') $ M.toList wakeMap
    let bestMinute = snd $ fst $ maximumBy (\(_, t) (_, t') -> compare t t') $ filter ((== mostSleepy) . fst . fst) $ M.toList minuteMap

    print $ mostSleepy * bestMinute
