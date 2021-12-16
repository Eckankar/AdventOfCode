import Data.Char (isDigit, intToDigit, digitToInt)
import Data.Maybe (mapMaybe)
import Numeric (readHex)
import Text.ParserCombinators.ReadP
import Debug.Trace (traceShow)

toBinary :: Int -> Int -> [Char]
toBinary d n = map intToDigit $ reverse $ toBinary' d n
    where toBinary' 0 n = []
          toBinary' d n = n `mod` 2 : toBinary' (d-1) (n `div` 2)

fromBinary = foldl (\a n -> a*2 + n) 0 . map digitToInt

data Packet = Packet Int PacketType
    deriving (Show, Eq)
data PacketType = Literal Int | Operator Int [Packet]
    deriving (Show, Eq)

getBits n = count n (satisfy isDigit)
intField n = fromBinary <$> getBits n

parseBitChain = do
    [b] <- getBits 1
    bs  <- getBits 4
    bs' <- if b == '0' then return [] else parseBitChain
    return $ bs ++ bs'

parseLiteral = do Literal . fromBinary <$> parseBitChain

parseSubpacketsLength = do
    char '0'
    payloadLength <- intField 15
    payloadContents <- getBits payloadLength
    return $ runParser (many1 parsePacket <* eof) payloadContents

parseSubpacketsNumber = do
    char '1'
    payloadCount <- intField 11
    count payloadCount parsePacket

parseOperator n = do
    subpackets <- choice [parseSubpacketsLength, parseSubpacketsNumber]
    return $ Operator n subpackets

parsePacketType 4 = parseLiteral
parsePacketType n = parseOperator n

parsePacket = do
    version <- intField 3
    typeid  <- intField 3
    ptype   <- parsePacketType typeid
    return $ Packet version ptype

parseInputPacket = do
    packet <- parsePacket
    many $ char '0'
    eof
    return packet

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

versions (Packet v t) = v : versions' t
    where versions' (Literal _) = []
          versions' (Operator _ ps) = concatMap versions ps

main :: IO ()
main = do
    inputBits <- fmap (concatMap (toBinary 4 . digitToInt)) getLine
    let packet = runParser parseInputPacket inputBits
    print $ sum $ versions packet
