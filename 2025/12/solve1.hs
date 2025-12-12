import qualified Data.Map as M
import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

import Data.List (transpose)

type Point = (Int, Int)
type Piece = (Int, [M.Map Point Char])
type Region = ((Int, Int), [Int])

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- munch1 isDigit
    return $ read $ sign : digits

parseRow :: ReadP String
parseRow = many1 (satisfy (\c -> c == '.' || c == '#')) <* char '\n'

parsePiece :: ReadP Piece
parsePiece = do
    number <- parseInt
    string ":\n"
    row1 <- parseRow
    row2 <- parseRow
    row3 <- parseRow

    let orig = [row1, row2, row3]
    let rots = map (M.filter (== '#') . toMap) $ iterate rotText orig
    return (number, take 4 rots)
        where rotText = reverse . transpose

parseRegion :: ReadP Region
parseRegion = do
    dimX <- parseInt
    char 'x'
    dimY <- parseInt
    string ": "
    counts <- sepBy1 parseInt (char ' ')
    return ((dimX, dimY), counts)


parseInput :: ReadP ([Piece], [Region])
parseInput = do
    pieces <- sepBy1 (parsePiece) (string "\n")
    string "\n"
    regions <- sepBy (parseRegion) (string "\n")
    eof
    return (pieces, regions)

runParser p = fst . head . filter (null . snd) . readP_to_S p

toMap :: [String] -> M.Map Point Char
toMap = M.fromList . concatMap (\(i, r) -> map (\(j, c) -> ((i, j), c)) $ zip [0..] r) . zip [0..]

add (x, y) (x', y') = (x + x', y + y')

movePiece :: M.Map Point Char -> Point -> M.Map Point Char
movePiece m p = M.mapKeys (add p) m

canSatisfyRegion :: [Piece] -> Region -> Bool
canSatisfyRegion pieces ((width, height), counts)
    -- if we have too little room for the pieces, easily reject
    | availableTiles < neededTiles = False
    -- if we can place the pieces next to each other without interlocking, easily accept
    | availableSimpleSlots >= sum counts = True
    -- otherwise, we have to search
    -- (as it turns out, the input doesn't actually hit this! well, that makes it easier...)
    | otherwise = tryPlacing M.empty $ zip counts $ map snd pieces
    where availableTiles = width * height
          neededTiles = sum $ zipWith (*) counts $ map (M.size . head . snd) pieces
          availableSimpleSlots = (width `div` 3) * (height `div` 3)

          tryPlacing :: M.Map Point Char -> [(Int, [M.Map Point Char])] -> Bool
          tryPlacing _ [] = True
          tryPlacing m ((0, _):rs) = tryPlacing m rs
          tryPlacing m ((count, ps):rs) =
            or [ tryPlacing m' ((count-1, ps):rs) | m' <- availPlacements ]
            where -- we know the middle is filled out in all pieces; so if that one's filled out, skip right away
                  availCoords = S.toList $ S.difference allCoords $ S.map (add (-1, -1)) $ M.keysSet m
                  allCoords = S.fromList [(x, y) | x <- [0 .. width - 3], y <- [0 .. height - 3]]
                  availPlacements =
                    [ M.union m p
                    | c <- availCoords
                    , p' <- ps
                    , let p = M.mapKeys (add c) p'
                    , (== 0) . M.size $ M.intersection m p
                    ]


main :: IO ()
main = do
    (pieces, regions) <- fmap (runParser parseInput) getContents
    print $ length $ filter id $ map (canSatisfyRegion pieces) regions