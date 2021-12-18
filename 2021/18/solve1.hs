import Control.Applicative ((<$>), Applicative(..),
                            Alternative((<|>)))
import Data.Char (isDigit)
import Text.ParserCombinators.ReadP

data Snailfish = Pair Snailfish Snailfish | Literal Int
    deriving (Eq)

instance Read Snailfish where
    readsPrec _ = readP_to_S parseSnailfish

instance Show Snailfish where
    show (Literal n) = show n
    show (Pair x y) = '[' : show x ++ "," ++ show y ++ "]"


parseSnailfish :: ReadP Snailfish
parseSnailfish =
    (Literal . read <$> munch1 isDigit)
    <|>
    (do [x, y] <- between (char '[') (char ']') $ parseSnailfish `sepBy` char ','
        return $ Pair x y)

data Explode = Exploded Snailfish (Maybe Int) (Maybe Int)
             | Unexploded Snailfish
    deriving (Show, Eq)

explodeFish x = explode 0 x
    where explode 4 (Pair (Literal x) (Literal y))  = Exploded (Literal 0) (Just x) (Just y)
          explode _ (Literal n) = Unexploded $ Literal n
          explode n (Pair x y)  =
            case (explode (n+1) x, explode (n+1) y) of
              (Exploded x' l r, _)             -> Exploded (Pair x' (addLeft r y))   l       Nothing
              (Unexploded x', Exploded y' l r) -> Exploded (Pair (addRight l x') y') Nothing r
              (Unexploded x', Unexploded y')   -> Unexploded $ Pair x' y'

          addLeft Nothing  x           = x
          addLeft (Just r) (Literal n) = Literal $ n+r
          addLeft (Just r) (Pair x y)  = Pair (addLeft (Just r) x) y

          addRight Nothing  x           = x
          addRight (Just r) (Literal n) = Literal $ n+r
          addRight (Just r) (Pair x y)  = Pair x $ addRight (Just r) y

data Split = Split Snailfish
           | Unsplit Snailfish

splitFish (Literal n) | n >= 10 = Split $ Pair (Literal (n `div` 2)) (Literal (n `div` 2 + n `mod` 2))
                      | otherwise = Unsplit $ Literal n
splitFish (Pair x y) =
    case (splitFish x, splitFish y) of
        (Split x', _)            -> Split $ Pair x' y
        (Unsplit x', Split y')   -> Split $ Pair x' y'
        (Unsplit x', Unsplit y') -> Unsplit $ Pair x' y'


reduce x =
    case explodeFish x of
        Exploded x' _ _ -> reduce x'
        Unexploded x' ->
            case splitFish x of
                Split x'   -> reduce x'
                Unsplit x' -> x'


add x y = reduce $ Pair x y

magnitude (Literal n) = n
magnitude (Pair x y) = 3 * magnitude x + 2 * magnitude y

main :: IO ()
main = do
    s <- fmap (map (read :: String -> Snailfish) . lines) getContents
    print $ magnitude $ foldl1 add s
