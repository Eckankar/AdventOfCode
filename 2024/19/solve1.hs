import qualified Data.Map as M
import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.List.Split (splitOn)
import Text.ParserCombinators.ReadP

data Trie = Trie Bool (M.Map Char Trie)
    deriving (Show, Ord, Eq)

emptyTrie = Trie False M.empty

addToTrie t s = add' t s
    where add' (Trie _ m) []     = Trie True m
          add' (Trie b m) (s:ss) = Trie b $ M.alter update s m
            where update mt = Just $ add' (toTrie mt) ss

                  toTrie Nothing = emptyTrie
                  toTrie (Just t) = t

buildTrie = foldl addToTrie emptyTrie

findParse :: String -> Trie -> S.Set Trie -> Bool
findParse [] tStart tCurs = not $ S.null $ S.filter (\(Trie b _) -> b) tCurs
findParse (c:cs) tStart tCurs = findParse cs tStart tCurs'
    where tCurs' = S.unions $ S.map step tCurs

          step :: Trie -> S.Set Trie
          step (Trie True m)  = S.union (step tStart) $ step (Trie False m)
          step (Trie False m) =
            case M.lookup c m of
                Nothing -> S.empty
                Just t' -> S.singleton t'

main :: IO ()
main = do
    pats:_:designs <- fmap lines getContents

    let trie = buildTrie $ splitOn ", " pats
    print $ length $ filter (\s -> findParse s trie (S.singleton trie)) designs
