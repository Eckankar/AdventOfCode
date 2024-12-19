import qualified Data.Map as M
import qualified Data.MultiSet as MS

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.List.Split (splitOn)
import Text.ParserCombinators.ReadP

import Debug.Trace (traceShow)

data Trie = Trie Bool (M.Map Char Trie)
    deriving (Show, Ord, Eq)

emptyTrie = Trie False M.empty

addToTrie t s = add' t s
    where add' (Trie _ m) []     = Trie True m
          add' (Trie b m) (s:ss) = Trie b $ M.alter update s m
            where t' = M.alter update s m
                  update mt = Just $ add' (toTrie mt) ss

                  toTrie Nothing = emptyTrie
                  toTrie (Just t) = t

buildTrie = foldl addToTrie emptyTrie

findParse :: String -> Trie -> MS.MultiSet Trie -> Int
findParse [] tStart tCurs = MS.size $ MS.filter (\(Trie b _) -> b) tCurs
findParse (c:cs) tStart tCurs = findParse cs tStart tCurs'
    where tCurs' = MS.concatMap step tCurs

          step :: Trie -> [Trie]
          step (Trie True m)  = step tStart ++ step (Trie False m)
          step (Trie False m) =
            case M.lookup c m of
                Nothing -> []
                Just t' -> [t']

main :: IO ()
main = do
    pats:_:designs <- fmap lines getContents

    let trie = buildTrie $ splitOn ", " pats
    print $ sum $ map (\s -> findParse s trie (MS.singleton trie)) designs
