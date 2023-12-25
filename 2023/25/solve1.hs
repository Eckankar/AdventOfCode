import qualified Data.Map as M
import qualified Data.Set as S

import Control.Applicative ((<$>), Applicative(..), Alternative((<|>)))
import Data.Char (isLetter)
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.DFS
import Data.Graph.Inductive.Query.MaxFlow
import Data.Tuple (swap)
import System.Process (callCommand)
import Text.ParserCombinators.ReadP

parseLabel :: ReadP String
parseLabel = munch1 isLetter

parseInputLine :: ReadP [(String, String)]
parseInputLine = do
    from <- parseLabel
    string ": "
    tos <- sepBy parseLabel (char ' ')
    return $ map (from, ) tos


runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

main :: IO ()
main = do
    input <- fmap (map (runParser (parseInputLine <* eof)) . lines) getContents

    let input' = S.fromList $ concat input

    let nodeMap = M.fromList $ zip (S.toList $ S.union (S.map fst input') (S.map snd input')) [1..]
    let nodes = map (\n -> (n, (0::Int))) $ M.elems nodeMap
    
    let edges = map (\(a, b) -> (nodeMap M.! a, nodeMap M.! b, 1)) $ S.toList input'
    let graph = undir $ (mkGraph (nodes :: [LNode Int]) (edges :: [LEdge Int])) :: Gr Int Int

    let edges' = map (\(a, b) -> (nodeMap M.! a, nodeMap M.! b, (1, 0, 1))) $ S.toList input'
    let graph' = undir $ (mkGraph (nodes :: [LNode Int]) (edges' :: [LEdge (Int, Int, Int)])) :: Gr Int (Int, Int, Int)

    let ((nFrom, nTo):_) = [ (a, b) | a <- M.elems nodeMap, b <- reverse $ M.elems nodeMap, a /= b, maxFlow graph a b == 3 ]

    let mfGraph = mfmg graph' nFrom nTo

    let deadEdges = S.fromList $ concatMap (\(a, b, (v, _, _)) -> [(a,b,1), (b,a,1)]) $ filter (\(_, _, (_, _, n)) -> n == 0) $ labEdges mfGraph
    let edges'' = S.toList $ S.difference (S.fromList edges) deadEdges
    let graph'' = undir $ (mkGraph (nodes :: [LNode Int]) (edges'' :: [LEdge Int])) :: Gr Int Int

    let invNodeMap = M.fromList $ map swap $ M.toList nodeMap
    let nReachable = length $ reachable nFrom graph''
    print $ nReachable * (M.size nodeMap - nReachable)
