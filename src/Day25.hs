module Day25 (solve1, solve2) where

import Data.Attoparsec.Text qualified as P
import Data.Char qualified as Char
import Data.Graph.Inductive qualified as G
import Data.Graph.Inductive.NodeMap qualified as G.NodeMap
import Data.Graph.Inductive.Query.MaxFlow qualified as MF
import Data.Maybe (fromJust)
import RIO
import RIO.List.Partial (head, tail)
import Prelude (print, putStrLn)

addEdge :: G.Gr Text Int -> (Text, Text) -> G.NodeMap Text -> (G.Gr Text Int, G.NodeMap Text)
addEdge g (a, b) nm =
  let (node1, nm') = G.mkNode nm a
      (node2, nm'') = G.mkNode nm' b
      edge = fromJust $ G.mkEdge nm'' (snd node1, snd node2, 1)
      edge' = fromJust $ G.mkEdge nm'' (snd node2, snd node1, 1)
      newNodes = filter (\n -> not $ G.gelem (fst n) g) [node1, node2]
   in (G.insEdges [edge, edge'] $ G.insNodes newNodes g, nm'')

-- undirected graph
parser :: P.Parser (G.Gr Text Int, G.NodeMap Text)
parser = do
  res <- P.many' $ do
    p <- P.takeWhile1 (/= ':')
    void ": "
    cs <- P.sepBy1' (P.takeWhile1 Char.isAlpha) (P.char ' ')
    P.endOfLine
    pure (p, cs)
  P.endOfInput
  let init = (G.empty, G.NodeMap.new)
  pure
    $ foldl'
      ( \acc (p, cs) ->
          foldl'
            ( \acc' i ->
                let g' = fst acc'
                    nm' = snd acc'
                 in addEdge g' (p, i) nm'
            )
            acc
            cs
      )
      init
      res

solve1 :: Text -> IO ()
solve1 input =
  let (graph, _) = case P.parseOnly parser input of
        Left err -> error err
        Right g -> g
      nodes = tail $ G.labNodes graph
      headNode = head $ G.labNodes graph
      headNodeID = fst headNode
      nodesID = map fst nodes
      targetNodeID = fst $ head $ filter (\x -> snd x == 3) $ map (\i -> (i, MF.maxFlow graph headNodeID i)) nodesID
      graph' = MF.maxFlowgraph graph headNodeID targetNodeID
      -- remove all edges with (1,1)
      graph'' = G.delEdges (map (\(a, b, _) -> (a, b)) $ filter (\(_, _, x) -> fst x == 1) $ G.labEdges graph') graph'
      reachableNodes = G.bfs headNodeID graph''
      s = length reachableNodes
      t = length (G.labNodes graph) - s
   in print (s * t)

solve2 :: Text -> IO ()
solve2 _ = putStrLn "[The end of this year's Advent of Code!]"
