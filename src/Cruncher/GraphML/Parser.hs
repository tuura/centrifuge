{-# LANGUAGE OverloadedStrings, TypeFamilies #-}

module Cruncher.GraphML.Parser (parseGraphML) where

import qualified Data.ByteString                  as BS
import qualified Text.XML.Hexml                   as XML
import qualified Algebra.Graph.Class              as C
import qualified Algebra.Graph.HigherKinded.Class as H
import qualified Algebra.Graph                    as G

import Data.List (partition)

import Cruncher.Types

-- | Parse GraphML file into a polymorphic graph expression
parseGraphML :: (C.Graph g, C.Vertex g ~ Protein) =>
                GraphMLSource -> Either BS.ByteString g
parseGraphML input = do
  -- retrieve xml root node
  tree <- XML.parse input
  -- dive two levels deeper, to the 'graph' tag
  graph <- extractGraphNode tree
  -- partition graph's children into nodes ant the rest (edges)
  let (rawVertices, rawEdges) =
        partition (\x -> XML.name x == "node") . XML.children $ graph
  -- turn xml entities into Haskell types
  vertices <- traverse extractVertex rawVertices
  edges    <- traverse extractEdge rawEdges
  -- construct the resulting graph
  return $ C.overlay (C.vertices vertices) (C.edges edges)
  where
    extractGraphNode :: XML.Node -> Either BS.ByteString XML.Node
    extractGraphNode root =
      case XML.childrenBy root "graphml" of
        [] ->  Left "Invalid GraphML: no 'graphml' tag"
        x:_ -> case XML.childrenBy x "graph" of
          [] -> Left "Invalid GraphML: no 'graph' tag"
          x:_ -> Right x

    extractVertex :: XML.Node -> Either BS.ByteString Protein
    extractVertex node =
      case XML.attributeValue <$> XML.attributeBy node "id" of
        Nothing -> Left "Invalid GraphML: a vertex doesn't have an 'id' \
                        \ attribute"
        Just n  -> Right n

    extractEdge :: XML.Node -> Either BS.ByteString (Protein, Protein)
    extractEdge node =
      case sequence . map (fmap XML.attributeValue . XML.attributeBy node) $
                      ["source", "target"] of
          Just [source, target] -> Right $ (source, target)
          _ -> Left "Invalid GraphML: an edge doesn't have a 'source' or \
                    \'target' attribute"
