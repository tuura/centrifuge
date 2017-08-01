-----------------------------------------------------------------------------
-- |
-- Module     : Centrifuge.DSL.Generic
-- Copyright  : (c) Georgy Lukyanov 2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : georgiylukjanov@gmail.com
-- Stability  : unstable
--
-- This module provides a DSL for graph import,
-- transformation and FPGA embedding.
-----------------------------------------------------------------------------

{-# LANGUAGE RankNTypes, TypeFamilies, GeneralizedNewtypeDeriving
           , DeriveFunctor, DeriveFoldable, DeriveTraversable
           , ScopedTypeVariables #-}

module Centrifuge.DSL.Generic
  ( Network
  , readNetwork
  , print
  , mergeVertices
  , splitVertex
  , induce
  ) where

import Data.Foldable (elem)
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Char8            as BSC8 (unpack)
import qualified Algebra.Graph                    as G
import qualified Algebra.Graph.Class              as C
import qualified Algebra.Graph.HigherKinded.Class as H
import qualified Algebra.Graph.AdjacencyMap       as AM
import Centrifuge.GraphML.Parser (parseGraphML)
import Centrifuge.VHDL.PrettyPrinter (generateGraphVHDL)

-- | A network
newtype Network a = Network { fromNetwork :: G.Graph a }
  deriving ( Foldable, Applicative, Alternative
           , Monad, MonadPlus, Functor, Traversable
           , H.Graph, H.ToGraph)

instance (Show a, Ord a) => Show (Network a) where
  show g = show $ (C.toGraph g :: AM.AdjacencyMap a)

-- A type for raw network with nodes being serialised as strings
type RawNetwork = Network BS.ByteString

-- TODO: To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance C.Graph (Network a) where
  type Vertex (Network a) = a
  empty   = H.empty
  vertex  = H.vertex
  overlay = H.overlay
  connect = H.connect

-- TODO: To be derived automatically using GeneralizedNewtypeDeriving in GHC 8.2
instance C.ToGraph (Network a) where
  type ToVertex (Network a) = a
  toGraph (Network g) = G.foldg C.empty C.vertex C.overlay C.connect g

-- | Read and parse a GraphML file describing a network leaving nodes unparsed.
readRawNetwork :: FilePath -> IO RawNetwork
readRawNetwork path = do
  input <- BS.readFile path
  case parseGraphML input of
    Left err -> error (BSC8.unpack err)
    Right graph -> return $ graph

-- | Read and parse a GraphML file describing a network, parse node values using
--   a supplied function.
readNetwork :: (BS.ByteString -> a) -> FilePath -> IO (Network a)
readNetwork nodeParser path = do
  raw <- readRawNetwork path
  return $ nodeParser <$> raw

-- | Merge a list of proteins into a single protein complex
mergeVertices :: Eq a => [a] -> a -> (Network a) -> (Network a)
mergeVertices xs = H.mergeVertices (\x -> x `elem` xs)

-- | Split a protein complex into a list of proteins
splitVertex :: Eq a => a -> [a] -> Network a -> Network a
splitVertex = H.splitVertex

-- | Compute the subgraph induced by a given protein predicate
induce :: (a -> Bool) -> (Network a) -> (Network a)
induce = H.induce

-- | Synthesise a network into a hardware circuit,
--   write the result to a VHDL file
writeVHDL :: (Ord a, Show a) => Network a -> FilePath -> IO ()
writeVHDL g path = BS.writeFile path $ generateGraphVHDL (fromNetwork g)