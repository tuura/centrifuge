-----------------------------------------------------------------------------
-- |
-- Module     : Cruncher.Protein
-- Copyright  : (c) Georgy Lukyanov 2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : georgiylukjanov@gmail.com
-- Stability  : unstable
--
-- This module provides a DSL for protein network import,
-- transformation and FPGA embedding.
-----------------------------------------------------------------------------

{-# LANGUAGE RankNTypes, TypeFamilies, GeneralizedNewtypeDeriving
           , DeriveFunctor, DeriveFoldable, DeriveTraversable
           , ScopedTypeVariables #-}


module Cruncher.Protein
  ( readGraphML
  , print
  , mergeVertices
  , splitVertex
  , induce
  ) where

-- import Prelude hiding (print)
import Data.Foldable (elem)
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Char8            as BSC8 (unpack)
import qualified Algebra.Graph                    as G
import qualified Algebra.Graph.Class              as C
import qualified Algebra.Graph.HigherKinded.Class as H
import qualified Algebra.Graph.AdjacencyMap       as AM
import Cruncher.GraphML.Parser (parseGraphML)
import Cruncher.VHDL.PrettyPrinter (generateGraphVHDL)

-- | Proteins
type Protein = BS.ByteString

-- | A network
newtype Network a = Network { fromNetwork :: G.Graph a }
  deriving ( Foldable, Applicative, Alternative
           , Monad, MonadPlus, Functor, Traversable
           , H.Graph, H.ToGraph)

instance (Show a, Ord a) => Show (Network a) where
  show g = show $ (C.toGraph g :: AM.AdjacencyMap a)

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

-- | Read and parse a GraphML file describing a network
readGraphML :: FilePath -> IO (Network Protein)
readGraphML path = do
  input <- BS.readFile path
  case parseGraphML input of
    Left err -> error (BSC8.unpack err)
    Right graph -> return $ graph

-- -- | Print a network
-- print :: (Network Protein) -> IO ()
-- print g = putStrLn . show $ (C.toGraph g :: AM.AdjacencyMap Protein)

-- | Merge a list of proteins into a single protein complex
mergeVertices :: [Protein] -> Protein -> (Network Protein) -> (Network Protein)
mergeVertices xs = H.mergeVertices (\x -> x `elem` xs)

-- | Split a protein complex into a list of proteins
splitVertex :: Protein -> [Protein] -> Network Protein -> Network Protein
splitVertex = H.splitVertex

-- | Compute the subgraph induced by a given protein predicate
induce :: (Protein -> Bool) -> (Network Protein) -> (Network Protein)
induce = H.induce

-- | Synthesise a network into a hardware circuit,
--   write the result to a VHDL file
writeVHDL :: (Network Protein) -> FilePath -> IO ()
writeVHDL g path = BS.writeFile path $ generateGraphVHDL (fromNetwork g)