{-# LANGUAGE RankNTypes, TypeFamilies #-}

module Cruncher.Types (GraphMLSource, Protein, Network) where

import qualified Algebra.Graph       as G
import qualified Data.ByteString     as BS

-- | GraphML source file
type GraphMLSource = BS.ByteString

-- | Proteins
type Protein = BS.ByteString

-- | Protein network
type Network = G.Graph Protein
