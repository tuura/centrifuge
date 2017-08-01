-----------------------------------------------------------------------------
-- |
-- Module     : Centrifuge.DSL.Protein
-- Copyright  : (c) Georgy Lukyanov 2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : georgiylukjanov@gmail.com
-- Stability  : unstable
--
-- This module provides a DSL for protein network import,
-- transformation and FPGA embedding.
-----------------------------------------------------------------------------

module Centrifuge.DSL.Protein
  ( readProteinNetwork
  ) where


import qualified Data.ByteString as BS
import Centrifuge.DSL.Generic

-- | Proteins
type Protein = BS.ByteString

parseProtein :: BS.ByteString -> Protein
parseProtein = id

readProteinNetwork :: FilePath -> IO (Network Protein)
readProteinNetwork = readNetwork parseProtein
