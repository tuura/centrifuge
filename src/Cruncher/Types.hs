module Cruncher.Types (Protein, Netlist) where

import qualified Data.ByteString as BS

type Protein = BS.ByteString

type Netlist = BS.ByteString