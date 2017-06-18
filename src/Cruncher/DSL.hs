module Cruncher.DSL ( readGraphML
                    , print
                    , mergeVertices
                    , splitVertex
                    , induce
                    ) where

import Prelude hiding (print)
import Data.Foldable (elem)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Char8            as BSC8 (unpack)
import qualified Algebra.Graph                    as G
import Cruncher.GraphML.Parser (parseGraphML)
import Cruncher.VHDL.PrettyPrinter (generateGraphVHDL)
import Cruncher.Types (Network (..), Protein)

-- | Read and parse a GraphML file describing a network
readGraphML :: FilePath -> IO Network
readGraphML path = do
  input <- BS.readFile path
  case parseGraphML input of
    Left err -> error (BSC8.unpack err)
    Right graph -> return $ graph

-- | Print a network as an edge list
print :: Network -> IO ()
print = putStrLn . show . G.edgeList

-- | Merge a list of proteins into a single protein complex
mergeVertices :: [Protein] -> Protein -> Network -> Network
mergeVertices xs = G.mergeVertices (\x -> x `elem` xs)

-- Split a protein complex into a list of proteins
splitVertex :: Protein -> [Protein] -> Network -> Network
splitVertex = G.splitVertex

-- Compute the subgraph induced by a given protein predicate
induce :: (Protein -> Bool) -> Network -> Network
induce = G.induce

-- | Synthesise a network into a hardware circuit,
--   write the result to a VHDL file
writeVHDL :: Network -> FilePath -> IO ()
writeVHDL g path = BS.writeFile path $ generateGraphVHDL g