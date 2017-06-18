{-# LANGUAGE OverloadedStrings, TypeApplications #-}

module Main where

import qualified Data.ByteString as BS
import qualified Algebra.Graph   as G
import qualified Algebra.Graph.Fold   as F
import qualified Algebra.Graph.HigherKinded.Class   as H

import System.Environment (getArgs)

import Cruncher.Types
import Cruncher.GraphML.Parser
import Cruncher.VHDL.PrettyPrinter

import Control.Monad (ap)

-- triangle :: H.Graph g => g (String, Int)
-- triangle = (("John", 23) `H.connect` ("Jane", 22)) `H.overlay`
--            (("Jane", 22) `H.connect` ("Jack", 45))

-- triangle :: H.Graph g => g Int
-- triangle = (23 `H.connect` 22) `H.overlay`
--            (22 `H.connect` 45)

-- type Protein = String

-- data Protein = Type1 | Type2 | Type3 deriving (Show, Eq, Ord)

isDiseaseRelevant :: String -> Bool
isDiseaseRelevant "A" = False
isDiseaseRelevant "B" = True
isDiseaseRelevant "C" = True
isDiseaseRelevant "D" = False
isDiseaseRelevant "E" = True
isDiseaseRelevant _   = False


graph :: G.Graph Protein
graph = G.edges [("A", "B"), ("B", "C"), ("B", "D"), ("C", "E"), ("D", "E")]

mergeExample = G.simplify $
  G.mergeVertices (\x -> x == "C" || x == "D") "CD" graph

splitExample = G.simplify $ G.splitVertex "CD" ["C","D"] $ mergeExample

-- data Graph a = Empty
--              | Vertex a
--              | Overlay (Graph a) (Graph a)
--              | Connect (Graph a) (Graph a) deriving (Show)

-- instance Functor Graph where
--   fmap _ Empty         = Empty
--   fmap f (Vertex x)    = Vertex (f x)
--   fmap f (Overlay x y) = Overlay (fmap f x) (fmap f y)
--   fmap f (Connect x y) = Connect (fmap f x) (fmap f y)

-- foldg :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Graph a -> b
-- foldg e v o c = go
--   where
--     go Empty         = e
--     go (Vertex x)    = v x
--     go (Overlay x y) = o (go x) (go y)
--     go (Connect x y) = c (go x) (go y)

-- instance Applicative Graph where
--   pure  = Vertex
--   (<*>) = ap

-- instance Monad Graph where
--   return = Vertex
--   g >>= f = foldg Empty f Overlay Connect g

-- induce :: (a -> Bool) -> Graph a -> Graph a
-- induce p g = g >>=
--   \v -> if p v then Vertex v else Empty

main :: IO ()
main = undefined

-- main :: IO ()
-- main = do
--   input <- head <$> getArgs >>= BS.readFile
--   case parseGraphML @G.Graph input of
--     Left err -> print err
--     Right graph -> print graph
--     -- Right graph -> BS.writeFile "out.vhdl" $ writeGraphVhdl graph

