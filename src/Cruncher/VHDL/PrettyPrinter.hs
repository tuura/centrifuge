{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
--
-- This module defines a VHDL-code generator to represent graph circuits in VHDL
-- For now it's essentially a port of `pangraph`'s VHDL code generator.
--
-----------------------------------------------------------------------------

module Cruncher.VHDL.PrettyPrinter (generateGraphVHDL) where

import qualified Algebra.Graph         as G
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC8 (pack, unpack)
import Data.Monoid ((<>))

-- | Synthesise a graph into a hardware circuit
generateGraphVHDL :: (Ord a, Show a) => G.Graph a -> BS.ByteString
generateGraphVHDL g =
  let vertexCount = G.vertexCount g
      stats = "-- Nodes: " <> (BSC8.pack . show $ vertexCount) <>
              " - Edges: " <> (BSC8.pack . show . G.edgeCount $ g) <> "\n"
      library     = createLibrary
      entity      = createEntity vertexCount
      archOpen    = openArchitecture vertexCount
      regs        = "\t-- Registers\n" <> bindRegisters (G.vertexList g) 0
      wiresIn     = "\t-- Wire connections: inputs\n" <> bindWiresIn g
      wiresOut    = "\t-- Wire connections: outputs\n" <>
                    bindWiresOut (vertexCount - 1)
      archClose   = closeArchitecture
    in stats
       <> library
       <> entity
       <> archOpen
       <> regs
       <> wiresIn
       <> wiresOut
       <> archClose

createLibrary :: BS.ByteString
createLibrary = "-- File generated automatically\n\
                \LIBRARY ieee;\n\
                \USE ieee.std_logic_1164.all;\n\n\
                \LIBRARY work;\n\n"

createEntity :: Int -> BS.ByteString
createEntity vertexCount =
     "ENTITY Graph IS\n"
  <> "\tPORT (\n"
  <> "\t\tCLK\t: IN\tstd_logic;\n"
  <> "\t\tRST\t: IN\tstd_logic;\n"
  <> "\t\tEN\t: IN\tstd_logic_vector(" <> threshold <> " downto 0);\n"
  <> "\t\tDIN\t: IN\tstd_logic_vector(" <> threshold <> " downto 0);\n"
  <> "\t\tDOUT\t: OUT\tstd_logic_vector(" <> threshold <> " downto 0));\n"
  <> "END Graph;\n\n"
  where
    threshold = BSC8.pack . show $ vertexCount - 1

openArchitecture :: Int -> BS.ByteString
openArchitecture vertexCount =
  let open     = "ARCHITECTURE Graph_circuit OF Graph IS\n\n"
      register = createRegister
      sigs     = createSignals vertexCount
      begin    = "BEGIN\n\n"
  in open
  <> register
  <> sigs
  <> begin

closeArchitecture :: BS.ByteString
closeArchitecture = "\nEND Graph_circuit;\n"

createRegister :: BS.ByteString
createRegister = "\tCOMPONENT ffd IS\n\
                 \\t\tPORT (\n\
                 \\t\t\tCLK\t: IN\tstd_logic;\n\
                 \\t\t\tRST\t: IN\tstd_logic;\n\
                 \\t\t\tEN\t: IN\tstd_logic;\n\
                 \\t\t\tD\t: IN\tstd_logic;\n\
                 \\t\t\tQ\t: OUT\tstd_logic\n\
                 \\t\t);\n\
                 \\tEND COMPONENT;\n\n"

createSignals :: Int -> BS.ByteString
createSignals vertexCount =
  let sigsIn  = "\tSIGNAL data_in  : STD_LOGIC_VECTOR(" <> threshold <>
                " downto 0);\n"
      sigsOut = "\tSIGNAL data_out : STD_LOGIC_VECTOR(" <> threshold <>
                " downto 0);\n\n"
  in sigsIn <> sigsOut
  where
    threshold = BSC8.pack . show $ vertexCount - 1

bindRegisters :: Show a => [a] -> Int -> BS.ByteString
bindRegisters [] _     = "\n"
bindRegisters (n:ns) i = bindRegister n i <> bindRegisters ns (i+1)

bindRegister :: Show a => a -> Int -> BS.ByteString
bindRegister name i =
     "\tREG_" <> (BSC8.pack . show $ name) <> " : ffd PORT MAP (\n"
  <> "\t\tCLK\t=>\tCLK,\n"
  <> "\t\tRST\t=>\tRST,\n"
  <> "\t\tEN\t=>\tEN(" <> (BSC8.pack $ show i) <> "),\n"
  <> "\t\tD\t=>\tdata_in(" <> (BSC8.pack $ show i) <> "),\n"
  <> "\t\tQ\t=>\tdata_out(" <> (BSC8.pack $ show i) <> "));\n"

bindWiresOut :: Int -> BS.ByteString
bindWiresOut 0 = "\tDOUT(0) <= data_out(0);\n"
bindWiresOut n =
  "\tDOUT(" <> nStr <>
  ") <= data_out(" <> nStr <> ");\n" <> bindWiresOut (n - 1)
  where
    nStr = BSC8.pack . show $ n

bindWiresIn :: (Ord a, Show a) => G.Graph a -> BS.ByteString
bindWiresIn g =
  let vertices = G.vertexList g
      edges    = G.edgeList g
  in getStructure vertices vertices edges

getStructure :: (Eq a, Show a) => [a] -> [a] -> [(a, a)] -> BS.ByteString
getStructure []     _     es = "\n"
getStructure (n:ns) nodes es =
     "\tdata_in(" <> (BSC8.pack $ show i) <>
     ") <=\tDIN(" <> (BSC8.pack $ show i) <> ")"
  <> getInput (getConnections n nodes es)
  <> getStructure ns nodes es
  where
      i = getIndex n nodes 0

getInput :: [Int] -> BS.ByteString
getInput []       = ";\n"
getInput (ni:nis) =
  "\n\t\t\tOR data_out(" <> (BSC8.pack $ show ni) <> ")" <> getInput nis

getIndex :: (Eq a, Show a) => a -> [a] -> Int -> Int
getIndex nn [] _ =
  error $ "Node " ++ show nn ++ " is not present in the graph"
getIndex n1 (n2:ns) i
    | n1 == n2  = i
    | otherwise = getIndex n1 ns (i + 1)

getConnections :: (Eq a, Show a) => a -> [a] -> [(a, a)] -> [Int]
getConnections _ _ []         = []
getConnections name ns ((source, target):es)
    | name == source = getIndex target ns 0 : getConnections name ns es
    | name == target = getIndex source ns 0 : getConnections name ns es
    | otherwise      = getConnections name ns es
