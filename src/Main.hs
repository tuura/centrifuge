{-# LANGUAGE OverloadedStrings, TypeApplications #-}

module Main where

import qualified Data.ByteString as BS
import qualified Algebra.Graph   as G

import System.Environment (getArgs)

import Cruncher.GraphML.Parser
import Cruncher.VHDL.PrettyPrinter

main :: IO ()
main = do
  input <- head <$> getArgs >>= BS.readFile
  case parseGraphML @G.Graph input of
    Left err -> print err
    Right graph -> BS.writeFile "out.vhdl" $ writeGraphVhdl graph