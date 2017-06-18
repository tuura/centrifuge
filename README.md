# Graph Cruncher

An experimental graph parsing, manipulation and transformation library based on
[Algebraic graphs](http://hackage.haskell.org/package/algebraic-graphs/docs/Algebra-Graph.html)

## Features

* The `Cruncher.DSL` module provides a DSL for protein network import, transformation and FPGA embedding.
* The `Cruncher.GraphML.Parse` lets to parse GraphML files into polymorphic algebraic graph expressions.
* The `Cruncher.VHDL.PrettyPrinter` provide a VHDL-code generator for algebraic graph expressions.

## More info

This repo is meant to be an accompanying material for 'Embedding Graphs in Silicon' FDL'17 paper.

VHDL code generation is a port of a corresponding [pangraph](https://github.com/tuura/pangraph/tree/master/src/Pangraph/VHDL) module.