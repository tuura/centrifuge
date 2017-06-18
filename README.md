# Graph Cruncher

An experimental graph parsing, manipulation and transformation library based on
[Algebraic graphs](http://hackage.haskell.org/package/algebraic-graphs/docs/Algebra-Graph.html)

## Features

* The `Cruncher.DSL` module provides a DSL for protein network import, transformation and FPGA embedding.
* The `Cruncher.GraphML.Parse` lets to parse GraphML files into polymorphic algebraic graph expressions.
* The `Cruncher.VHDL.PrettyPrinter` provide a VHDL-code generator for algebraic graph expressions.

## Example GHCi session

Enable OverloadedStrings extension for ByteString literals

```
> :set -XOverloadedStrings
```

Import the network from GraphML file

```
> g1 <- readGraphML "examples/graphs/fdl_17.graphml"
> print g1
[("A","B"), ("B","C"), ("B","D"), ("C","E"), ("D","E")]
```

Vertex merging

```
> g2 = mergeVertices ["C","D"] "CD" g1
> print g2
[("A","B"), ("B","CD"), ("CD","E")]
```

Vertex splitting

```
> g3 = splitVertex "CD" ["C","D"] g2
> print g3
[("A","B"), ("B","C"), ("B","D"), ("C","E"), ("D","E")]
```

Subgraph induction

```
> relevantProtein p = p ‘notElem‘ ["A","D","CD"]
> induce relevantProtein g3
[("B","C"), ("C","E")]
```

VHDL outout
```
> writeVHDL g3 "circuit.vhdl"
```

## More info

This repo is meant to be an accompanying material for 'Embedding Graphs in Silicon' FDL'17 paper.

VHDL code generation is a port of a corresponding [pangraph](https://github.com/tuura/pangraph/tree/master/src/Pangraph/VHDL) module.

