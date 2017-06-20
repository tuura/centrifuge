# Graph Cruncher

An experimental graph parsing, manipulation and transformation library based on
[Algebraic graphs](http://hackage.haskell.org/package/algebraic-graphs/docs/Algebra-Graph.html)

## The "Protein" domain-specific language

The "Protein" DSL is a modelling tool for protein networks.

### Features

* Import protein networks from GraphML files with `readGraphML` function.
* Split, merge or remove vertices.
* Induce subnetworks.
* Generate VHDL circuit descriptions for simulation.

### Try "Protein" interactively  in GHCi

Run GHCi with stack:

```
> stack ghci
```

Enable the `OverloadedStrings` extension to use `ByteString` literals

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
