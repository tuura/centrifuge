```
> g <- parse "example.graphml"
> print g
edges [("A","B"), ("B","C"), ("B","E"), ("C","D"), ("D","E")]
```

```
> :type G.mergeVertices
mergeVertices :: Eq a => (a -> Bool) -> a -> G.Graph a -> G.Graph a
```

```
> g' = mergeVertices (\x -> x == "C" || x == "D") "CD" g
> print g'
edges [("A","B"), ("B","CD"), ("CD","E")]
```

```
> :type splitVertex
splitVertex :: Eq a => a -> [a] -> G.Graph a -> G.Graph a
```

```
> g'' = splitVertex "CD" ["C","D"] merged
> print g''
edges [("A","B"),("B","C"),("B","D"),("C","E"),("D","E")]
```

```
> :type G.induce
induce :: (a -> Bool) -> G.Graph a -> G.Graph a
```

```
> induce isDiseaseRelevant g''
edges [("B","C"),("C","E")]
```
