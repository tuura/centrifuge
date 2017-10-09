```
x <- readRawNetwork "examples/graphs/small.graphml"
```

```
render x
```

```
let (y :: Network ByteString) = fmap (const "n10") x
> parse error
```


```
let (y :: RawNetwork) = fmap (const "n10") x
> ()
```

```
p <- readRawNetwork "examples/graphs/big.graphml"
```


```
let f = \(x :: ByteString) -> any (== 7)
```

```
let q = mergeVertices ["000001", "000002"] "n1" p
```

```
let q = mergeVertices (Prelude.map (pack . Prelude.map c2w . ("0000" ++) . show) [10..99]) "n1" p
```

```
let r = splitVertex "n1" (Prelude.map (pack . Prelude.map c2w . ("r" ++) . show) [10..12]) q
```