# ShOTT

An Observational [ShiTT](https://github.com/KonjacSource/ShiTT)

## New primitives

```haskell
eq    : (S T : U) (x : S) (y : T) -> U
coe   : (S T : U) (Q : eq U U S T) -> S -> T 
coeP  : (S T : U) (Q : eq U U S T) (s : S) -> eq S T s (coe S T Q s)
```

## TODO 

Syntax sugar for OTT.

## Note

Keep tracing every equality terms is too complex, so I give up on that.