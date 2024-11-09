# ShOTT

An Observational [ShiTT](https://github.com/KonjacSource/ShiTT)

Forked from ShiTT since [Nov 10, 2024](https://github.com/KonjacSource/ShiTT/commit/df9896b24fba7690cb4a795e60f5139933d839af)

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