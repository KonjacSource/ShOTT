# ShOTT

An Observational [ShiTT](https://github.com/KonjacSource/ShiTT)

Forked from ShiTT since [Nov 10, 2024](https://github.com/KonjacSource/ShiTT/commit/df9896b24fba7690cb4a795e60f5139933d839af)

## New primitives

```haskell
eq    : (S T : U) (x : S) (y : T) -> U
coe   : (S T : U) (Q : eq U U S T) -> S -> T 
coeP  : (S T : U) (Q : eq U U S T) (s : S) -> eq S T s (coe S T Q s)
-- More in OTT.shitt
```

Now we can prove something we can not in MLTT.

For example.

```haskell
fun appendAssoc {A : U} {l m n : N} 
                (u : Vec A l) (v : Vec A m) (w : Vec A n) 
              : Id (append (append u v) w) (append u (append v w))
-- More in OTTExample.shitt
```

This is ill-typed in MLTT. But it is fine now thanks to heterogeneous equality.

## TODO

Syntax sugar for OTT.

## Note

Keep tracing every equality terms is too complex, so I give up on that.

## Reference

Pattern matching and inductive type:

- Ulf Norell. [Towards a practical programming language based on dependent type theory](https://www.cse.chalmers.se/~ulfn/papers/thesis.pdf).

Solving meta variables:

- András Kovács. [elaboration-zoo](https://github.com/AndrasKovacs/elaboration-zoo).

Termination checking:

- Karl Mehltretter. [Termination Checking for a Dependently Typed Language](https://www.cse.chalmers.se/~abela/mehltret-da.pdf).

Observational Type Theory:

- Thorsten Altenkirch, Conor McBride. [Towards observational type theory, draft (2006)](http://strictlypositive.org/ott.pdf).

- Thorsten Altenkirch, Conor McBride, Wouter Swierstra. Observational Equality, Now!, PLPV ‘07: Proceedings of the 2007 workshop on Programming languages meets program verification (2007) 57-68 [ISBN:978-1-59593-677-6, [doi:10.1145/1292597.1292608](http://doi.org/10.1145/1292597.1292608), [pdf](https://www.cs.nott.ac.uk/~psztxa/publ/obseqnow.pdf)]

- [Guest0x0](https://github.com/Guest0x0), [trebor](https://github.com/Guest0x0/trebor)
