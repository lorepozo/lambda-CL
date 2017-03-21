# lambda-CL

### `λx.λy.(y x) → (C I)`

A transformer from lambda expressions to combinatory logic in Schönfinkel's
BCIKS system.
Expanded from some small work done by Gerald Jay Sussman.

### BCIKS quick guide

- `B f g x = f (g x)` (composition)
- `C f x y = f y x` (order swap)
- `I x = x` (identity)
- `K x y = x` (drop arg)
- `S f g x = (f x) (g x)` (duplicate arg)

### Usage

Simply load [`./cl.scm`](./cl.scm) using your preferred scheme interpreter and
use the `T` and `T-trace` functions. The combinators `B`, `C`, `I`, `K`, `S`
are loaded as well.

```sh
$ mit-scheme -load cl.scm
```

```scheme
> (T '(lambda (x) (lambda (y) (y x))))
;Value: (c i)

> (eval (T '(((lambda (x) (lambda (y) (y x))) 3) sin))
        user-initial-environment)
;Value: .1411200080598672

> (((c i) 3) sin)
;Value: .1411200080598672

> (sin 3)
;Value: .1411200080598672
```

The `T-trace` function prints a trace of the transformations numbered
conventionally (and including η-reduction):

1. `T[x] => x`
2. `T[(E₁ E₂)] => (T[E₁] T[E₂])`
3. `T[λx.E] => (K T[E])` if x is not free in E
4. `T[λx.x] => I`
5. `T[λx.λy.E] => T[λx.T[λy.E]]` if x is free in E
6. `T[λx.(E₁ E₂)] => (S T[λx.E₁] T[λx.E₂])` if x is free in both E₁ and E₂
7. `T[λx.(E₁ E₂)] => (C T[λx.E₁] T[E₂])` if x is free in E₁ but not E₂
8. `T[λx.(E₁ E₂)] => (B T[E₁] T[λx.E₂])` if x is free in E₂ but not E₁


```scheme
> (T-trace '(lambda (x) (lambda (y) (y x)))
((t (lambda (x) (lambda (y) (y x)))) =>)
((t (lambda (x) (t (lambda (y) (y x))))) by 5)

((t (lambda (y) (y x))) =>)
(((c (t (lambda (y) y))) (t x)) by 7)

((t x) =>)
((x) by 1)

((t (lambda (y) y)) =>)
(i by 4)

((t (lambda (x) ((c i) x))) =>)
((t (c i)) by eta-reduction)

((t (c i)) =>)
(((t c) (t i)) by 2)

((t i) =>)
((i) by 1)

((t c) =>)
((c) by 1)

;Value: (c i)
```
