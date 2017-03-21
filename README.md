# lambda-CL

### `λx.λy.(y x) → (C I)`
### `(C I) → λx.λy.(y x)`

Convert between lambda expressions and combinatory logic in Schönfinkel's
BCIKS system.

## BCIKS quick guide

- `B f g x = f (g x)` (composition)
- `C f x y = f y x` (order swap)
- `I x = x` (identity)
- `K x y = x` (drop arg)
- `S f g x = (f x) (g x)` (duplicate arg)

## Usage

Simply load [`./cl.scm`](./cl.scm) using your preferred scheme interpreter
and use the `T` and `L` functions. The combinators `B`, `C`, `I`, `K`, `S`
are loaded as well.

```sh
$ mit-scheme -load cl.scm
```

### T: λ -> CL

To convert from expressions of lambda calculus to combinatory logic, use the
`T` function:

```scheme
> (T '(lambda (x) (lambda (y) (y x))))
;Value: (c i)

> (T '(lambda (x) (lambda (y) (+ (sin x) (sin y)))))
;Value: ((c ((b b) ((b +) sin))) sin)

> (eval (T '(((lambda (x)
                (lambda (y)
                  (y x)))
              3) sin))
        user-initial-environment)
;Value: .1411200080598672
```

### L: CL -> λ

To convert from combinatory logic to expressions of lambda calculus, use the
`L` function:

```scheme
> (L '(C I))
;Value: (lambda (a) (lambda (b) (b a)))

> (L '(((C I) 3) sin))
;Value: (sin 3)

> (((eval (L '(C I))
          user-initial-environment)
    3) sin)
;Value: .1411200080598672
```
