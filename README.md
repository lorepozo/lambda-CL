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

On Racket, require [`./cl.rkt`](./cl.rkt), and on other scheme interpreters,
load [`./cl.scm`](./cl.scm). These provide the `T`, `L`, and `P` functions
(described below), as well as the combinators `B`, `C`, `I`, `K`, `S`.

```sh
$ racket -i --require cl.rkt
$ mit-scheme -load cl.scm
```

### Currying

Arguments must be given in curried form. The `T` function can parse lambda
expressions and curried function applications (i.e. pairs of the form
**`(a b)`**). The `L` function can only parse curried function applications.
Any free variables will be left intact unless a variable name conflicts with
a combinator (watch out for `i`!).

### T: λ -> CL

To convert from expressions of lambda calculus to combinatory logic, use the
`T` function:

```racket
> (T '(lambda (x) (lambda (y) (y x))))
; (c i)

> (T '(lambda (x) (lambda (y) ((+ (sin x)) (sin y)))))
; ((c ((b b) ((b +) sin))) sin)
```

### L: CL -> λ

To convert from combinatory logic to expressions of lambda calculus, use the
`L` function. The resulting lambda expression is in beta normal form.

```racket
> (L '(C I))
; (lambda (a) (lambda (b) (b a)))

> (L '(((C I) 3) sin))
; (sin 3)

> #| racket     |# (define f (eval (L '(C I)) (current-namespace)))
> #| mit-scheme |# (define f (eval (L '(C I)) user-initial-environment))
; f
> ((f 3) sin)
; .1411200080598672

> (T (L '(C I)))
; (c i)
```

### P: λ -> uncurried and pretty

To uncurry and "prettify" a lambda expression, use the `P` function. This
will minimize use of parentheses, spaces, and lambda literals (`λaλb.b a` ->
`λab.b a`). It returns a string, not a quoted expression like the other
functions, so it can use the unicode `λ` symbol rather than the verbose word
`lambda`.

```racket
> (P '(lambda (x) (lambda (y) (lambda (z) ((((f (g x)) z) (lambda (w) ((+ y) w))) b)))))
; "λxyz.f (g x) z (λw.+ y w) b"

> (P (L '(C I)))
; "λab.b a"
```
