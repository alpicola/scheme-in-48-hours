# Scheme in 48 Hours

An implemention of a small subset of R6RS Scheme.

goal:

Major features of the R6RS's base library

- types
  - `boolean`
  - `pair` (immutable)
  - `symbol`
  - `number` (only integer)
  - `procedure`
  - `null`
- definitions
  - `define`
  - `define-syntax`
- expressions
  - `quote`
  - `lambda`
  - `if`
  - `let`
  - `let*`
  - `letrec`
  - `letrec*`
  - `begin`
- first-class continuations
  - `call-with-current-continuation`
- hygienic macros
  - `syntax-rules`
  - `identifier-syntax`
