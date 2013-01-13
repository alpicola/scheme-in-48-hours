# Scheme in 48 Hours

An implemention of a small subset of R6RS Scheme.

## Goal

The following features of the R6RS's base library

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
  - `set!`
  - `let`
  - `let*`
  - `letrec`
  - `letrec*`
  - `begin`
- first-class continuations
  - `call-with-current-continuation`
- hygienic macros
  - `syntax-rules`
