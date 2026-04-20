# Convert a set of types from or to R supported types

Convert a set of types from or to R supported types

## Usage

``` r
conv_type_ref(x, from = NULL, to = NULL, ignore.case = TRUE)
```

## Arguments

- x:

  a character vector of types to be converted. If from is not null, x
  should be a set of valid types in the from reference. If to is not
  null, x should be a set of supported R types (SUPPTYPES).

- from:

  a character vector of length one: the type reference
  ([list_type_ref](https://patzaw.github.io/ReDaMoR/reference/list_type_ref.md))
  of x

- to:

  a character vector of length one: the targeted type reference
  ([list_type_ref](https://patzaw.github.io/ReDaMoR/reference/list_type_ref.md))

- ignore.case:

  should case be ignored when converting \`from“ type reference
  (default: TRUE)

## Details

Only `from` XOR `to` should be set
