# Lengths of object elements

Lengths of object elements

## Usage

``` r
lengths(x, use.names = TRUE)
```

## Arguments

- x:

  an object. If there is no method implemented for this object, the
  [`base::lengths()`](https://rdrr.io/r/base/lengths.html) function is
  used.

- use.names:

  logical indicating if the result should inherit the names from x.

## Value

A non-negative integer of length length(x), except when any element has
a length of more than 2^31 - 1 elements, when it returns a double
vector. When use.names is true, the names are taken from the names on x,
if any.

## See also

[`base::lengths()`](https://rdrr.io/r/base/lengths.html)
