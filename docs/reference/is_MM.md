# Identify if a file is in MatrixMarket text format

Identify if a file is in MatrixMarket text format

## Usage

``` r
is_MM(file)
```

## Arguments

- file:

  the file to read

## Value

A logical. If FALSE, the first line of the file is returned as an
attribute named "r1": `attr(is_MM, "r1")`
