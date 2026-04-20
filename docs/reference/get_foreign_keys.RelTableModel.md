# Get foreign keys from [RelTableModel](https://patzaw.github.io/ReDaMoR/reference/RelTableModel.md)

Get foreign keys from
[RelTableModel](https://patzaw.github.io/ReDaMoR/reference/RelTableModel.md)

## Usage

``` r
# S3 method for class 'RelTableModel'
get_foreign_keys(x)
```

## Arguments

- x:

  a
  [RelTableModel](https://patzaw.github.io/ReDaMoR/reference/RelTableModel.md)

## Value

A tibble with the following fields:

- from: the origin of the key

- ff: the key fields in from

- to: the target of the key

- tf: the key fields in to

- fmin: minimum cardinality of from

- fmax: maximum cardinality of from

- tmin: minimum cardinality of to

- tmax: maximum cardinality of to
