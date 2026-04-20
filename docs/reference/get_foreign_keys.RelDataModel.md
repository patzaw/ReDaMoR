# Get foreign keys in [RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)

Get foreign keys in
[RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)

## Usage

``` r
# S3 method for class 'RelDataModel'
get_foreign_keys(x)
```

## Arguments

- x:

  a
  [RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)

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
