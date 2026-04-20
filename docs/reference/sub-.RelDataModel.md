# Subset a [RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)

Subset a
[RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)

## Usage

``` r
# S3 method for class 'RelDataModel'
x[i, rmForeignKeys = FALSE, ...]
```

## Arguments

- x:

  the
  [RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)
  objcet

- i:

  the index or the names of the elements to extract

- rmForeignKeys:

  if TRUE, remove foreign keys which are not available after extraction.
  If FALSE (default) the function will throw an error if any foreign
  keys does not exist in the extracted RelDataModel.

- ...:

  additional arguments for the
  [`RelDataModel`](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)
  function.
