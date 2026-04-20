# Remove a table from a [RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)

Remove a table from a
[RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)

## Usage

``` r
remove_table(x, tableName, rmForeignKeys = FALSE)
```

## Arguments

- x:

  a
  [RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)

- tableName:

  the name of the table to remove

- rmForeignKeys:

  if TRUE, remove foreign keys which are not available after extraction.
  If FALSE (default) the function will throw an error if any foreign
  keys does not exist in the extracted RelDataModel.

## Value

A
[RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)
