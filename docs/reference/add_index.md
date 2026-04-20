# Add an index to a table in a [RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)

Add an index to a table in a
[RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)

## Usage

``` r
add_index(x, tableName, fieldNames, unique)
```

## Arguments

- x:

  a
  [RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)

- tableName:

  the name of the table to modify (a single character)

- fieldNames:

  the names of the fields to include in the index

- unique:

  a logical indicating if the indexed values are unique

## Value

A
[RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)
