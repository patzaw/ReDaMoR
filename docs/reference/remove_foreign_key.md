# Remove a foreign key between two tables

Remove a foreign key between two tables

## Usage

``` r
remove_foreign_key(x, fromTable, fromFields, toTable, toFields)
```

## Arguments

- x:

  a
  [RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)

- fromTable:

  the name of the referencing table

- fromFields:

  the name of the referencing fields

- toTable:

  the name of the referenced table

- toFields:

  the names of the referenced fields

## Value

A
[RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)
