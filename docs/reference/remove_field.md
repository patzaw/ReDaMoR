# Remove a field from a table in a [RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)

Remove a field from a table in a
[RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)

## Usage

``` r
remove_field(x, tableName, fieldName, rmForeignKeys = FALSE)
```

## Arguments

- x:

  a
  [RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)

- tableName:

  the name of the table to modify (a single character)

- fieldName:

  the name of the field to remove (a single character)

- rmForeignKeys:

  a single logical indicating if the corresponding foreign keys should
  be removed. If FALSE (default), the function will throw an error if it
  encounter a foreign key using the field.

## Value

A
[RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)
