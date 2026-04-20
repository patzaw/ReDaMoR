# Update field information in a table of a [RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)

Update field information in a table of a
[RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)

## Usage

``` r
update_field(
  x,
  tableName,
  fieldName,
  type = NULL,
  nullable = NULL,
  unique = NULL,
  comment = NULL
)
```

## Arguments

- x:

  a
  [RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)

- tableName:

  the name of the table to modify (a single character)

- fieldName:

  the name of the field to modify (a single character)

- type:

  the type of the field (a single character)

- nullable:

  if the field is nullable (a single logical)

- unique:

  if the values are unique (a single logical)

- comment:

  a description (a single character)

## Value

A
[RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)
