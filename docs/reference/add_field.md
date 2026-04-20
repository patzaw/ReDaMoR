# Add a field to a table in a [RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)

Add a field to a table in a
[RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)

## Usage

``` r
add_field(x, tableName, name, type, nullable, unique, comment)
```

## Arguments

- x:

  a
  [RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)

- tableName:

  the name of the table to modify (a single character)

- name:

  the name of the field to add (a single character)

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
