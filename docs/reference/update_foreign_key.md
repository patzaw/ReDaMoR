# Update a the cardinalities of a foreign key between two tables

Update a the cardinalities of a foreign key between two tables

## Usage

``` r
update_foreign_key(
  x,
  fromTable,
  fromFields,
  toTable,
  toFields,
  fmin,
  fmax,
  tmin,
  tmax
)
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

- fmin:

  from minimum cardinality

- fmax:

  from maximum cardinality

- tmin:

  to minimum cardinality

- tmax:

  to maximum cardinality

## Value

A
[RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)
