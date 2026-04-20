# Add a foreign key between two tables

Add a foreign key between two tables

## Usage

``` r
add_foreign_key(
  x,
  fromTable,
  fromFields,
  toTable,
  toFields,
  fmin = 0L,
  fmax = -1L,
  tmin = 1L,
  tmax = 1L
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

  from minimum cardinality (default: 0L)

- fmax:

  from maximum cardinality (default: -1L ==\> Infinite)

- tmin:

  to minimum cardinality (default: 1L)

- tmax:

  to maximum cardinality (default: 1L)

## Value

A
[RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)
