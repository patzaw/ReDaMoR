# Create a RelDataModel object

Create a RelDataModel object

## Usage

``` r
RelDataModel(l, checkFK = TRUE, createFKIndex = FALSE)
```

## Arguments

- l:

  the list of table models
  ([RelTableModel](https://patzaw.github.io/ReDaMoR/reference/RelTableModel.md)
  objects)

- checkFK:

  a logical indicating if foreign keys should be checked (default: TRUE)

- createFKIndex:

  should an index be create for the foreign keys (default: FALSE)

## Value

A RelDataModel object.
