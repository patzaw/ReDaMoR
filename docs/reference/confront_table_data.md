# Confront a [RelTableModel](https://patzaw.github.io/ReDaMoR/reference/RelTableModel.md) to actual data

Confront a
[RelTableModel](https://patzaw.github.io/ReDaMoR/reference/RelTableModel.md)
to actual data

## Usage

``` r
confront_table_data(x, d, checks = c("unique", "not nullable"))
```

## Arguments

- x:

  a
  [RelTableModel](https://patzaw.github.io/ReDaMoR/reference/RelTableModel.md)

- d:

  a data frame or a matrix for matrix model

- checks:

  a character vector with the name of optional checks to be done
  (Default: all of them c("unique", "not nullable"))

## Value

A report as a list
