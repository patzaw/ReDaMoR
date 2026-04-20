# List indexes of a [RelTableModel](https://patzaw.github.io/ReDaMoR/reference/RelTableModel.md) object

List indexes of a
[RelTableModel](https://patzaw.github.io/ReDaMoR/reference/RelTableModel.md)
object

## Usage

``` r
index_table(x)
```

## Arguments

- x:

  a
  [RelTableModel](https://patzaw.github.io/ReDaMoR/reference/RelTableModel.md)
  object

## Value

A tibble with the following columns:

- **index**: an integer corresponding to the index number

- **field**: a character corresponding to field belonging to the index

- **unique**: a logical indicating the uniqueness of the field
