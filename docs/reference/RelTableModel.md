# Create a RelTableModel object

Create a RelTableModel object

## Usage

``` r
RelTableModel(
  l = NULL,
  tableName,
  fields,
  primaryKey = NULL,
  foreignKeys = NULL,
  indexes = NULL,
  display = list(x = as.numeric(NA), y = as.numeric(NA), color = as.character(NA),
    comment = as.character(NA))
)
```

## Arguments

- l:

  DEPRECATED. A named list with the function parameters. If `NULL`
  (default) the function parameters are used. If not `NULL`, the
  function parameters are ignored and taken from l.

- tableName:

  a character vector of length one

- fields:

  a tibble with the following columns:

  - *name*: character

  - *type*: character

  - *nullable*: logical (optional, defaults to TRUE)

  - *unique*: logical (optional, defaults = FALSE)

  - *comment*: character (optional, defaults to NA_character\_)

- primaryKey:

  a character vector of any length. All values should be in fields\$name

- foreignKeys:

  a list of foreign keys. Each foreign key is defined as a list with the
  following elements:

  - *refTable*: a character vector of length one (the referenced table)

  - *key*: a tibble with a "from" and a "to" columns

  - (*cardinality*): an optional integer vector with 4 values:

    - fmin: from minimum cardinality

    - fmax: from maximum cardinality

    - tmin: to minimum cardinality

    - tmax: to maximum cardinality

- indexes:

  a list of indexes. Each index is defined by 3 columns:

  - *field*: character (all in fields\$name)

  - *order*: character

  - *unique*: logical

- display:

  a list gathering:

  - *x*: single numeric value for the x position of the table

  - *y*: single numeric value for the y position of the table

  - *color*: single character value corresponding to the color of the
    table

  - *comment*: single character value with some description of the table

## Value

A RelTableModel object.

## Details

When defining a matrix, 3 and only 3 fields must be defined: 2 of types
'row' and 'column' and the 3rd of your choice. In this case primaryKey
is defined automatically as the combination of row and column.
