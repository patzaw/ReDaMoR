# Read a named sparse matrix in MatrixMarket text format

Read a named sparse matrix in MatrixMarket text format

## Usage

``` r
read_named_MM(
  file,
  skip = 0,
  n_max = Inf,
  class = c("dgCMatrix", "tibble"),
  guess_max = 20
)
```

## Arguments

- file:

  the file to read

- skip:

  the number of records to skip (default: 0)

- n_max:

  the maximum number of records to read (default: Inf)

- class:

  the class of object to return. By default a "dgCMatrix". If "tibble"
  is chosen, the sparse matrix is returned as a tibble with 3 columns: i
  (row index), j (column index) and x (values) and an "header" attribute
  containing the matrix rownames and colnames.

- guess_max:

  the number of lines to read to find the header. (see
  [`read_named_MM_header()`](https://patzaw.github.io/ReDaMoR/reference/read_named_MM_header.md))

## Value

By default a dgCMatrix. If the "tibble" class is chosen, the sparse
matrix is returned as a tibble with 3 columns: i (row index), j (column
index) and x (values) and an "header" attribute containing the matrix
rownames and colnames.
