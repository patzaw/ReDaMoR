# Read the header of a named sparse matrix in MatrixMarket text format

Read the header of a named sparse matrix in MatrixMarket text format

## Usage

``` r
read_named_MM_header(file, guess_max = 20)
```

## Arguments

- file:

  the file to read

- guess_max:

  the number of lines to read to find the header. (4 should be
  sufficient. Default: 20)

## Value

A list with the following fields:

- rownames: a character vector with the matrix row names

- colnames: a character vector with the matrix column names

- rows: the number of matrix rows

- columns: the number of matrix columns

- values: the number of values in the matrix

- header_length: the number of lines in the header
