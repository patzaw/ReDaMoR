# Confront a [RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md) to actual data

Confront a
[RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)
to actual data

## Usage

``` r
confront_data(
  x,
  data = list(),
  paths = NULL,
  returnData = FALSE,
  verbose = TRUE,
  n_max = Inf,
  checks = if (n_max == Inf) {
     c("unique", "not nullable", "foreign keys")
 } else
    {
     as.character()
 },
  delim = "\t",
  ...
)
```

## Arguments

- x:

  a
  [RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)

- data:

  a list of data frames to be confronted with the model.

- paths:

  a character vector with file paths taken into account if the data is
  empty. The file [basename](https://rdrr.io/r/base/basename.html)
  without extension will be considered as the table name.

- returnData:

  a logical indicating if the data should be returned with the report
  (default: FALSE).

- verbose:

  a single logical value indicating if some process information should
  be displayed (default: TRUE)

- n_max:

  maximum number of records to read (default: Inf).

- checks:

  a character vector with the name of optional checks to be done
  (Default: if n_max==Inf ==\> all of them c("unique", "not nullable",
  "foreign keys"), else ==\> none)

- delim:

  single character used to separate fields within a record (default:
  "\t")

- ...:

  supplementary parameters for the
  [readr::read_delim](https://readr.tidyverse.org/reference/read_delim.html)
  function.

## Value

A report as a list

## Examples

``` r
## Read the model ----
hpo_model <- read_json_data_model(
  system.file("examples/HPO-model.json", package = "ReDaMoR")
)
## Confront to data ----
confrontation_report <- confront_data(
  hpo_model,
  path = list.files(
    system.file("examples/HPO-subset", package = "ReDaMoR"),
    full.names = TRUE
  ),
  returnData = TRUE
)
#> Processing "HPO_hp" (table 1 / 9) 
#> Processing "HPO_altId" (table 2 / 9) 
#> Processing "HPO_sourceFiles" (table 3 / 9) 
#> Processing "HPO_diseases" (table 4 / 9) 
#> Processing "HPO_diseaseHP" (table 5 / 9) 
#> Processing "HPO_diseaseSynonyms" (table 6 / 9) 
#> Processing "HPO_parents" (table 7 / 9) 
#> Processing "HPO_descendants" (table 8 / 9) 
#> Processing "HPO_synonyms" (table 9 / 9) 
#> Model
#> SUCCESS
#> 
#> Check configuration
#>    - Optional checks: unique, not nullable, foreign keys
#>    - Maximum number of records: Inf
#> 
#> HPO_hp
#> SUCCESS
#> Field issues or warnings
#>    - description: SUCCESS Missing values 117/500 = 23%
#> 
```
