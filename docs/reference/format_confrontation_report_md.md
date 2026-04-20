# Format confrontation report in markdown format

Format confrontation report in markdown format

## Usage

``` r
format_confrontation_report_md(
  cr,
  title = "Model",
  level = 0,
  numbered = TRUE,
  bgSuccess = "green",
  txSuccess = "black",
  bgFailure = "red",
  txFailure = "white",
  bgMessage = "#FFBB33",
  txMessage = "white"
)
```

## Arguments

- cr:

  the confrontation report from
  [confront_data](https://patzaw.github.io/ReDaMoR/reference/confront_data.md)

- title:

  a character with a single value corresponding to the report

- level:

  rmarkdown level in document hierarchy (default:0 ==\> highest). It
  should be an integer between 0 and 4.

- numbered:

  a logical. If TRUE (default) the sections are part of document
  numbering.

- bgSuccess:

  background color for SUCCESS

- txSuccess:

  text color for SUCCESS

- bgFailure:

  background color for FAILURE

- txFailure:

  text color for FAILURE

- bgMessage:

  background color for a warning message

- txMessage:

  text color for a warning message

## Examples

``` r
## Read the model ----
hpo_from_sql <- read_SQL_data_model(
  system.file("examples/HPO-model.sql", package = "ReDaMoR")
)
#> Warning: l param is deprecated and will probably removed in the future
#> Warning: l param is deprecated and will probably removed in the future
#> Warning: l param is deprecated and will probably removed in the future
#> Warning: l param is deprecated and will probably removed in the future
#> Warning: l param is deprecated and will probably removed in the future
#> Warning: l param is deprecated and will probably removed in the future
#> Warning: l param is deprecated and will probably removed in the future
#> Warning: l param is deprecated and will probably removed in the future
#> Warning: l param is deprecated and will probably removed in the future
## Confront to data ----
confrontation_report <- confront_data(
  hpo_from_sql,
  path = list.files(
    system.file("examples/HPO-subset", package = "ReDaMoR"),
    full.names = TRUE
  ),
  verbose = FALSE,
  returnData = TRUE
)
## Show the report in console ----
format_confrontation_report(confrontation_report) %>% cat()
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
## Format the report using markdown ----
format_confrontation_report_md(confrontation_report) %>% cat()
#> 
#> # Model
#> 
#> <span style="background-color:green; color:black; padding:2px;">SUCCESS</span>
#> 
#> ## Check configuration
#> 
#> - **Optional checks**: unique, not nullable, foreign keys
#> - **Maximum number of records**: Inf
#> 
#> 
#> ## HPO_hp
#> 
#> <span style="background-color:green; color:black; padding:2px;">SUCCESS</span>
#> 
#> 
#> ### Field issues or warnings
#> 
#> - description: <span style="background-color:green; color:black; padding:2px;">SUCCESS</span> <span style="background-color:#FFBB33; color:white; padding:2px;">Missing values 117/500 = 23%</span>
#> 
```
