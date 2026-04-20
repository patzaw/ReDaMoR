# Read a data model from JSON

Read a data model from JSON

## Usage

``` r
read_json_data_model(txt)
```

## Arguments

- txt:

  a JSON string, URL or file

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
