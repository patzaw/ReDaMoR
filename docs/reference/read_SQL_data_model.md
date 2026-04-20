# Read a data model from an SQL file from the MySQL Workbench

Read a data model from an SQL file from the MySQL Workbench

## Usage

``` r
read_SQL_data_model(f, typeRef = "MySQLWB", mysqlcomments = TRUE)

readSQLDataModel(...)
```

## Arguments

- f:

  the SQL file to read

- typeRef:

  the reference for type conversion (Default: "MySQLWB"; see
  [`list_type_ref()`](https://patzaw.github.io/ReDaMoR/reference/list_type_ref.md))

- mysqlcomments:

  if MySQL comments (starting with \#) should be removed (Default: TRUE)

- ...:

  params for `read_SQL_data_model`

## Value

A
[RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)
object

## Details

Database, table and field names should be surrounded by "\`".

## Functions

- `readSQLDataModel()`: Deprecated version of read_SQL_data_model

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
