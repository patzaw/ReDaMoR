# Guess [RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md) constraints based on the provided or existing tables

Guess
[RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)
constraints based on the provided or existing tables

## Usage

``` r
guess_constraints(
  x,
  data = NULL,
  env = parent.frame(n = 1),
  constraints = c("unique", "not nullable", "foreign keys")
)
```

## Arguments

- x:

  a
  [RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)

- data:

  a named list of tables. All names of x should exist in data. If NULL,
  the data are taken from env.

- env:

  the R environment in which to find the tables

- constraints:

  the type of constraints to guess

## Value

A
[RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)

## Details

The guessed constraints should be carefully review, especially the
foreign keys.

Complex foreign keys involving multiple fields are not guessed.

## Examples

``` r
## Read data files ----
to_read <- list.files(
  system.file("examples/HPO-subset", package = "ReDaMoR"),
  full.names = TRUE
)
hpo_tables <- list()
for (f in to_read) {
  hpo_tables[[sub("[.]txt$", "", basename(f))]] <- readr::read_tsv(f)
}
#> Rows: 89 Columns: 2
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (2): id, alt
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 972 Columns: 2
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (2): id, descendant
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 2594 Columns: 3
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (2): db, hp
#> dbl (1): id
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 2337 Columns: 4
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (2): db, synonym
#> dbl (1): id
#> lgl (1): preferred
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 1903 Columns: 3
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (2): db, label
#> dbl (1): id
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 500 Columns: 4
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (3): id, name, description
#> dbl (1): level
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 95 Columns: 2
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (2): id, parent
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 2 Columns: 2
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr  (1): url
#> date (1): current
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Rows: 730 Columns: 3
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr (3): id, synonym, type
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
## Build the model from a list of data frames ----
new_model <- df_to_model(
  list = names(hpo_tables),
  envir = as.environment(hpo_tables)
)
## Guess constraints and auto layout ----
new_model <- guess_constraints(new_model, data = hpo_tables) %>%
  auto_layout(lengthMultiplier = 250)

## Plot the model ----
new_model %>%
  plot()

{"x":{"nodes":{"tableName":["HPO_altId","HPO_descendants","HPO_diseaseHP","HPO_diseaseSynonyms","HPO_diseases","HPO_hp","HPO_parents","HPO_sourceFiles","HPO_synonyms"],"label":["<b>HPO_altId<\/b>\n    - id {character}\n    - *alt {character}","<b>HPO_descendants<\/b>\n    - id {character}\n    - descendant {character}","<b>HPO_diseaseHP<\/b>\n    - db {character}\n    - id {numeric}\n    - hp {character}","<b>HPO_diseaseSynonyms<\/b>\n    - db {character}\n    - id {numeric}\n    - synonym {character}\n    - preferred {logical}","<b>HPO_diseases<\/b>\n    - db {character}\n    - id {numeric}\n    - label {character}","<b>HPO_hp<\/b>\n    - *id {character}\n    - *name {character}\n    - (description {character})\n    - level {numeric}","<b>HPO_parents<\/b>\n    - *id {character}\n    - parent {character}","<b>HPO_sourceFiles<\/b>\n    - *url {character}\n    - *current {Date}","<b>HPO_synonyms<\/b>\n    - id {character}\n    - *synonym {character}\n    - type {character}"],"title":["<div style=\"max-width:400px; max-height:300px;overflow:scroll;\"><p><strong style=\"text-decoration:underline;\">HPO_altId<\/strong><\/p> <ul> <li><strong>id<\/strong><\/li> <li><strong>alt<\/strong><\/li> <\/ul><\/div>","<div style=\"max-width:400px; max-height:300px;overflow:scroll;\"><p><strong style=\"text-decoration:underline;\">HPO_descendants<\/strong><\/p> <ul> <li><strong>id<\/strong><\/li> <li><strong>descendant<\/strong><\/li> <\/ul><\/div>","<div style=\"max-width:400px; max-height:300px;overflow:scroll;\"><p><strong style=\"text-decoration:underline;\">HPO_diseaseHP<\/strong><\/p> <ul> <li><strong>db<\/strong><\/li> <li><strong>id<\/strong><\/li> <li><strong>hp<\/strong><\/li> <\/ul><\/div>","<div style=\"max-width:400px; max-height:300px;overflow:scroll;\"><p><strong style=\"text-decoration:underline;\">HPO_diseaseSynonyms<\/strong><\/p> <ul> <li><strong>db<\/strong><\/li> <li><strong>id<\/strong><\/li> <li><strong>synonym<\/strong><\/li> <li><strong>preferred<\/strong><\/li> <\/ul><\/div>","<div style=\"max-width:400px; max-height:300px;overflow:scroll;\"><p><strong style=\"text-decoration:underline;\">HPO_diseases<\/strong><\/p> <ul> <li><strong>db<\/strong><\/li> <li><strong>id<\/strong><\/li> <li><strong>label<\/strong><\/li> <\/ul><\/div>","<div style=\"max-width:400px; max-height:300px;overflow:scroll;\"><p><strong style=\"text-decoration:underline;\">HPO_hp<\/strong><\/p> <ul> <li><strong>id<\/strong><\/li> <li><strong>name<\/strong><\/li> <li><strong>description<\/strong><\/li> <li><strong>level<\/strong><\/li> <\/ul><\/div>","<div style=\"max-width:400px; max-height:300px;overflow:scroll;\"><p><strong style=\"text-decoration:underline;\">HPO_parents<\/strong><\/p> <ul> <li><strong>id<\/strong><\/li> <li><strong>parent<\/strong><\/li> <\/ul><\/div>","<div style=\"max-width:400px; max-height:300px;overflow:scroll;\"><p><strong style=\"text-decoration:underline;\">HPO_sourceFiles<\/strong><\/p> <ul> <li><strong>url<\/strong><\/li> <li><strong>current<\/strong><\/li> <\/ul><\/div>","<div style=\"max-width:400px; max-height:300px;overflow:scroll;\"><p><strong style=\"text-decoration:underline;\">HPO_synonyms<\/strong><\/p> <ul> <li><strong>id<\/strong><\/li> <li><strong>synonym<\/strong><\/li> <li><strong>type<\/strong><\/li> <\/ul><\/div>"],"shape":["box","box","box","box","box","box","box","box","box"],"font.multi":[true,true,true,true,true,true,true,true,true],"font.align":["left","left","left","left","left","left","left","left","left"],"x":[0.3543009217930049,132.7854120164409,105.1578303324615,-102.6960594510221,-250,-1.624495516232061,-3.613306174082037,250,-136.1612796873943],"y":[16.17976416045197,-129.7160335831638,248.2641607529528,250,81.25714786596389,-119.6821421016099,-250,76.48708483197424,-125.7818406369439],"color.background":["lightgrey","lightgrey","lightgrey","lightgrey","lightgrey","lightgrey","lightgrey","lightgrey","lightgrey"],"color.border":["black","black","black","black","black","black","black","black","black"],"color.highlight.border":["orange","orange","orange","orange","orange","orange","orange","orange","orange"],"color.highlight.background":["lightgrey","lightgrey","lightgrey","lightgrey","lightgrey","lightgrey","lightgrey","lightgrey","lightgrey"],"id":["HPO_altId","HPO_descendants","HPO_diseaseHP","HPO_diseaseSynonyms","HPO_diseases","HPO_hp","HPO_parents","HPO_sourceFiles","HPO_synonyms"]},"edges":{"id":["HPO_altId->HPO_hp: id->id","HPO_descendants->HPO_hp: id->id","HPO_parents->HPO_hp: id->id","HPO_synonyms->HPO_hp: id->id"],"to":["HPO_hp","HPO_hp","HPO_hp","HPO_hp"],"title":["<table style=\"border: 1px solid black; padding:1px;\"><tr style=\"border: 1px solid black; padding:1px;\"><th style=\"border: 1px solid black; padding:1px;text-align:center;\">HPO_altId<br>(0..n)<\/th><th style=\"border: 1px solid black; padding:1px;text-align:center;\">HPO_hp<br>(1..1)<\/th><\/tr><tr style=\"border: 1px solid black; padding:1px;\"><td style=\"border: 1px solid black; padding:1px;\">id<\/td><td style=\"border: 1px solid black; padding:1px;\">id<\/td><\/tr><\/table>","<table style=\"border: 1px solid black; padding:1px;\"><tr style=\"border: 1px solid black; padding:1px;\"><th style=\"border: 1px solid black; padding:1px;text-align:center;\">HPO_descendants<br>(0..n)<\/th><th style=\"border: 1px solid black; padding:1px;text-align:center;\">HPO_hp<br>(1..1)<\/th><\/tr><tr style=\"border: 1px solid black; padding:1px;\"><td style=\"border: 1px solid black; padding:1px;\">id<\/td><td style=\"border: 1px solid black; padding:1px;\">id<\/td><\/tr><\/table>","<table style=\"border: 1px solid black; padding:1px;\"><tr style=\"border: 1px solid black; padding:1px;\"><th style=\"border: 1px solid black; padding:1px;text-align:center;\">HPO_parents<br>(0..n)<\/th><th style=\"border: 1px solid black; padding:1px;text-align:center;\">HPO_hp<br>(1..1)<\/th><\/tr><tr style=\"border: 1px solid black; padding:1px;\"><td style=\"border: 1px solid black; padding:1px;\">id<\/td><td style=\"border: 1px solid black; padding:1px;\">id<\/td><\/tr><\/table>","<table style=\"border: 1px solid black; padding:1px;\"><tr style=\"border: 1px solid black; padding:1px;\"><th style=\"border: 1px solid black; padding:1px;text-align:center;\">HPO_synonyms<br>(0..n)<\/th><th style=\"border: 1px solid black; padding:1px;text-align:center;\">HPO_hp<br>(1..1)<\/th><\/tr><tr style=\"border: 1px solid black; padding:1px;\"><td style=\"border: 1px solid black; padding:1px;\">id<\/td><td style=\"border: 1px solid black; padding:1px;\">id<\/td><\/tr><\/table>"],"ff":["id","id","id","id"],"tf":["id","id","id","id"],"from":["HPO_altId","HPO_descendants","HPO_parents","HPO_synonyms"],"arrows":["to","to","to","to"],"font.align":["bottom","bottom","bottom","bottom"],"smooth.type":["curvedCCW","curvedCCW","curvedCCW","curvedCCW"],"smooth.roundness":[-0,-0,0,0],"selfReferenceSize":[30,30,30,30],"uef":["HPO_altId","HPO_descendants","HPO_hp","HPO_hp"],"uet":["HPO_hp","HPO_hp","HPO_parents","HPO_synonyms"],"ue":["HPO_altId<->HPO_hp","HPO_descendants<->HPO_hp","HPO_hp<->HPO_parents","HPO_hp<->HPO_synonyms"],"color.color":["black","black","black","black"],"color.highlight":["orange","orange","orange","orange"]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot","borderWidth":2,"labelHighlightBold":false},"manipulation":{"enabled":false},"edges":{"width":2,"selectionWidth":2},"interaction":{"multiselect":true,"zoomSpeed":1},"layout":{"randomSeed":2},"physics":{"enabled":false}},"groups":null,"width":null,"height":null,"idselection":{"enabled":false},"byselection":{"enabled":false},"main":null,"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)","tooltipStay":300,"tooltipStyle":"position: fixed;visibility:hidden;padding: 5px;white-space: nowrap;font-family: verdana;font-size:14px;font-color:#000000;background-color: #f5f4ed;-moz-border-radius: 3px;-webkit-border-radius: 3px;border-radius: 3px;border: 1px solid #808074;box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);"},"evals":[],"jsHooks":[]}
```
