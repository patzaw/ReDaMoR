## ----setup, include = FALSE---------------------------------------------------
library(knitr)
library(ReDaMoR)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
cranRef <- function(x){
  sprintf(
    "[%s](https://CRAN.R-project.org/package=%s): %s",
    x, x, packageDescription(x)$Title
  )
}

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("ReDaMoR")

## ----eval=FALSE---------------------------------------------------------------
#  devtools::install_github("patzaw/ReDaMoR")

## ----eval=FALSE---------------------------------------------------------------
#  library(ReDaMoR)
#  m <- model_relational_data()

## ----eval=FALSE---------------------------------------------------------------
#  m <- model_relational_data(recover_RelDataModel())

## -----------------------------------------------------------------------------
hpo_model <- read_json_data_model(
  system.file("examples/HPO-model.json", package="ReDaMoR")
)
plot(hpo_model)

## -----------------------------------------------------------------------------
## Edit the model
# m <- model_relational_data(hpo_model)

## ----eval=FALSE---------------------------------------------------------------
#  library(ReDaMoR)
#  model_relational_data()

## -----------------------------------------------------------------------------
confrontation_report <- confront_data(
  hpo_model,
  path=list.files(
    system.file("examples/HPO-subset", package="ReDaMoR"),
    full.names=TRUE
  ),
  returnData=TRUE
)

## ----results='asis'-----------------------------------------------------------
# view_confrontation_report(confrontation_report) # Use RStudio viewer
format_confrontation_report_md(
  confrontation_report,
  title="Example: Confrontation with original data",
  level=1, numbered=FALSE
) %>%
  cat()

## -----------------------------------------------------------------------------
hpo_tables <- confrontation_report$data

## ----results='asis'-----------------------------------------------------------
hpo_tables$HPO_diseases <- hpo_tables$HPO_diseases %>% slice(1:100)
hpo_tables$HPO_synonyms[1:10, "synonym"] <- NA
hpo_tables$HPO_hp <- hpo_tables$HPO_hp %>% mutate(level=as.character(level))
confront_data(hpo_model, hpo_tables, verbose=FALSE) %>%
  format_confrontation_report_md(
    title="Example: Confrontation with altered data",
    level=1, numbered=FALSE
  ) %>%
    cat()

## -----------------------------------------------------------------------------
hpo_tables <- confrontation_report$data
new_model <- df_to_model(
  list=names(hpo_tables), envir=as.environment(hpo_tables)
)
new_model %>%
  auto_layout(lengthMultiplier=250) %>%
  plot()

## -----------------------------------------------------------------------------
# model_relational_data(new_model)

## -----------------------------------------------------------------------------
ge_model <- read_json_data_model(
  system.file("examples/GE-model.json", package="ReDaMoR")
)
plot(ge_model)

