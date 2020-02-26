## ----setup, include = FALSE---------------------------------------------------
library(knitr)
library(ReDaMoR)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE--------------------------------------------------------------
#  devtools::install_github("patzaw/ReDaMoR")

## ---- eval=FALSE--------------------------------------------------------------
#  library(ReDaMoR)
#  m <- model_relational_data()

## ---- eval=FALSE--------------------------------------------------------------
#  m <- model_relational_data(recover_RelDataModel())

## -----------------------------------------------------------------------------
hpoModel <- read_json_data_model(
  system.file("examples/HPO-model.json", package="ReDaMoR")
)
plot(hpoModel)
## Edit the model
# m <- model_relational_data(hpoModel)

## ---- eval=FALSE--------------------------------------------------------------
#  library(ReDaMoR)
#  model_relational_data()

