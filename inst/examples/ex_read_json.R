## Read the model ----
hpo_model <- read_json_data_model(
   system.file("examples/HPO-model.json", package="ReDaMoR")
)
## Confront to data ----
confrontation_report <- confront_data(
   hpo_model,
   path=list.files(
      system.file("examples/HPO-subset", package="ReDaMoR"),
      full.names=TRUE
   ),
   returnData=TRUE
)
