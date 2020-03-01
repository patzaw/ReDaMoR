## Read the model ----
hpo_from_sql <- read_SQL_data_model(
   system.file("examples/HPO-model.sql", package="ReDaMoR")
)
## Confront to data ----
confrontation_report <- confront_data(
   hpo_from_sql,
   path=list.files(
      system.file("examples/HPO-subset", package="ReDaMoR"),
      full.names=TRUE
   ),
   returnData=TRUE
)
