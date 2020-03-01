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
   verbose=FALSE,
   returnData=TRUE
)
## Show the report in console ----
format_confrontation_report(confrontation_report) %>% cat()
## Format the report using markdown ----
format_confrontation_report_md(confrontation_report) %>% cat()
