## Read data files ----
to_read <- list.files(
   system.file("examples/HPO-subset", package="ReDaMoR"),
   full.names=TRUE
)
hpo_tables <- list()
for(f in to_read){
   hpo_tables[[sub("[.]txt$", "", basename(f))]] <- readr::read_tsv(f)
}
## Build the model from a list of data frames ----
new_model <- df_to_model(
   list=names(hpo_tables), envir=as.environment(hpo_tables)
)
## Plot the model ----
new_model %>%
   auto_layout(lengthMultiplier=250) %>%
   plot()
