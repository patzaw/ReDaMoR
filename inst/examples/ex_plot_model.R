## Read the model ----
hpo_model <- read_json_data_model(
   system.file("examples/HPO-model.json", package="ReDaMoR")
)
## Plot the model ----
plot(hpo_model)
