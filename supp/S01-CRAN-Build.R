library(here)

##############################@
## Build documentation ----
devtools::document(
  pkg = here::here(),
  roclets = c('rd', 'collate', 'namespace')
)
install.packages(here::here(), repos = NULL)

##############################@
## Build and copy vignettes ----
rmarkdown::render(here("README.Rmd"))
tools::buildVignettes(dir = here(), clean = FALSE, quiet = TRUE)
dir.create(here("inst/doc"), showWarnings = FALSE)
for (f in list.files(here("vignettes"), pattern = "\\.(html|pdf|R)$")) {
  file.copy(
    file.path(here("vignettes"), f),
    file.path(here("inst/doc"), f),
    overwrite = TRUE
  )
  if (sub("^.*[.]", "", f) == "html") {
    file.copy(
      file.path(here("vignettes"), f),
      file.path(here("pkgdown/assets"), f),
      overwrite = TRUE
    )
  }
}

##############################@
## Build website ----
unlink(here::here("docs"), recursive = TRUE, force = TRUE)
pkgdown::build_site(pkg = here::here(), preview = FALSE)

##############################@
## Build and check package ----
pv <- desc::desc_get_version(here())
system(paste(
  sprintf("cd %s", here("..")),
  "R CMD build ReDaMoR",
  sprintf("R CMD check --as-cran --no-build-vignettes ReDaMoR_%s.tar.gz", pv),
  sep = " ; "
))
install.packages(here(sprintf("../ReDaMoR_%s.tar.gz", pv)), repos = NULL)
