# Relational data modeler GUI

Relational data modeler GUI

## Usage

``` r
model_relational_data(
  modelInput = RelDataModel(list()),
  fromR = interactive(),
  defaultColor = "#D9D9D9",
  availableColors = c("#9BC8FE", "#F67FC4", "#C6BDF1", "#DFFB86", "#F8DEC3", "#8FE6E0",
    "#FEFE8F", "#FAC6DC", "#A9ECC9"),
  example = system.file("examples/HPO-model.json", package = utils::packageName()),
  forceIntro = FALSE
)
```

## Arguments

- modelInput:

  the
  [RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)
  to start from

- fromR:

  a logical indicating if the application is launched from R

- defaultColor:

  a single color indicating the default table color

- availableColors:

  a character of possible colors for tables

- example:

  a file path to an sql or json model

- forceIntro:

  if TRUE the help tour start when the application is launched (default:
  FALSE)

## Value

The
[RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)
designed with the GUI.
