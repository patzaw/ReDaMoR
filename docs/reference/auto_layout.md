# Pre-compute [RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md) layout when missing any x or y table position

Pre-compute
[RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)
layout when missing any x or y table position

## Usage

``` r
auto_layout(
  x,
  layout = "layout_nicely",
  lengthMultiplier = 40 * length(x),
  force = FALSE
)
```

## Arguments

- x:

  a
  [RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)

- layout:

  character name of igraph layout function to use (Default:
  "layout_nicely").

- lengthMultiplier:

  a numeric value to scale x and y coordinate (default: 40\*length(x))

- force:

  if TRUE autolayout even if all tables have coordinates (default:
  FALSE)

## Value

A
[RelDataModel](https://patzaw.github.io/ReDaMoR/reference/RelDataModel.md)
