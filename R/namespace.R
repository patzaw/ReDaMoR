#' @import magrittr
#' @import dplyr
#' @import shiny
#' @import rintrojs
#' @import visNetwork
#' @import readr
#' @importFrom Matrix sparseMatrix
#' @importFrom utils packageName object.size
#' @importFrom stats rbeta runif complete.cases
#' @importFrom graphics plot
#' @importFrom markdown renderMarkdown
#' @importFrom rstudioapi viewer
#' @importFrom jsonlite fromJSON write_json
#' @importFrom colourpicker colourInput
#' @importFrom DT DTOutput renderDT datatable dataTableProxy replaceData
#' @importFrom DT formatStyle
#' @importFrom crayon bgGreen white bgRed bgMagenta green yellow bold underline
#' @importFrom shinyjs useShinyjs enable disable
#'
NULL
# igraph is required by the auto_layout() function but not imported
