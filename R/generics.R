###############################################################################@
#' Check that all defined foreign keys are available
#'
#' @export
#'
checkForeignKeys <- function(x, ...){
   UseMethod("checkForeignKeys", x)
}

###############################################################################@
#' Return a table of indexes
#'
#' @export
#'
indexTable <- function(x){
   UseMethod("indexTable", x)
}

###############################################################################@
#' Return a type specification string
#'
#' @export
#'
col_types <- function(x){
   UseMethod("col_types", x)
}

###############################################################################@
#' Interactive exploration of an object
#'
#' @export
#'
explore <- function(x, ...){
   UseMethod("explore", x)
}
