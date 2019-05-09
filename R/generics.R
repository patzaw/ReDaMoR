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

###############################################################################@
#' @export
#'
renameTable <- function(x, ...){
   UseMethod("renameTable", x)
}

###############################################################################@
#' @export
#'
addTable <- function(x, ...){
   UseMethod("addTable", x)
}

###############################################################################@
#' @export
#'
removeTable <- function(x, ...){
   UseMethod("removeTable", x)
}

###############################################################################@
#' @export
#'
renameField <- function(x, ...){
   UseMethod("renameField", x)
}

###############################################################################@
#' @export
#'
addField <- function(x, ...){
   UseMethod("addField", x)
}

###############################################################################@
#' @export
#'
removeField <- function(x, ...){
   UseMethod("removeField", x)
}

###############################################################################@
#' @export
#'
setPK <- function(x, ...){
   UseMethod("setPK", x)
}

###############################################################################@
#' @export
#'
addFK <- function(x, ...){
   UseMethod("addFK", x)
}

###############################################################################@
#' @export
#'
removeFK <- function(x, ...){
   UseMethod("removeFK", x)
}

###############################################################################@
#' @export
#'
addIndex <- function(x, ...){
   UseMethod("addIndex", x)
}

###############################################################################@
#' @export
#'
removeIndex <- function(x, ...){
   UseMethod("removeIndex <- function(x, ...){
   UseMethod", x)
}

###############################################################################@
#' @export
#'
updateTableDisplay <- function(x, ...){
   UseMethod("updateTableDisplay", x)
}

###############################################################################@
#' @export
#'
updateField <- function(x, ...){
   UseMethod("updateField", x)
}


