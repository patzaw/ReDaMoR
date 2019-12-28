###############################################################################@
#' Check that all defined foreign keys are available
#'
#' @export
#'
check_foreign_keys <- function(x, ...){
   UseMethod("check_foreign_keys", x)
}

###############################################################################@
#' Return a table of indexes
#'
#' @export
#'
index_table <- function(x){
   UseMethod("index_table", x)
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
rename_table <- function(x, ...){
   UseMethod("rename_table", x)
}

###############################################################################@
#' @export
#'
add_table <- function(x, ...){
   UseMethod("add_table", x)
}

###############################################################################@
#' @export
#'
remove_table <- function(x, ...){
   UseMethod("remove_table", x)
}

###############################################################################@
#' @export
#'
rename_field <- function(x, ...){
   UseMethod("rename_field", x)
}

###############################################################################@
#' @export
#'
add_field <- function(x, ...){
   UseMethod("add_field", x)
}

###############################################################################@
#' @export
#'
remove_field <- function(x, ...){
   UseMethod("remove_field", x)
}

###############################################################################@
#' @export
#'
set_primary_key <- function(x, ...){
   UseMethod("set_primary_key", x)
}

###############################################################################@
#' @export
#'
add_foreign_key <- function(x, ...){
   UseMethod("add_foreign_key", x)
}

###############################################################################@
#' @export
#'
remove_foreign_key <- function(x, ...){
   UseMethod("remove_foreign_key", x)
}

###############################################################################@
#' @export
#'
add_index <- function(x, ...){
   UseMethod("add_index", x)
}

###############################################################################@
#' @export
#'
remove_index <- function(x, ...){
   UseMethod("remove_index", x)
}

###############################################################################@
#' @export
#'
set_unique_index <- function(x, ...){
   UseMethod("set_unique_index", x)
}

###############################################################################@
#' @export
#'
update_field <- function(x, ...){
   UseMethod("update_field", x)
}

###############################################################################@
#' @export
#'
update_table_display <- function(x, ...){
   UseMethod("update_table_display", x)
}

###############################################################################@
#' @export
#'
auto_layout <- function(x, ...){
   UseMethod("auto_layout", x)
}

###############################################################################@
#' @export
#'
correct_constraints <- function(x, ...){
   UseMethod("correct_constraints", x)
}
