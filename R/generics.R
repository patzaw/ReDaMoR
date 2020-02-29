###############################################################################@
#' Get a foreign key table from an object
#'
#' @param x a [RelTableModel] or a [RelDataModel]
#'
#' @return A tibble with the following fields:
#' - from: the origin of the key
#' - ff: the key fields in from
#' - to: the target of the key
#' - tf: the key fields in to
#' - fmin: minimum cardinality of from
#' - fmax: maximum cardinality of from
#' - tmin: minimum cardinality of to
#' - tmax: maximum cardinality of to
#'
#' @export
#'
get_foreign_keys <- function(x){
   UseMethod("get_foreign_keys", x)
}
