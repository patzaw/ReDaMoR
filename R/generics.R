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


###############################################################################@
#' Lengths of object elements
#'
#' @param x an object. If there is no method implemented for this object,
#' the [base::lengths()] function is used.
#' @param use.names logical indicating if the result should
#' inherit the names from x.
#'
#' @return A non-negative integer of length length(x), except when any element
#' has a length of more than 2^31 - 1 elements, when it returns a double vector.
#' When use.names is true, the names are taken from the names on x, if any.
#'
#' @seealso [base::lengths()]
#'
#' @export
lengths <- function(x, use.names=TRUE){
   UseMethod("lengths", x)
}
#' @export
lengths.default <- function(x, use.names=TRUE){
   base::lengths(unclass(x), use.names=use.names)
}

