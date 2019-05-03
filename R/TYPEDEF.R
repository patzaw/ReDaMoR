TYPETABLE <- list(
   MySQLWB=tibble(
      ext=c(
         "INT.*", "DECIMAL.*", "TINYINT.*", "VARCHAR.*", "BLOB.*",
         "DATE.*", "DATETIME.*"
      ),
      R=c(
         "integer", "numeric", "logical", "character", "character",
         "Date", "POSIXct"
      )
   ),
   ClickHouse=tibble(
      ext=c("Int32", "Float64", "UInt8", "String", "Date", "DateTime"),
      R=c("integer", "numeric", "logical", "character", "Date", "POSIXct")
   )
)

###############################################################################@
#' Supported R types
#'
#' @export
#'
SUPPTYPES <- c("integer", "numeric", "logical", "character", "Date", "POSIXct")

###############################################################################@
#' List supported types references
#'
#' @export
#'
listTypeRef <- function(){
   names(TYPETABLE)
}

###############################################################################@
#' Normalize type names
#'
#' @param x a character vector to normalize
#' @param typeRef a character vector of length one: the type
#' reference (\code{\link{listTypeRef}})
#'
#' @export
#'
normTypeRef <- function(x, typeRef){
   stopifnot(is.character(x))
   typeRef <- match.arg(typeRef, listTypeRef())
   ntypes <- unique(TYPETABLE[[typeRef]] %>% pull(ext))
   toRet <- x
   for(nt in ntypes){
      toRet <- sub(
         nt,
         nt,
         toRet
      )
   }
   return(toRet)
}

###############################################################################@
#' Convert a set of types from or to R supported types
#'
#' @param x a character vector of types to be converted. If from is not null,
#' x should be a set of valid types in the from reference. If to is not null,
#' x should be a set of supported R types (SUPPTYPES).
#' @param from a character vector of length one: the type reference
#' (\code{\link{listTypeRef}}) of x
#' @param to a character vector of length one: the targeted type reference
#' (\code{\link{listTypeRef}})
#'
#' @details Only `from` XOR `to` should be set
#'
#' @export
#'
typeRefConv <- function(x, from=NULL, to=NULL){
   stopifnot(
      !is.null(from) | !is.null(to),
      is.null(from) | is.null(to)
   )
   if(!is.null(from)){
      r <- match.arg(from, listTypeRef())
      ct <- TYPETABLE[[r]]
      notSupported <- setdiff(x, ct %>% pull(ext))
      if(length(notSupported)>0){
         stop(paste(
            sprintf("The following types are not supported %s types:", r),
            paste(notSupported, collapse=", ")
         ))
      }
      toRet <- ct %>% slice(match(x, ext)) %>% pull(R)
   }
   if(!is.null(to)){
      r <- match.arg(to, listTypeRef())
      ct <- TYPETABLE[[r]]
      checkTypes(x)
      toRet <- ct %>% slice(match(x, R)) %>% pull(ext)
   }
   return(toRet)
}

###############################################################################@
#' Check if a set of types is supported
#'
#' @param x a character vector of types to be checked
#'
#' @export
#'
checkTypes <- function(x){
   stopifnot(is.character(x))
   notSupported <- setdiff(x, SUPPTYPES)
   if(length(notSupported)>0){
      stop(paste(
         "The following types are not supported:",
         paste(notSupported, collapse=", ")
      ))
   }
}

###############################################################################@
#' Convert an object into a specific type
#'
#' @param x an object to convert
#' @param type the targeted type
#'
#' @export
#'
asType <- function(x, type){
   type <- match.arg(type, SUPPTYPES)
   return(switch(
      type,
      "integer"=as.integer(x),
      "numeric"=as.numeric(x),
      "logical"=as.logical(x),
      "character"=as.character(x),
      "Date"=as.Date(x),
      "POSIXct"=as.POSIXct(x)
   ))
}
