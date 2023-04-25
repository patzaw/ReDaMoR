TYPETABLE <- list(
   MySQLWB=c(
      "^TINYINT([()].*[)])?$",    "TINYINT",    "logical",
      "^BIGINT([()].*[)])?$",     "BIGINT",     "integer",
      "^INT([()].*[)])?$",        "INT",        "integer",
      "^SMALLINT([()].*[)])?$",   "SMALLINT",   "integer",
      "^DOUBLE([()].*[)])?$",     "DOUBLE",     "numeric",
      "^FLOAT([()].*[)])?$",      "FLOAT",      "numeric",
      "^DECIMAL([()].*[)])?$",    "DECIMAL",    "numeric",
      "^VARCHAR([()].*[)])?$",    "VARCHAR",    "character",
      "^TEXT([()].*[)])?$",       "TEXT",       "character",
      "^TINYTEXT([()].*[)])?$",   "TINYTEXT",   "character",
      "^LONGTEXT([()].*[)])?$",   "LONGTEXT",   "character",
      "^MEDIUMTEXT([()].*[)])?$", "MEDIUMTEXT", "character",
      "^BLOB([()].*[)])?$",       "BLOB",       "base64",
      "^LONGBLOB([()].*[)])?$",   "LONGBLOB",   "base64",
      "^MEDIUMBLOB([()].*[)])?$", "MEDIUMBLOB", "base64",
      "^TINYBLOB([()].*[)])?$",   "TINYBLOB",   "base64",
      "^DATETIME([()].*[)])?$",   "DATETIME",   "POSIXct",
      "^DATE([()].*[)])?$",       "DATE",       "Date",
      "^ENUM([()].*[)])?$",       "ENUM",       "character",
      "^SET([()].*[)])?$",        "SET",        "character"
   ) %>%
      matrix(
         ncol=3, byrow=TRUE,
         dimnames=list(NULL, c("match", "inst", "R"))
      ) %>%
      dplyr::as_tibble(),
   ClickHouse=c(
      "Int32",           "Int32",           "integer",
      "Float64",         "Float64",         "numeric",
      "UInt8",           "UInt8",           "logical",
      "String",          "String",          "character",
      "Date",            "Date",            "Date",
      "DateTime",        "DateTime",        "POSIXct",
      "Array(String)",   "Array(String)",   "base64"
   ) %>%
      matrix(
         ncol=3, byrow=TRUE,
         dimnames=list(NULL, c("match", "inst", "R"))
      ) %>%
      dplyr::as_tibble()
)

###############################################################################@
#' Supported R types
#'
#' @export
#'
SUPPTYPES <- c(
   "integer", "numeric", "logical", "character", "Date", "POSIXct", "base64"
)

###############################################################################@
#' List supported types references
#'
#' @export
#'
list_type_ref <- function(){
   names(TYPETABLE)
}

###############################################################################@
#' Normalize type names
#'
#' @param x a character vector to normalize
#' @param typeRef a character vector of length one: the type
#' reference ([list_type_ref])
#' @param ignore.case should case be ignored (default: TRUE)
#'
#' @export
#'
norm_type_ref <- function(x, typeRef, ignore.case=TRUE){
   stopifnot(is.character(x))
   typeRef <- match.arg(typeRef, list_type_ref())
   ntypes <- unique(TYPETABLE[[typeRef]] %>% dplyr::pull("match"))
   toRet <- x
   for(nt in ntypes){
      toRet <- sub(
         nt,
         nt,
         toRet,
         ignore.case=ignore.case
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
#' ([list_type_ref]) of x
#' @param to a character vector of length one: the targeted type reference
#' ([list_type_ref])
#' @param ignore.case should case be ignored when converting `from``
#' type reference (default: TRUE)
#'
#' @details Only `from` XOR `to` should be set
#'
#' @export
#'
conv_type_ref <- function(x, from=NULL, to=NULL, ignore.case=TRUE){
   stopifnot(
      !is.null(from) | !is.null(to),
      is.null(from) | is.null(to)
   )
   if(!is.null(from)){
      r <- match.arg(from, list_type_ref())
      ct <- TYPETABLE[[r]]
      x <- norm_type_ref(x, from, ignore.case=ignore.case)
      notSupported <- setdiff(x, ct %>% dplyr::pull("match"))
      if(length(notSupported)>0){
         stop(paste(
            sprintf("The following types are not supported %s types:", r),
            paste(notSupported, collapse=", ")
         ))
      }
      toRet <- ct %>% dplyr::slice(match(x, .data$match)) %>% dplyr::pull("R")
   }
   if(!is.null(to)){
      r <- match.arg(to, list_type_ref())
      ct <- TYPETABLE[[r]]
      check_types(x)
      toRet <- ct %>% dplyr::slice(match(x, .data$R)) %>% dplyr::pull("inst")
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
check_types <- function(x){
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
as_type <- function(x, type){
   type <- match.arg(type, SUPPTYPES)
   return(switch(
      type,
      "integer"=as.integer(x),
      "numeric"=as.numeric(x),
      "logical"=as.logical(x),
      "character"=as.character(x),
      "Date"=as.Date(x),
      "POSIXct"=as.POSIXct(x),
      "base64"=if(length(x)==0){
         character(0)
      }else{
         unlist(lapply(x, function(y){
            if(length(y)==0 || (length(y)==1 && is.na(y))){
               return(NA)
            }else{
               paste(y, collapse="")
            }
         }))
      }
   ))
}
