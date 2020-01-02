###############################################################################@
#' Create a RelTableModel object
#'
#' @param l the list with the following elements
#' - **tableName**: a character vector of length one
#' - **fields**: a tibble with the follwoing columns:
#'    + *name*: character
#'    + *type*: character
#'    + *nullable*: logical
#'    + *comment*:  character
#' - **primaryKey**: a character vector of any length. All
#' values should be in fields$name
#' - **foreignKeys**: a list of foreign keys. Each foreigned key is defined
#' as a list with the following elements:
#'    + *refTable*: a character vector of length one (the referenced table)
#'    + *key*: a tibble with a "from" and a "to" columns
#'    + (*cardinality*): an optional integer vector with 4 values:
#'       - fmin: from minimum cardinality
#'       - fmax: from maximum cardinality
#'       - tmin: to minimum cardinality
#'       - tmax: to maximum cardinality
#' - **indexes**: a list of indexes. Each index is defined by
#' 3 columns:
#'    + *field*: character (all in fields$name)
#'    + *order*: character
#'    + *unique*: logical
#' - **display**: a list gathering:
#'    + *x*: single numeric value for the x position of the table
#'    + *y*: single numeric value for the y position of the table
#'    + *color*: single character value corresponding to the color of the table
#'    + *comment*: single character value with some description of the table
#'
#' @return A RelTableModel object.
#'
#' @export
#'
RelTableModel <- function(l){

   ############################################################################@
   ## Checks ----

   ## * Simple checks ----
   tableInfo <- c(
      # "dbName",
      "tableName", "fields",
      "primaryKey", "foreignKeys", "indexes",
      "display"
   )
   fieldInfo <- c("name", "type", "nullable", "unique", "comment")
   stopifnot(
      all(tableInfo %in% names(l)),
      all(names(l) %in% tableInfo),

      # is.character(l$dbName),
      # length(l$dbName)==1,
      # !is.na(l$dbName),

      is.character(l$tableName),
      length(l$tableName)==1,
      !is.na(l$tableName),

      is.data.frame(l$fields),
      all(fieldInfo %in% colnames(l$fields)),
      all(colnames(l$fields) %in% fieldInfo),
      # nrow(l$fields) > 0,
      is.character(l$fields$name),
      all(!is.na(l$fields$name)),
      is.character(l$fields$type),
      all(!is.na(l$fields$type)),
      is.logical(l$fields$nullable),
      all(!is.na(l$fields$nullable)),
      is.logical(l$fields$unique),
      all(!is.na(l$fields$unique)),
      is.character(l$fields$comment),

      is.null(l$primaryKey) || is.character(l$primaryKey),
      all(!is.na(l$primaryKey)),
      all(l$primaryKey %in% l$fields$name)
   )
   l$fields <- as_tibble(l$fields) %>%
      select(c("name", "type", "nullable", "unique", "comment"))
   if(length(l$primaryKey)==1){
      l$fields[
         which(l$fields$name==l$primaryKey),
         "unique"
      ] <- TRUE
   }

   ## * Field types ----
   check_types(l$fields$type)

   ## * Foreign keys ----
   if(!is.null(l$foreignKeys)){
      stopifnot(is.list(l$foreignKeys))
      fkn <- c("refTable", "key")
      l$foreignKeys <- lapply(
         l$foreignKeys,
         function(fko){
            fk <- fko[fkn]
            stopifnot(
               all(names(fk) %in% fkn),
               all(fkn %in% names(fk)),
               !is.na(fk$refTable),
               is.character(fk$refTable),
               length(fk$refTable)==1,
               !is.na(fk$refTable),
               is.data.frame(fk$key),
               all(c("from", "to") %in% names(fk$key)),
               all(names(fk$key) %in% c("from", "to")),
               is.character(fk$key$from),
               all(!is.na(fk$key$from)),
               is.character(fk$key$to),
               all(!is.na(fk$key$to)),
               all(fk$key$from %in% l$fields$name)
            )
            fk$key <- as_tibble(fk$key)
            cnames <- c("fmin", "fmax", "tmin", "tmax")
            if("cardinality" %in% names(fko)){
               stopifnot(
                  is.integer(fko$cardinality),
                  length(fko$cardinality)==4,
                  all(
                     cnames %in%
                        names(fko$cardinality)
                  ),
                  all(!is.na(fko$cardinality)),
                  all(fko$cardinality >= -1),
                  fko$cardinality["fmin"] > -1,
                  fko$cardinality["tmin"] > -1,
                  fko$cardinality["fmax"]==-1 || (
                     fko$cardinality["fmax"] > 0 &&
                     fko$cardinality["fmax"] >= fko$cardinality["fmin"]
                  ),
                  fko$cardinality["tmax"]==-1 || (
                     fko$cardinality["tmax"] > 0 &&
                     fko$cardinality["tmax"] >= fko$cardinality["tmin"]
                  )
               )
               fk$cardinality <- fko$cardinality[cnames]
            }else{
               fk$cardinality <- c(0, -1, 1, 1)
               names(fk$cardinality) <- cnames
            }
            return(fk)
         }
      )
      names(l$foreignKeys) <- NULL
   }

   ## * Indexes ----
   if(!is.null(l$indexes)){
      stopifnot(is.list(l$indexes))
      # idn <- c("field", "unique")
      idn <- c("fields", "unique")
      l$indexes <- lapply(
         l$indexes,
         function(ind){
            stopifnot(
               is.list(ind),
               all(names(ind) %in% idn),
               all(idn %in% names(ind)),
               is.character(ind$fields),
               all(!is.na(ind$fields)),
               is.logical(ind$unique),
               all(!is.na(ind$unique)),
               length(ind$unique)==1,
               all(ind$fields %in% l$fields$name)
            )
            ind$fields <- unique(ind$fields)
            return(ind)
         }
      )
   }

   ## * Display ----
   if(!is.null(l$display)){
      stopifnot(is.list(l$display))
      dn <- c("x", "y", "color", "comment")
      stopifnot(
         all(names(l$display) %in% dn),
         all(dn %in% names(l$display)),
         is.numeric(l$display$x),
         is.numeric(l$display$y),
         is.character(l$display$color),
         is.character(l$display$comment),
         all(unlist(lapply(l$display, length))==1)
      )
   }

   ############################################################################@
   ## Creating the object ----
   toRet <- l
   class(toRet) <- c("RelTableModel", class(toRet))
   return(toRet)

}

###############################################################################@
#' Check the object is  a [RelTableModel] object
#'
#' @param x any object
#'
#' @return A single logical: TRUE if x is a [RelTableModel] object
#'
#' @export
#'
is.RelTableModel <- function(x){
   inherits(x, "RelTableModel")
}

###############################################################################@
#' Format a [RelTableModel] object for printing
#'
#' @param x a [RelTableModel] object
#'
#' @return A single character
#'
#' @export
#'
format.RelTableModel <- function(x){
   f <- x$fields
   pk <- x$primaryKey
   it <- index_table(x)
   ind <- NULL
   # uq <- NULL
   if(!is.null(it)){
      it <- it %>% filter(index!=0)
      ind <- unique(it$field)
      # uq <- unique(it$field[which(it$unique)])
   }
   f$i <- unlist(lapply(
      f$name,
      function(n){
         paste(sort(it$index[which(it$field==n)]), collapse=",")
      }
   ))
   toRet <- paste0(
      # sprintf("Database: %s", x$dbName), "\n",
      sprintf("Table name: %s", x$tableName), "\n",
      paste(
         apply(
            f, 1,
            function(y){
               toRet <- "   "
               toRet <- paste0(
                  toRet,
                  ifelse(y[1] %in% pk, "*", " "),
                  # ifelse(y[1] %in% uq, "+ ", "  "),
                  ifelse(y[4]=="TRUE" & !y[1] %in% pk, "+ ", "  "),
                  y[1]
               )
               toRet <- paste0(
                  toRet, " (",
                  y[2],
                  ifelse(
                     y[1] %in% ind,
                     # paste0(", idx.", y[5]),
                     paste0(", idx.", y[6]),
                     ""
                  ),
                  ifelse(y[3]=="FALSE", ", not", ","),
                  " nullable",
                  ")"
               )
               return(toRet)
            }
         ),
         collapse="\n"
      )
   )
   if(length(x$foreignKeys)>0){
      toRet <- paste0(
         toRet, "\n",
         "Referenced tables:\n",
         paste(unlist(lapply(
            x$foreignKeys,
            function(y){
               y$refTable
            }
         )), collapse="\n")
      )
   }
   return(toRet)
}

###############################################################################@
#' @export
#'
print.RelTableModel <- function(x, ...){
   cat(format(x, ...), "\n")
}

###############################################################################@
#' List indexes of a [RelTableModel] object
#'
#' @param x a [RelTableModel] object
#'
#' @return A tibble with the following columns:
#' - **index**: an integer corresponding to the index number
#' - **field**: a character corresponding to field belonging to the index
#' - **unique**: a logical indicating the uniqueness of the field
#'
#' @export
#'
index_table.RelTableModel <- function(x){
   pk <- x$primaryKey
   ind <- x$indexes
   toRet <- NULL
   i <- 0
   if(length(pk)>0){
      toRet <- tibble(
         index=i,
         field=pk,
         # unique=length(pk)==1
         uniqueIndex=TRUE
      )
   }
   for(ci in ind){
      i <- i+1
      toRet <- bind_rows(
         toRet,
         tibble(
            index=i,
            field=ci$fields,
            uniqueIndex=ci$unique
         )
      )
   }
   return(toRet)
}

###############################################################################@
#' Get the types of the columns of a [RelTableModel] object
#'
#' @param x a [RelTableModel] object
#' @return A col_spec object with the type of each column
#'
#' @importFrom readr cols col_character col_double col_integer col_logical
#' @importFrom readr col_date col_datetime
#'
#' @export
#'
col_types.RelTableModel <- function(x){
   do.call(
      cols,
      structure(
         lapply(
            x$fields$type,
            function(y){
               switch(
                  y,
                  "integer"=col_integer(),
                  "numeric"=col_double(),
                  "logical"=col_logical(),
                  "character"=col_character(),
                  "Date"=col_date(),
                  "POSIXct"=col_datetime()
               )
            }
         ),
         .Names=x$fields$name
      )
   )
}

###############################################################################@
#' Correct the constraints of a table to make them consistent
#'
#' @param x a [RelTableModel] object
#'
#' @export
#'
correct_constraints.RelTableModel <- function(x){
   ## Primary key uniqueness
   if(length(x$primaryKey)==1){
      x$fields[which(x$fields$name==x$primaryKey), "unique"] <- TRUE
      x$fields[which(x$fields$name==x$primaryKey), "nullable"] <- FALSE
   }
   ## Index uniqueness
   if(length(x$indexes)>0){
      for(i in 1:length(x$indexes)){
         if(x$indexes[[i]]$unique && length(x$indexes[[i]]$fields)==1){
            x$fields[
               which(x$fields$name==x$indexes[[i]]$fields), "unique"
               ] <- TRUE
         }
      }
   }
   ## Field uniqueness
   uniqueFields <- x$fields %>% filter(unique) %>% pull(name)
   if(length(x$indexes)>0 && length(uniqueFields)>0){
      for(i in 1:length(x$indexes)){
         if(any(x$indexes[[i]]$fields %in% uniqueFields)){
            x$indexes[[i]]$unique <- TRUE
         }
      }
   }
   ##
   return(x)
}

