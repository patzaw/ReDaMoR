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
#' @import dplyr
#' @importFrom magrittr %>%
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
   for(
      att in setdiff(
         names(attributes(l$fields)), c("row.names", "names", "class")
      )
   ){
      attr(l$fields, att) <- NULL
   }
   l$fields <- as_tibble(l$fields) %>%
      select(c("name", "type", "nullable", "unique", "comment"))

   ## * Primary key ----
   l$primaryKey <- sort(l$primaryKey)
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
            for(
               att in setdiff(
                  names(attributes(fk$key)), c("row.names", "names", "class")
               )
            ){
               attr(fk$key, att) <- NULL
            }
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
            ind$fields <- sort(unique(ind$fields))
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
   toRet <- correct_constraints(toRet)
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
#' @param ... for generics compatibility (not used)
#'
#' @return A single character
#'
#' @export
#'
format.RelTableModel <- function(x, ...){
   f <- x$fields
   pk <- x$primaryKey
   it <- index_table(x)
   ind <- NULL
   # uq <- NULL
   if(!is.null(it)){
      it <- it %>% filter(.data$index!=0)
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
#' @export
#'
length.RelTableModel <- function(x){
   nrow(x$fields)
}


###############################################################################@
#' @export
#'
lengths.RelTableModel <- function(x, use.names=TRUE){
   lengths(unclass(x), use.names=use.names)
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
index_table <- function(x){
   stopifnot(is.RelTableModel(x))
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
#'
#' @return A col_spec object with the type of each column
#'
#' @export
#'
col_types <- function(x){
   stopifnot(is.RelTableModel(x))
   do.call(
      readr::cols,
      structure(
         lapply(
            x$fields$type,
            function(y){
               switch(
                  y,
                  "integer"=readr::col_integer(),
                  "numeric"=readr::col_double(),
                  "logical"=readr::col_logical(),
                  "character"=readr::col_character(),
                  "Date"=readr::col_date(),
                  "POSIXct"=readr::col_datetime()
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
correct_constraints <- function(x){
   stopifnot(is.RelTableModel(x))
   ## Primary key uniqueness
   if(length(x$primaryKey)==1){
      x$fields[which(x$fields$name==x$primaryKey), "unique"] <- TRUE
      x$fields[which(x$fields$name==x$primaryKey), "nullable"] <- FALSE
   }
   if(length(x$primaryKey)>0){
      ei <- lapply(
         x$indexes,
         function(y){
            identical(sort(y$fields), sort(x$primaryKey))
         }
      ) %>%
         unlist() %>%
         as.logical() %>%
         which()
      if(length(ei)>1) stop("Check this part of code")
      if(length(ei)==0){
         x$indexes <- unique(c(
            x$indexes,
            list(
               list(
                  fields=sort(x$primaryKey),
                  unique=TRUE
               )
            )
         ))
      }else{
         x$indexes[[ei]]$unique <- TRUE
      }
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
   uniqueFields <- x$fields %>% filter(.data$unique) %>% pull("name")
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

###############################################################################@
#' Confront a [RelTableModel] to actual data
#'
#' @param x a [RelTableModel]
#' @param d a data frame
#' @param checks a character vector with the name of optional checks to be done
#' (Default: all of them c("unique", "not nullable"))
#'
#' @return A report as a list
#'
#' @export
#'
confront_table_data <- function(
   x,
   d,
   checks=c("unique", "not nullable")
){
   stopifnot(is.RelTableModel(x))
   stopifnot(
      is.data.frame(d)
   )
   ## Optional checks ----
   if(length(checks)>0){
      checks <- match.arg(
         checks,
         c("unique", "not nullable"),
         several.ok=TRUE
      )
   }
   ## Available fields ----
   missingFields <- setdiff(x$fields$name, colnames(d))
   suppFields <- setdiff(colnames(d), x$fields$name)
   availableFields <- intersect(colnames(d), x$fields$name)
   toRet <- list(
      missingFields=missingFields,
      suppFields=suppFields,
      availableFields=availableFields,
      fields=list(),
      success=length(missingFields)==0 && length(suppFields)==0
   )
   ## Fields ----
   for(i in 1:nrow(x$fields)){
      fn <- x$fields$name[i]
      ft <- x$fields$type[i]
      fe <- x$fields$nullable[i]
      fu <- x$fields$unique[i]
      toRet$fields[[fn]] <- list(success=TRUE, message=NULL)
      if(!inherits(pull(d, !!fn), ft)){
         toRet$fields[[fn]]$success <- FALSE
         toRet$success <- FALSE
         toRet$fields[[fn]]$message <- paste(c(
            toRet$fields[[fn]]$message,
            sprintf(
               'Unexpected "%s"',
               paste(class(pull(d, !!fn)), collapse=", ")
            )
         ), collapse=" ")
      }
      if("not nullable" %in% checks){
         mis <- sum(is.na(pull(d, !!fn)))
         if(mis!=0){
            toRet$fields[[fn]]$message <- paste(c(
               toRet$fields[[fn]]$message,
               sprintf(
                  'Missing values %s/%s = %s%s',
                  mis, nrow(d), round(mis*100/nrow(d)), "%"
               )
            ), collapse=" ")
            if(!fe){
               toRet$fields[[fn]]$success <- FALSE
               toRet$success <- FALSE
            }
         }
      }
      if("unique" %in% checks && fu && any(duplicated(pull(d, !!fn)))){
         toRet$fields[[fn]]$success <- FALSE
         toRet$success <- FALSE
         toRet$fields[[fn]]$message <- paste(c(
            toRet$fields[[fn]]$message,
            "Some values are duplicated"
         ), collapse=" ")
      }
   }
   ## Indexes ----
   if("unique" %in% checks && length(x$indexes)>0){
      toRet$indexes <- list()
      for(i in 1:length(x$indexes)){
         toRet$indexes[[i]] <- list()
         idx <- x$indexes[[i]]
         if(!idx$unique){
            toRet$indexes[[i]]$success <- TRUE
         }else{
            if(any(duplicated(d[,idx$fields]))){
               toRet$indexes[[i]]$success <- FALSE
               toRet$indexes[[i]]$message <- paste(c(
                  toRet$indexes[[i]]$message,
                  "Some values are duplicated"
               ), collapse=" ")
               toRet$success <- FALSE
            }else{
               toRet$indexes[[i]]$success <- TRUE
            }
         }
      }
   }

   ## Return the results ----
   return(toRet)
}


###############################################################################@
#' Check if two [RelTableModel] are identical
#'
#' @param x a [RelTableModel]
#' @param y a [RelTableModel]
#' @param includeDisplay a single logical (default: TRUE) indicating if
#' the display should be included in the comparison
#'
#' @return A logical: TRUE if the 2 models are identical
#'
#' @export
#'
identical_RelTableModel <- function(x, y, includeDisplay=TRUE){
   stopifnot(is.RelTableModel(x), is.RelTableModel(y))

   ## Name ----
   toRet <- x$tableName==y$tableName

   ## Fields ----
   toRet <- toRet && identical(
      x$fields %>% arrange(.data$name),
      y$fields %>% arrange(.data$name)
   )

   ## Primary key ----
   toRet <- toRet && length(x$primaryKey)==length(y$primaryKey) &&
      all(x$primaryKey==y$primaryKey)

   ## Indexes ----
   toRet <- toRet && length(x$indexes)==length(y$indexes)
   if(toRet && length(x$indexes)>0){
      xidx <- lapply(
         x$indexes,
         function(z){
            z$fields <- sort(z$fields)
            return(z)
         }
      )
      xidx <- xidx[order(unlist(lapply(
         xidx,
         function(z) paste(z$fields, collapse=", ")
      )))]
      yidx <- lapply(
         y$indexes,
         function(z){
            z$fields <- sort(z$fields)
            return(z)
         }
      )
      yidx <- yidx[order(unlist(lapply(
         yidx,
         function(z) paste(z$fields, collapse=", ")
      )))]
      toRet <- identical(xidx, yidx)
   }

   ## Foreign keys ----
   toRet <- toRet && length(x$foreignKeys)==length(y$foreignKeys)
   if(toRet && length(x$foreignKeys)>0){
      xfk <- lapply(
         x$foreignKeys,
         function(z){
            z$key <- z$key %>% arrange(.data$from, .data$to)
            return(z)
         }
      )
      xfk <- xfk[order(unlist(lapply(
         xfk,
         function(z){
            paste0(
               z$refTable, " [",
               paste(
                  paste(z$key$from, z$key$to, sep="->"),
                  collapse=", "
               ),
               "]"
            )
         }
      )))]
      yfk <- lapply(
         y$foreignKeys,
         function(z){
            z$key <- z$key %>% arrange(.data$from, .data$to)
            return(z)
         }
      )
      yfk <- yfk[order(unlist(lapply(
         yfk,
         function(z){
            paste0(
               z$refTable, " [",
               paste(
                  paste(z$key$from, z$key$to, sep="->"),
                  collapse=", "
               ),
               "]"
            )
         }
      )))]
      toRet <- identical(xfk, yfk)
   }

   ## Display ----
   if(includeDisplay){
      toRet <- toRet && length(x$display)==length(y$display) &&
         (length(x$display)==0 || identical(
            x$display[order(names(x$display))],
            y$display[order(names(y$display))]
         ))
   }
   return(toRet)
}

###############################################################################@
#' Get foreign keys from [RelTableModel]
#'
#' @param x a [RelTableModel]
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
get_foreign_keys.RelTableModel <- function(x){
   tn <- x$tableName
   fk <- x$foreignKeys
   if(length(fk)==0){
      return(NULL)
   }
   toRet <- do.call(rbind, lapply(
      fk,
      function(k){
         to <- k$refTable
         kt <- k$key %>% arrange(.data$from, .data$to)
         tibble(
            to=to,
            ff=list(kt$from), tf=list(kt$to)
         ) %>%
            bind_cols(as_tibble(t(k$cardinality))) %>%
            return()
      }
   ))
   toRet %>% mutate(
      from=tn
   ) %>%
      select("from", "ff", "to", "tf", "fmin", "fmax", "tmin", "tmax") %>%
      return()
}
