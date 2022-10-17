###############################################################################@
#' Create a RelTableModel object
#'
#' @param l DEPRECATED. A named list with the function parameters.
#' If `NULL` (default) the function parameters are used. If not `NULL`, the
#' function parameters are ignored and taken from l.
#' @param tableName a character vector of length one
#' @param fields a tibble with the following columns:
#'    - *name*: character
#'    - *type*: character
#'    - *nullable*: logical
#'    - *comment*:  character
#' @param primaryKey a character vector of any length. All
#' values should be in fields$name
#' @param foreignKeys a list of foreign keys. Each foreign key is defined
#' as a list with the following elements:
#'    - *refTable*: a character vector of length one (the referenced table)
#'    - *key*: a tibble with a "from" and a "to" columns
#'    - (*cardinality*): an optional integer vector with 4 values:
#'       + fmin: from minimum cardinality
#'       + fmax: from maximum cardinality
#'       + tmin: to minimum cardinality
#'       + tmax: to maximum cardinality
#' @param indexes a list of indexes. Each index is defined by
#' 3 columns:
#'    - *field*: character (all in fields$name)
#'    - *order*: character
#'    - *unique*: logical
#' @param display a list gathering:
#'    - *x*: single numeric value for the x position of the table
#'    - *y*: single numeric value for the y position of the table
#'    - *color*: single character value corresponding to the color of the table
#'    - *comment*: single character value with some description of the table
#'
#' @details When defining a matrix, 3 and only 3 fields must be defined:
#' 2 of types 'row' and 'column' and the 3rd of your choice. In this case
#' primaryKey is defined automatically as the combination of row and column.
#'
#' @return A RelTableModel object.
#'
#' @export
#'
RelTableModel <- function(
      l=NULL,
      tableName,
      fields,
      primaryKey=NULL,
      foreignKeys=NULL,
      indexes=NULL,
      display=NULL

){

   ############################################################################@
   ## Management of params ----
   if(!is.null(l)){
      warning("l param is deprecated and will probably removed in the future")
   }else{
      l <- list(
         tableName=tableName,
         fields=fields,
         primaryKey=primaryKey,
         foreignKeys=foreignKeys,
         indexes=indexes,
         display=display
      )
   }

   ############################################################################@
   ## Checks ----

   ## * Simple checks ----
   tableInfo <- c(
      "tableName", "fields",
      "primaryKey", "foreignKeys", "indexes",
      "display"
   )
   fieldInfo <- c("name", "type", "nullable", "unique", "comment")
   stopifnot(
      all(tableInfo %in% names(l)),
      all(names(l) %in% tableInfo),

      is.character(l$tableName),
      length(l$tableName)==1,
      !is.na(l$tableName),

      is.data.frame(l$fields),
      all(fieldInfo %in% colnames(l$fields)),
      all(colnames(l$fields) %in% fieldInfo),
      # nrow(l$fields) > 0,
      is.character(l$fields$name),
      all(!is.na(l$fields$name)),
      !any(duplicated(l$fields$name)),
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
   l$fields <- dplyr::as_tibble(l$fields) %>%
      dplyr::select(c("name", "type", "nullable", "unique", "comment")) %>%
      dplyr::mutate("comment"=as.character(
         ifelse(is.na(.data$comment), "", .data$comment)
      ))

   ## * Primary key ----
   l$primaryKey <- sort(l$primaryKey)
   if(length(l$primaryKey)==1){
      l$fields[
         which(l$fields$name==l$primaryKey),
         "unique"
      ] <- TRUE
   }

   ## * Matrix and Field types ----
   if(any(l$fields$type %in% c("row", "column"))){
      if(!all(c("row", "column") %in% l$fields$type)){
         stop("Both row and column should be provided when modelling a matrix")
      }
      if(nrow(l$fields) !=3 ){
         stop("A matrix model should have 3 and only 3 fields")
      }
      if(length(unique(l$fields$type)) != 3){
         stop(paste(
            "A matrix model should have 3 fields":
            "2 of types 'row' and 'column' and the 3rd of your choice"
         ))
      }
      rcfields <- l$fields %>% dplyr::filter(.data$type %in% c("row", "column"))
      if(any(rcfields$nullable)){
         stop("Matrix row and column cannot be nullable")
      }
      if(any(rcfields$unique)){
         stop(paste(
            "The combination of row and column is unique (primary key)",
            " but not row names and column names independently"
         ))
      }
      l$primaryKey <- rcfields$name
      if(setdiff(l$fields$type, c("row", "column"))=="base64"){
         stop("A matrix cannot store base64 documents")
      }
      check_types(setdiff(l$fields$type, c("row", "column")))
   }else{
      check_types(l$fields$type)
   }

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
            fk$key <- dplyr::as_tibble(fk$key)
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
#' Check if the object is  a [RelTableModel] object
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
#' Check if the object is a [RelTableModel] matrix object
#'
#' A matrix model is a special [RelTableModel] object with 3 and only 3 fields:
#' 2 of types 'row' and 'column' and the 3rd of your choice.
#'
#' @param x any object
#'
#' @return A single logical: TRUE if x is a [RelTableModel] matrix object
#'
#' @export
#'
is.MatrixModel <- function(x){
   inherits(x, "RelTableModel") &&
      "row" %in% x$fields$type
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
      it <- it %>% dplyr::filter(.data$index!=0)
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
      sprintf(
         "Table name: %s%s",
         x$tableName,
         ifelse(is.MatrixModel(x), " (matrix)", "")
      ), "\n",
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
      toRet <- dplyr::tibble(
         index=i,
         field=pk,
         # unique=length(pk)==1
         uniqueIndex=TRUE
      )
   }
   for(ci in ind){
      i <- i+1
      toRet <- dplyr::bind_rows(
         toRet,
         dplyr::tibble(
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
                  "row"=readr::col_character(),
                  "column"=readr::col_character(),
                  "integer"=readr::col_integer(),
                  "numeric"=readr::col_double(),
                  "logical"=readr::col_logical(),
                  "character"=readr::col_character(),
                  "Date"=readr::col_date(),
                  "POSIXct"=readr::col_datetime(),
                  "base64"=readr::col_character()
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
            if(
               x$fields[
                  which(x$fields$name==x$indexes[[i]]$fields), "type"
               ] %in% c("row", "column")
            ){
               stop("Matrix row and column cannot be unique individually")
            }
            x$fields[
               which(x$fields$name==x$indexes[[i]]$fields), "unique"
               ] <- TRUE
         }
      }
   }
   ## Field uniqueness
   uniqueFields <- x$fields %>%
      dplyr::filter(.data$unique) %>%
      dplyr::pull("name")
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
#' @param d a data frame or a matrix for matrix model
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
   ## Optional checks ----
   if(length(checks)>0){
      checks <- match.arg(
         checks,
         c("unique", "not nullable"),
         several.ok=TRUE
      )
   }
   stopifnot(is.RelTableModel(x))
   ## Matrix model ----
   if(is.MatrixModel(x)){
      if(!inherits(d, c("matrix", "Matrix"))){
         stop(x$tableName, " is neither a matrix nor a Matrix")
      }
      vf <- x$fields %>% dplyr::filter(!.data$type %in% c("row", "column"))
      toRet <- list(
         missingFields = character(0),
         suppFields = character(0),
         availableFields = vf$name,
         fields=list(),
         success=TRUE
      )
      toRet$fields[[vf$name]] <- list(success=TRUE, message=NULL)
      if(!inherits(d[1], vf$type)){
         toRet$fields[[vf$name]]$success <- FALSE
         toRet$success <- FALSE
         toRet$fields[[vf$name]]$message <- paste(c(
            toRet$fields[[vf$name]]$message,
            sprintf(
               'Unexpected "%s"',
               paste(class(d[1]), collapse=", ")
            )
         ), collapse=" ")
      }
      if("not nullable" %in% checks){
         mis <- sum(is.na(d))
         if(mis!=0){
            toRet$fields[[vf$name]]$message <- paste(c(
               toRet$fields[[vf$name]]$message,
               sprintf(
                  'Missing values %s/%s = %s%s',
                  mis, length(d), round(mis*100/length(d)), "%"
               )
            ), collapse=" ")
            if(!vf$nullable){
               toRet$fields[[vf$name]]$success <- FALSE
               toRet$success <- FALSE
            }
         }
      }
      if("unique" %in% checks && vf$unique && any(duplicated(d))){
         toRet$fields[[vf$name]]$success <- FALSE
         toRet$success <- FALSE
         toRet$fields[[vf$name]]$message <- paste(c(
            toRet$fields[[vf$name]]$message,
            "Some values are duplicated"
         ), collapse=" ")
      }
      return(toRet)
   }

   if(!inherits(d, "data.frame")){
      stop(x$tableName, " is not a data.frame")
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
   for(fn in availableFields){
      ft <- x$fields$type[which(x$fields$name==fn)]
      ft <- ifelse(ft %in% c("row", "column"), "character", ft)
      fe <- x$fields$nullable[which(x$fields$name==fn)]
      fu <- x$fields$unique[which(x$fields$name==fn)]
      toRet$fields[[fn]] <- list(success=TRUE, message=NULL)
      if(!inherits(dplyr::pull(d, !!fn), ft)){
         if(ft!="base64" || !inherits(dplyr::pull(d, !!fn), "character")){
            toRet$fields[[fn]]$success <- FALSE
            toRet$success <- FALSE
            toRet$fields[[fn]]$message <- paste(c(
               toRet$fields[[fn]]$message,
               sprintf(
                  'Unexpected "%s"',
                  paste(class(dplyr::pull(d, !!fn)), collapse=", ")
               )
            ), collapse=" ")
         }
      }
      if("not nullable" %in% checks){
         mis <- sum(is.na(dplyr::pull(d, !!fn)))
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
      toTest <- dplyr::pull(d, !!fn)
      toTest <- toTest[which(!is.na(toTest))]
      if("unique" %in% checks && fu && any(duplicated(toTest))){
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
         if(any(idx$fields %in% missingFields)){
            toRet$indexes[[i]]$success <- FALSE
            toRet$indexes[[i]]$message <- paste(c(
               toRet$indexes[[i]]$message,
               "Missing field"
            ), collapse=" ")
            toRet$success <- FALSE
         }else{
            if(!idx$unique){
               toRet$indexes[[i]]$success <- TRUE
            }else{
               toTest <- dplyr::filter(
                  d[,idx$fields],
                  apply(d[,idx$fields], 1, function(x) all(!is.na(x)))
               )
               if(any(duplicated(toTest))){
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
   }
   if("not nullable" %in% checks && is.MatrixModel(x)){
      if(!"indexes" %in% names(toRet)){
         toRet$indexes <- list()
      }
      i <- length(toRet$indexes)+1
      rf <- x$fields %>%
         dplyr::filter(.data$type=="row") %>%
         dplyr::pull("name")
      cf <- x$fields %>%
         dplyr::filter(.data$type=="column") %>%
         dplyr::pull("name")
      fe <- x$fields %>%
         dplyr::filter(!.data$type %in% c("row", "column")) %>%
         dplyr::pull("nullable")
      ncells <- length(unique(d[,rf])) * length(unique(d[,cf]))
      mis <- ncells - nrow(d)
      if(mis > 0 ){
         toRet$indexes[[i]]$message <- paste(c(
            toRet$indexes[[i]]$message,
            sprintf(
               'Missing cells %s/%s = %s%s',
               mis, ncells, round(mis*100/ncells), "%"
            )
         ), collapse=" ")
         if(!fe){
            toRet$indexes[[i]]$success <- FALSE
            toRet$success <- FALSE
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
      x$fields %>% dplyr::arrange(.data$name),
      y$fields %>% dplyr::arrange(.data$name)
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
            z$key <- z$key %>% dplyr::arrange(.data$from, .data$to)
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
            z$key <- z$key %>% dplyr::arrange(.data$from, .data$to)
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
         kt <- k$key %>% dplyr::arrange(.data$from, .data$to)
         dplyr::tibble(
            to=to,
            ff=list(kt$from), tf=list(kt$to)
         ) %>%
            dplyr::bind_cols(dplyr::as_tibble(t(k$cardinality))) %>%
            return()
      }
   ))
   toRet %>% dplyr::mutate(
      from=tn
   ) %>%
      dplyr::select(
         "from", "ff", "to", "tf", "fmin", "fmax", "tmin", "tmax"
      ) %>%
      return()
}
