###############################################################################@
#' Create a RelDataModel object
#'
#' @param l the list of table models ([RelTableModel] objects)
#' @param checkFK a logical indicating if foreign keys should be checked
#' (default: TRUE)
#'
#' @return A RelDataModel object.
#'
#' @export
#'
RelDataModel <- function(l, checkFK=TRUE){

   ## Checks ----
   stopifnot(
      all(unlist(lapply(l, is.RelTableModel)))
   )

   ## Creating the object ----
   toRet <- l
   names(toRet) <- NULL
   tn <- unlist(lapply(
      toRet,
      # function(x) paste(x$dbName, x$tableName, sep=".")
      function(x) x$tableName
   ))
   dtn <- unique(tn[which(duplicated(tn))])
   if(length(dtn) > 0){
      stop(paste(
         "Duplicated tables:",
         paste(dtn, collapse=", ")
      ))
   }
   if(any(tn %in% PCKRESERVED)){
      stop(sprintf("%s are reserved words", paste(PCKRESERVED, collapse=", ")))
   }
   names(toRet) <- tn

   ## Correct constraints ----
   for(ft in names(toRet)){
      x <- toRet[[ft]]
      if(length(x$foreignKey)>0) for(i in 1:length(x$foreignKey)){
         fk <- x$foreignKey[[i]]
         if(
            fk$cardinality["fmin"]>0 && nrow(fk$key)==1 &&
            fk$refTable %in% names(fk)
         ){
            toRet[[fk$refTable]]$fields[
               which(toRet[[fk$refTable]]$fields$name==fk$key$from),
               "nullable"
               ] <- FALSE
         }
         if(fk$cardinality["tmin"]>0 && nrow(fk$key)==1){
            toRet[[ft]]$fields[
               which(toRet[[ft]]$fields$name==fk$key$from),
               "nullable"
               ] <- FALSE
         }

         if(fk$cardinality["fmax"]==1){
            ei <- lapply(
               toRet[[ft]]$indexes,
               function(y){
                  identical(sort(y$fields), sort(fk$key$from))
               }
            ) %>%
               unlist() %>%
               as.logical() %>%
               which()
            if(length(ei)>1) stop("Check this part of code")
            if(length(ei)==0){
               toRet[[ft]]$indexes <- unique(c(
                  toRet[[ft]]$indexes,
                  list(
                     list(
                        fields=sort(fk$key$from),
                        unique=TRUE
                     )
                  )
               ))
            }else{
               toRet[[ft]]$indexes[[ei]]$unique <- TRUE
            }
         }

         if(fk$cardinality["tmax"]==1 && fk$refTable %in% names(toRet)){
            ei <- lapply(
               toRet[[fk$refTable]]$indexes,
               function(y){
                  identical(sort(y$fields), sort(fk$key$to))
               }
            ) %>%
               unlist() %>%
               as.logical() %>%
               which()
            if(length(ei)>1) stop("Check this part of code")
            if(length(ei)==0){
               toRet[[fk$refTable]]$indexes <- unique(c(
                  toRet[[fk$refTable]]$indexes,
                  list(
                     list(
                        fields=sort(fk$key$to),
                        unique=TRUE
                     )
                  )
               ))
            }else{
               toRet[[fk$refTable]]$indexes[[ei]]$unique <- TRUE
            }
         }
      }
   }
   toRet <- lapply(toRet, correct_constraints)

   ## Return the results ----
   class(toRet) <- c("RelDataModel", class(toRet))
   if(checkFK){
      check_foreign_keys(toRet)
   }
   return(toRet)
}

###############################################################################@
#' Check if the object is  a [RelDataModel] object
#'
#' @param x any object
#'
#' @return A single logical: TRUE if x is a [RelDataModel] object
#'
#' @export
#'
is.RelDataModel <- function(x){
   inherits(x, "RelDataModel")
}

###############################################################################@
#' Check the availability of foreign keys
#'
#' @param x a [RelDataModel] object
#'
#' @return Nothing. The function throws an error if there is an issue with
#' foreign keys.
#'
#' @export
#'
check_foreign_keys <- function(x){
   stopifnot(is.RelDataModel(x))
   lapply(
      x,
      function(tm){
         fks <- tm$foreignKeys
         if(is.null(fks)){
            return()
         }
         lapply(
            fks,
            function(fk){
               ft <- which(unlist(lapply(
                  x,
                  function(y){
                     y$tableName==fk$refTable
                  }
               )))
               if(length(ft)!=1){
                  stop(sprintf(
                     "Cannot find the foreign table: %s",
                     fk$refTable
                  ))
               }
               ft <- x[[ft]]
               for(i in 1:nrow(fk$key)){
                  from <- fk$key[i,]$from
                  to <- fk$key[i,]$to
                  if(!to %in% ft$fields$name){
                     stop(sprintf(
                        "Cannot find the %s field in the %s table",
                        to,
                        fk$refTable
                     ))
                  }
                  fromt <- tm$fields$type[which(tm$fields$name==from)]
                  fromt <- ifelse(
                     fromt %in% c("row", "column"), "character", fromt
                  )
                  tot <- ft$fields$type[which(ft$fields$name==to)]
                  tot <- ifelse(
                     tot %in% c("row", "column"), "character", tot
                  )
                  if(fromt != tot){
                     stop(sprintf(
                        paste(
                           "The %s field in the %s table and",
                           "the %s field in the %s table",
                           "are not of the same type"
                        ),
                        from, tm$tableName, to, ft$tableName
                     ))
                  }
               }
            }
         )
      }
   )
   invisible(NULL)
}

###############################################################################@
#' @export
#'
length.RelDataModel <- function(x){
   length(unclass(x))
}

###############################################################################@
#' Subset a [RelDataModel]
#'
#' @param x the [RelDataModel] objcet
#' @param i the index or the names of the elements to extract
#' @param rmForeignKeys if TRUE, remove foreign keys which are not
#' available after extraction. If FALSE (default) the function will throw an
#' error if any foreign keys does not exist in the extracted RelDataModel.
#' @param ... additional arguments for the \code{\link{RelDataModel}} function.
#'
#' @export
#'
'[.RelDataModel' <- function(x, i, rmForeignKeys=FALSE, ...){
   l <- unclass(x)
   l <- l[i]
   if(rmForeignKeys){
      l <- lapply(
         l,
         function(tm){
            if(length(tm$foreignKeys)>0){
               tm$foreignKeys <- tm$foreignKeys[which(unlist(lapply(
                  tm$foreignKeys,
                  function(fk){
                     fk$refTable %in% names(l)
                  }
               )))]
            }
            return(tm)
         }
      )
      return(RelDataModel(l=l, ...))
   }else{
      toRet <- try(RelDataModel(l=l, ...), silent=TRUE)
      if(inherits(toRet, "try-error")){
         if(length(grep("Cannot find the foreign table", toRet))>0){
            toRet <- paste0(
               toRet,
               "Set rmForeignKeys to TRUE to remove unavailable foreign keys\n"
            )
         }
         stop(toRet)
      }else{
         return(toRet)
      }
   }
}

###############################################################################@
#' @export
#'
'[<-.RelDataModel' <- function(x, i, value){
   stop("'[<-' is not supported for RelDataModel: use 'c' instead")
}

###############################################################################@
#' @export
#'
'[[<-.RelDataModel' <- function(x, i, value){
   stop("'[[<-' is not supported for RelDataModel: use 'c' instead")
}

###############################################################################@
#' @export
#'
names.RelDataModel <- function(x){
   names(unclass(x))
}

###############################################################################@
#' @export
#'
'names<-.RelDataModel' <- function(x, value){
   stopifnot(
      is.character(value),
      length(value)==length(x),
      sum(duplicated(value))==0
   )
   ovalues <- names(x)
   l <- unclass(x)
   for(i in 1:length(l)){
      l[[i]]$tableName <- value[i]
      fk <- l[[i]]$foreignKeys
      if(length(fk) > 0) for(j in 1:length(fk)){
         nn <- value[which(
            ovalues==fk[[j]]$refTable
         )]
         fk[[j]]$refTable <- nn
      }
      l[[i]]$foreignKeys <- fk
   }
   return(RelDataModel(l=l))
}


###############################################################################@
#' Merge [RelDataModel] objects
#'
#' @param ... [RelDataModel] objects
#' @param checkFK a logical indicating if foreign keys should be checked
#' (default: TRUE)
#'
#' @return A [RelDataModel] objects
#'
#' @export
#'
c.RelDataModel <- function(..., checkFK=TRUE){
   inputs <- list(...)
   stopifnot(
      all(unlist(lapply(inputs, is.RelDataModel)))
   )
   il <- lapply(
      inputs,
      function(x){
         return(unclass(x))
      }
   )
   l <- do.call(c, il)
   return(RelDataModel(
      l=l,
      checkFK=checkFK
   ))
}

###############################################################################@
#' Convert a [RelDataModel] object in a list of 5 normalized tibbles
#'
#' @param rdm a [RelDataModel] object
#'
#' @return A list with the following tibbles:
#'
#' - **tables**: The tables in the model with the following information
#'    + **name**: the name of the table
#'    + **x**: the x coordinate of the table in the model drawing
#'    (NA ==> position undefined)
#'    + **y**: the y coordinate of the table in the model drawing
#'    (NA ==> position undefined)
#'    + **color**: the color of the table in the model drawing
#'    (NA ==> undefined)
#'    + **comment**: comment about the table
#'
#' - **fields**: The fields in the model with the following information
#'    + **name**: the name of the field
#'    + **type**: the type of the field
#'    + **nullable**: a logical indicating if the field can be null
#'    + **comment**: comment about the field
#'    + **table**: the name of the table to which the field belongs
#'
#' - **primaryKeys**: The primary keys in the model with the
#' following information
#'    + **table**: the name of the relevant table
#'    + **field**: the name of the field participating to the primary key
#'
#' - **foreignKeys**: The foreign keys in the model with the
#' following information
#'    + **table**: the name of the referring table
#'    + **fki**: the identifier of the foreign key (by referring table)
#'    + **field**: the name of the referring field
#'    + **refTable**: the name of the referred table
#'    + **refField**: the name of the referred field
#'
#' - **indexes**: The indexes in the model with the following information
#'    + **table**: the name of the relevant table
#'    + **idx**: the identifier of the index (by table)
#'    + **field**: the name of the field participating to the index
#'    + **unique**: a logical indicating if the field is unique
#'
#' @export
toDBM <- function(rdm){
   stopifnot(is.RelDataModel(rdm))
   toRet <- lapply(
      rdm,
      function(tm){
         ##
         tables <- dplyr::tibble(
            name=tm$tableName,
            x=as.numeric(NA),
            y=as.numeric(NA),
            color=as.character(NA),
            comment=as.character(NA)
         )
         if(!is.null(tm$display)){
            for(n in c("x", "y", "color", "comment")){
               tables[,n] <- tm$display[[n]]
            }
         }
         ##
         fields <- tm$fields %>%
            dplyr::mutate(
               table=tm$tableName,
               fieldOrder=1:nrow(tm$fields)
            )
         ##
         if(length(tm$primaryKey)==0){
            primaryKeys <- dplyr::tibble(
               table=character(),
               field=character()
            )
         }else{
            primaryKeys <- dplyr::tibble(
               table=tm$tableName,
               field=tm$primaryKey
            )
         }
         ##
         if(length(tm$foreignKeys)==0){
            foreignKeys <- dplyr::tibble(
               table=character(),
               fki=integer(),
               field=character(),
               refTable=character(),
               refField=character()
            )
         }else{
            foreignKeys <- do.call(dplyr::bind_rows, lapply(
               1:length(tm$foreignKeys),
               function(fki){
                  fmin <- tm$foreignKeys[[fki]]$cardinality["fmin"]
                  fmax <- tm$foreignKeys[[fki]]$cardinality["fmax"]
                  tmin <- tm$foreignKeys[[fki]]$cardinality["tmin"]
                  tmax <- tm$foreignKeys[[fki]]$cardinality["tmax"]
                  toRet <- tm$foreignKeys[[fki]]$key %>%
                     dplyr::mutate(
                        table=tm$tableName,
                        refTable=tm$foreignKeys[[fki]]$refTable,
                        fki=fki,
                        fmin=fmin,
                        fmax=fmax,
                        tmin=tmin,
                        tmax=tmax
                     )
                  toRet %>%
                     dplyr::rename("field"="from", "refField"="to") %>%
                     dplyr::select(
                        "table", "fki", "field", "refTable", "refField",
                        "fmin", "fmax", "tmin", "tmax"
                     )
               }
            ))
         }
         ##
         if(length(tm$indexes)==0){
            indexes <- dplyr::tibble(
               table=character(),
               idx=integer(),
               field=character(),
               unique=logical()
            )
         }else{
            indexes <- do.call(dplyr::bind_rows, lapply(
               1:length(tm$indexes),
               function(idx){
                  if(length(idx)==0){
                     return(NULL)
                  }
                  toRet <- dplyr::tibble(
                     table=tm$tableName,
                     idx=idx,
                     unique=tm$indexes[[idx]]$unique,
                     field=tm$indexes[[idx]]$fields
                  ) %>% dplyr::select("table", "idx", "field", "unique")
                  return(toRet)
               }
            ))
         }
         ##
         return(list(
            tables=tables,
            fields=fields,
            primaryKeys=primaryKeys,
            foreignKeys=foreignKeys,
            indexes=indexes
         ))
      }
   )
   return(list(
      tables=do.call(dplyr::bind_rows, lapply(toRet, function(x) x$tables)),
      fields=do.call(dplyr::bind_rows, lapply(toRet, function(x) x$fields)),
      primaryKeys=do.call(
         dplyr::bind_rows,
         lapply(toRet, function(x) x$primaryKeys)
      ),
      foreignKeys=do.call(
         dplyr::bind_rows,
         lapply(toRet, function(x) x$foreignKeys)
      ),
      indexes=do.call(dplyr::bind_rows, lapply(toRet, function(x) x$indexes))
   ))
}

###############################################################################@
#' Convert a list of 5 normalized tibbles in a [RelDataModel] object
#'
#' @param dbm a list with the following tibbles:
#'
#' - **tables**: The tables in the model with the following information
#'    + **name**: the name of the table
#'    + **x**: the x coordinate of the table in the model drawing
#'    (NA ==> position undefined)
#'    + **y**: the y coordinate of the table in the model drawing
#'    (NA ==> position undefined)
#'    + **color**: the color of the table in the model drawing
#'    (NA ==> undefined)
#'    + **comment**: comment about the table
#'
#' - **fields**: The fields in the model with the following information
#'    + **name**: the name of the field
#'    + **type**: the type of the field
#'    + **nullable**: a logical indicating if the field can be null
#'    + **comment**: comment about the field
#'    + **table**: the name of the table to which the field belongs
#'
#' - **primaryKeys**: The primary keys in the model with the
#' following information
#'    + **table**: the name of the relevant table
#'    + **field**: the name of the field participating to the primary key
#'
#' - **foreignKeys**: The foreign keys in the model with the
#' following information
#'    + **table**: the name of the referring table
#'    + **fki**: the identifier of the foreign key (by referring table)
#'    + **field**: the name of the referring field
#'    + **refTable**: the name of the referred table
#'    + **refField**: the name of the referred field
#'
#' - **indexes**: The indexes in the model with the following information
#'    + **table**: the name of the relevant table
#'    + **idx**: the identifier of the index (by table)
#'    + **field**: the name of the field participating to the index
#'    + **unique**: a logical indicating if the field is unique
#'
#' @return A [RelDataModel] object
#'
#' @export
#'
fromDBM <- function(dbm){

   tableName <- split(dbm$tables$name, dbm$tables$name)

   dbm$fields <- dplyr::arrange(dbm$fields, .data$fieldOrder)
   fields <- split(
      dplyr::select(dbm$fields, -"table", -"fieldOrder"), dbm$fields$table
   )

   primaryKey <- split(dbm$primaryKeys$field, dbm$primaryKeys$table)

   foreignKeys <- split(
      dplyr::select(dbm$foreignKeys, -"table"), dbm$foreignKeys$table
   ) %>%
      lapply(function(tfk){
         split(dplyr::select(tfk, -"fki"), tfk$fki) %>%
            lapply(function(fk){
               list(
                  refTable=fk$refTable[1],
                  key=dplyr::select(fk, "from"="field", "to"="refField"),
                  cardinality=c(
                     fk$fmin[1], fk$fmax[1],
                     fk$tmin[1], fk$tmax[1]
                  ) %>%
                     magrittr::set_names(c("fmin", "fmax", "tmin", "tmax"))
               )
            }) %>%
            magrittr::set_names(NULL)
      })

   indexes <- split(dplyr::select(dbm$indexes, -"table"), dbm$indexes$table) %>%
      lapply(function(ti){
         split(dplyr::select(ti, -"idx"), ti$idx) %>%
            lapply(function(idx){
               return(list(fields=idx$field, unique=idx$unique[1]))
            }) %>%
            magrittr::set_names(NULL)
      })

   display <- split(dplyr::select(dbm$tables, -"name"), dbm$tables$name) %>%
      lapply(function(di){
         di %>%
            magrittr::set_class("list") %>%
            `attr<-`("row.names", NULL)
      })

   toRet <- lapply(
      tableName,
      function(tn){
         return(RelTableModel(
            tableName=tn,
            fields=fields[[tn]],
            primaryKey=primaryKey[[tn]],
            foreignKeys=foreignKeys[[tn]],
            indexes=indexes[[tn]],
            display=display[[tn]]
         ))
      }
   ) %>% RelDataModel()

   return(toRet)

}

###############################################################################@
#' Rename a table in a [RelDataModel]
#'
#' @param x a [RelDataModel] object
#' @param old a single character corresponding to the table name to change
#' @param new the new table name
#'
#' @return A [RelDataModel]
#'
#' @export
#'
rename_table <- function(x, old, new){
   stopifnot(is.RelDataModel(x))
   stopifnot(
      is.RelDataModel(x),
      length(old)==1,
      length(new)==1,
      old %in% names(x)
   )
   newNames <- names(x)
   newNames[which(newNames==old)] <- new
   names(x) <- newNames
   return(x)
}

###############################################################################@
#' Add a table to a [RelDataModel]
#'
#' @param x a [RelDataModel]
#' @param newTable the name of the new table or a [RelTableModel]
#'
#' @return A [RelDataModel]
#'
#' @export
#'
#'
add_table <- function(x, newTable){
   stopifnot(is.RelDataModel(x))
   stopifnot(is.RelTableModel(newTable) || is.character(newTable))
   if(is.character(newTable)){
      stopifnot(
         length(newTable)==1
      )
      newTable <- RelTableModel(
         tableName=newTable,
         fields=dplyr::tibble(
            name=character(),
            type=character(),
            nullable=logical(),
            unique=logical(),
            comment=character()
         ),
         primaryKey=NULL,
         foreignKeys=NULL,
         indexes=NULL,
         display=list(
            x=as.numeric(NA), y=as.numeric(NA),
            color=as.character(NA),
            comment=as.character(NA)
         )
      )
   }
   stopifnot(!newTable$tableName %in% names(x))
   x <- unclass(x)
   x[[newTable$tableName]] <- newTable
   return(RelDataModel(x))
}

###############################################################################@
#' Remove a table from a [RelDataModel]
#'
#' @param x a [RelDataModel]
#' @param tableName the name of the table to remove
#' @param rmForeignKeys if TRUE, remove foreign keys which are not
#' available after extraction. If FALSE (default) the function will throw an
#' error if any foreign keys does not exist in the extracted RelDataModel.
#'
#' @return A [RelDataModel]
#'
#' @export
#'
remove_table <- function(x, tableName, rmForeignKeys=FALSE){
   stopifnot(is.RelDataModel(x))
   stopifnot(
      is.character(tableName),
      length(tableName)==1,
      tableName %in% names(x)
   )
   return(x[-which(names(x)==tableName), rmForeignKeys=rmForeignKeys])
}

###############################################################################@
#' Add a field to a table in a [RelDataModel]
#'
#' @param x a [RelDataModel]
#' @param tableName the name of the table to modify (a single character)
#' @param name the name of the field to add (a single character)
#' @param type the type of the field (a single character)
#' @param nullable if the field is nullable (a single logical)
#' @param unique if the values are unique (a single logical)
#' @param comment a description (a single character)
#'
#' @return A [RelDataModel]
#'
#' @export
#'
add_field <- function(
   x, tableName,
   name, type, nullable, unique, comment
){
   stopifnot(is.RelDataModel(x))
   stopifnot(
      is.character(tableName), length(tableName)==1,
      !is.MatrixModel(x[[tableName]]),
      !is.na(tableName),
      tableName %in% names(x),
      is.character(name), length(name)==1,
      !is.na(name),
      !name %in% x[[tableName]]$fields$name,
      is.character(type), length(type)==1,
      !is.na(type), type %in% c(SUPPTYPES, "row", "column"),
      is.logical(nullable), length(nullable)==1,
      !is.na(nullable),
      is.logical(unique), length(unique)==1,
      !is.na(unique),
      is.character(comment), length(comment)==1
   )
   x <- unclass(x)
   if(type %in% c("row", "column")){
      stopifnot(
         length(x[[tableName]])==0 #, !nullable, unique
      )
      x[[tableName]]$fields <- dplyr::tibble(
         name=c(
            name,
            paste0(
               toupper(setdiff(c("row", "column"), type)),
               "_TO_UPDATE"
            ),
            "VALUE_TO_UPDATE"
         ),
         type=c(type, setdiff(c("row", "column"), type), "character"),
         nullable=c(FALSE, FALSE, TRUE),
         unique=c(FALSE, FALSE, FALSE),
         comment=c(comment, NA, NA)
      )
      x[[tableName]]$primaryKey <- x[[tableName]]$fields$name[1:2]
   }else{
      x[[tableName]]$fields <- dplyr::bind_rows(
         x[[tableName]]$fields,
         dplyr::tibble(
            name=name,
            type=type,
            nullable=nullable,
            unique=unique,
            comment=comment
         )
      )
   }
   return(RelDataModel(x))
}

###############################################################################@
#' Rename an existing field in a [RelDataModel] table
#'
#' @param x a [RelDataModel]
#' @param tableName the name of the table to modify (a single character)
#' @param current the current name of the field to modify (a single character)
#' @param new the new name of the field (a single character)
#'
#' @return A [RelDataModel]
#'
#' @export
#'
rename_field <- function(x, tableName, current, new){
   stopifnot(is.RelDataModel(x))
   stopifnot(
      is.character(tableName), length(tableName)==1,
      tableName %in% names(x),
      is.character(current), length(current)==1,
      current %in% x[[tableName]]$fields$name,
      current!=new,
      is.character(new), length(new)==1,
      !new %in% x[[tableName]]$fields$name
   )
   x <- unclass(x)
   tm <- x[[tableName]]
   ## Renaming the field ----
   tm$fields[
      which(tm$fields$name==current),
      "name"
   ] <- new
   ## Adapting primary key ----
   tm$primaryKey[which(tm$primaryKey==current)] <- new
   tm$primaryKey <- sort(tm$primaryKey)
   ## Adapting indexes ----
   if(length(tm$indexes)>0) for(i in 1:length(tm$indexes)){
      tm$indexes[[i]]$fields[
         which(tm$indexes[[i]]$fields==current)
      ] <- new
      tm$indexes[[i]]$fields <- sort(tm$indexes[[i]]$fields)
   }
   ## Adapting foreign keys ----
   if(length(tm$foreignKeys)>0) for(i in 1:length(tm$foreignKeys)){
      tm$foreignKeys[[i]]$key[
         which(tm$foreignKeys[[i]]$key$from==current),
         "from"
      ] <- new
   }
   x[[tableName]] <- tm
   ## Changing foreign Keys in other tables ----
   for(i in 1:length(x)){
      tm <- x[[i]]
      keys <- tm$foreignKeys
      if(length(keys)>0) for(j in 1:length(keys)){
         if(keys[[j]]$refTable==tableName){
            keys[[j]]$key[which(keys[[j]]$key$to==current),"to"] <- new
         }
      }
      tm$foreignKeys <- keys
      x[[i]] <- tm
   }
   return(RelDataModel(x))
}

###############################################################################@
fk_match <- function(
   x, fromTable, fromFields, toTable, toFields
){
   tocheck <- paste(fromFields, toFields, sep="-->") %>% sort()
   tfk <- x[[fromTable]]$foreignKeys
   lapply(
      tfk,
      function(y){
         y$refTable==toTable &&
         nrow(y$key)==length(tocheck) &&
         all(
            sort(paste(y$key$from, y$key$to, sep="-->")) == tocheck
         )
      }
   ) %>%
      unlist() %>%
      as.logical() %>%
      which()
}

###############################################################################@
#' Add a foreign key between two tables
#'
#' @param x a [RelDataModel]
#' @param fromTable the name of the referencing table
#' @param fromFields the name of the referencing fields
#' @param toTable the name of the referenced table
#' @param toFields the names of the referenced fields
#' @param fmin from minimum cardinality (default: 0L)
#' @param fmax from maximum cardinality (default: -1L ==> Infinite)
#' @param tmin to minimum cardinality (default: 1L)
#' @param tmax to maximum cardinality (default: 1L)
#'
#' @return A [RelDataModel]
#'
#' @export
#'
add_foreign_key <- function(
   x, fromTable, fromFields, toTable, toFields,
   fmin=0L, fmax=-1L, tmin=1L, tmax=1L
){
   stopifnot(is.RelDataModel(x))
   ft <- x[[fromTable]]$fields$type[
      match(fromFields, x[[fromTable]]$fields$name)
   ]
   ft <- ifelse(ft %in% c("row", "column"), "character", ft)
   tt <- x[[toTable]]$fields$type[
      match(toFields, x[[toTable]]$fields$name)
   ]
   tt <- ifelse(tt %in% c("row", "column"), "character", tt)
   stopifnot(
      is.character(fromTable), length(fromTable)==1,
      fromTable %in% names(x),
      is.character(toTable), length(toTable)==1,
      toTable %in% names(x),
      is.character(fromFields), length(fromFields)>=1,
      is.character(toFields), length(toFields)==length(fromFields),
      all(fromFields %in% x[[fromTable]]$fields$name),
      all(toFields %in% x[[toTable]]$fields$name),
      all(ft == tt),
      all(ft!="base64"),
      all(!is.na(c(fmin, fmax, tmin, tmax))),
      is.integer(fmin), is.integer(fmax), is.integer(tmin), is.integer(tmax),
      fmin > -1, tmin > -1,
      fmax==-1 || (fmax > 0 && fmax >= fmin),
      tmax==-1 || (tmax > 0 && tmax >= tmin)
   )
   if(is.MatrixModel(x[[fromTable]])){
      fi <- x[[fromTable]]$fields %>% dplyr::filter(.data$name %in% fromFields)
      if(any(!fi$type %in% c("row", "column"))){
         stop(paste(
            "Only row and column of matrices can be used in foreign keys"
         ))
      }
      if(fmax==1 && nrow(fi)==1){
         stop("Matix row and column cannot be mapped individually unambiguisly")
      }
   }
   if(is.MatrixModel(x[[toTable]])){
      fi <- x[[toTable]]$fields %>% dplyr::filter(.data$name %in% toFields)
      if(any(!fi$type %in% c("row", "column"))){
         stop(paste(
            "Only row and column of matrices can be used in foreign keys"
         ))
      }
      if(tmax==1 && nrow(fi)==1){
         stop("Matix row and column cannot be mapped individually unambiguisly")
      }
   }
   efk <- fk_match(x, fromTable, fromFields, toTable, toFields)
   if(length(efk)!=0){
      warning("The foreign key already exists ==> no change")
      return(x)
   }
   x <- unclass(x)
   x[[fromTable]]$foreignKeys <- c(
      x[[fromTable]]$foreignKeys,
      list(list(
         refTable=toTable,
         key=dplyr::tibble(
            from=fromFields,
            to=toFields
         ),
         cardinality=c(
            "fmin"=as.integer(fmin),
            "fmax"=as.integer(fmax),
            "tmin"=as.integer(tmin),
            "tmax"=as.integer(tmax)
         )
      ))
   )
   return(RelDataModel(x))
}

###############################################################################@
#' Remove a foreign key between two tables
#'
#' @param x a [RelDataModel]
#' @param fromTable the name of the referencing table
#' @param fromFields the name of the referencing fields
#' @param toTable the name of the referenced table
#' @param toFields the names of the referenced fields
#'
#' @return A [RelDataModel]
#'
#' @export
#'
remove_foreign_key <- function(
   x, fromTable, fromFields, toTable, toFields
){
   stopifnot(is.RelDataModel(x))
   stopifnot(
      is.character(fromTable), length(fromTable)==1,
      fromTable %in% names(x),
      is.character(toTable), length(toTable)==1,
      toTable %in% names(x),
      is.character(fromFields), length(fromFields)>=1,
      is.character(toFields), length(toFields)==length(fromFields),
      all(fromFields %in% x[[fromTable]]$fields$name),
      all(toFields %in% x[[toTable]]$fields$name)
   )
   efk <- fk_match(x, fromTable, fromFields, toTable, toFields)
   if(length(efk)==0){
      warning("The foreign key does not exist ==> no change")
      return(x)
   }
   x <- unclass(x)
   x[[fromTable]]$foreignKeys <- x[[fromTable]]$foreignKeys[-efk]
   return(RelDataModel(x))
}

###############################################################################@
#' Update a the cardinalities of a foreign key between two tables
#'
#' @param x a [RelDataModel]
#' @param fromTable the name of the referencing table
#' @param fromFields the name of the referencing fields
#' @param toTable the name of the referenced table
#' @param toFields the names of the referenced fields
#' @param fmin from minimum cardinality
#' @param fmax from maximum cardinality
#' @param tmin to minimum cardinality
#' @param tmax to maximum cardinality
#'
#' @return A [RelDataModel]
#'
#' @export
#'
update_foreign_key <- function(
   x, fromTable, fromFields, toTable, toFields,
   fmin, fmax, tmin, tmax
){
   stopifnot(is.RelDataModel(x))
   toRet <- x %>%
      remove_foreign_key(
         fromTable, fromFields, toTable, toFields
      ) %>%
      add_foreign_key(
         fromTable, fromFields, toTable, toFields,
         fmin, fmax, tmin, tmax
      )
   return(toRet)
}

###############################################################################@
#' Remove a field from a table in a [RelDataModel]
#'
#' @param x a [RelDataModel]
#' @param tableName the name of the table to modify (a single character)
#' @param fieldName the name of the field to remove (a single character)
#' @param rmForeignKeys a single logical indicating if the corresponding foreign
#' keys should be removed. If FALSE (default), the function will throw an
#' error if it encounter a foreign key using the field.
#'
#' @return A [RelDataModel]
#'
#' @export
#'
remove_field <- function(
   x,
   tableName,
   fieldName,
   rmForeignKeys=FALSE
){
   stopifnot(is.RelDataModel(x))
   stopifnot(
      is.character(tableName), length(tableName)==1,
      !is.MatrixModel(x[[tableName]]),
      tableName %in% names(x),
      is.character(fieldName), length(fieldName)==1,
      fieldName %in% x[[tableName]]$fields$name
   )
   ## Foreign keys ----
   refk <- lapply(
      x[[tableName]]$foreignKeys,
      function(y){
         fieldName %in% y$key$from
      }
   ) %>%
      unlist() %>%
      as.logical() %>%
      which()
   tefk <- lapply(
      x,
      function(y){
         lapply(
            y$foreignKeys,
            function(z){
               tableName==z$refTable &
               fieldName %in% z$key$to
            }
         ) %>%
            unlist() %>%
            as.logical() %>%
            which()
      }
   )
   x <- unclass(x)
   if(length(refk)>0 || any(unlist(lapply(tefk, length))>0)){
      if(!rmForeignKeys){
         stop(
            "This field is used as a foreign key.\n",
            "Set rmForeignKeys to TRUE to remove them"
         )
      }else{
         if(length(refk)>0){
            x[[tableName]]$foreignKeys <- x[[tableName]]$foreignKeys[-refk]
         }
         for(tn in names(tefk)){
            if(length(tefk[[tn]]>0)){
               x[[tn]]$foreignKeys <- x[[tn]]$foreignKeys[-tefk[[tn]]]
            }
         }
      }
   }
   ## Adapting primary key ----
   if(fieldName %in% x[[tableName]]$primaryKey){
      x[[tableName]]$primaryKey <- character()
   }
   ## Adapting indexes ----
   if(length(x[[tableName]]$indexes)>0){
      toRm <- c()
      for(i in 1:length(x[[tableName]]$indexes)){
         if(fieldName %in% x[[tableName]]$indexes[[i]]$fields){
            toRm <- c(toRm, i)
         }
      }
      if(length(toRm)>0){
         x[[tableName]]$indexes <- x[[tableName]]$indexes[-toRm]
      }
   }
   ## Removing the field ----
   x[[tableName]]$fields <- x[[tableName]]$fields %>%
      dplyr::filter(.data$name!=fieldName)
   ## Returning the results ----
   return(RelDataModel(x))

}

###############################################################################@
#' Order fields in a table in a [RelDataModel]
#'
#' @param x a [RelDataModel]
#' @param tableName the name of the table to modify (a single character)
#' @param order a vector of integers all in (1:number_of_fields)
#'
#' @return A [RelDataModel]
#'
#' @export
#'
order_fields <- function(
   x,
   tableName,
   order
){
   stopifnot(is.RelDataModel(x))
   stopifnot(
      is.character(tableName), length(tableName)==1,
      tableName %in% names(x),
      is.numeric(order),
      length(order)==nrow(x[[tableName]]$fields),
      all(order %in% 1:nrow(x[[tableName]]$fields))
   )
   x <- unclass(x)
   x[[tableName]]$fields <- x[[tableName]]$fields[order,]
   return(RelDataModel(x))
}

###############################################################################@
#' Set the primary key a table in a [RelDataModel]
#'
#' @param x a [RelDataModel]
#' @param tableName the name of the table to modify (a single character)
#' @param fieldNames the names of the fields to include in the primary key
#'
#' @return A [RelDataModel]
#'
#' @export
#'
set_primary_key <- function(x, tableName, fieldNames){
   stopifnot(is.RelDataModel(x))
   fieldNames <- as.character(fieldNames)
   stopifnot(
      is.character(tableName), length(tableName)==1,
      !is.MatrixModel(x[[tableName]]),
      tableName %in% names(x),
      all(fieldNames %in% x[[tableName]]$fields$name)
   )
   x <- unclass(x)
   x[[tableName]]$primaryKey <- sort(fieldNames)
   return(RelDataModel(x))
}


###############################################################################@
#' Add an index to a table in a [RelDataModel]
#'
#' @param x a [RelDataModel]
#' @param tableName the name of the table to modify (a single character)
#' @param fieldNames the names of the fields to include in the index
#' @param unique a logical indicating if the indexed values are unique
#'
#' @return A [RelDataModel]
#'
#' @export
#'
add_index <- function(x, tableName, fieldNames, unique){
   stopifnot(is.RelDataModel(x))
   fieldNames <- as.character(fieldNames) %>% unique()
   stopifnot(
      is.character(tableName), length(tableName)==1,
      tableName %in% names(x),
      all(fieldNames %in% x[[tableName]]$fields$name),
      is.logical(unique), length(unique)==1
   )
   ei <- lapply(
      x[[tableName]]$indexes,
      function(y){
         identical(sort(y$fields), sort(fieldNames))
      }
   ) %>%
      unlist() %>%
      as.logical() %>%
      any()
   if(ei){
      warning("The index already exists ==> no change")
      return(x)
   }
   x <- unclass(x)
   x[[tableName]]$indexes <- c(
      x[[tableName]]$indexes,
      list(list(fields=sort(fieldNames), unique=unique))
   )
   return(RelDataModel(x))
}

###############################################################################@
#' Remove an index from a table in a [RelDataModel]
#'
#' @param x a [RelDataModel]
#' @param tableName the name of the table to modify (a single character)
#' @param fieldNames the names of the fields composing the index
#'
#' @return A [RelDataModel]
#'
#' @export
#'
remove_index <- function(x, tableName, fieldNames){
   stopifnot(is.RelDataModel(x))
   fieldNames <- as.character(fieldNames)
   stopifnot(
      is.character(tableName), length(tableName)==1,
      tableName %in% names(x),
      all(fieldNames %in% x[[tableName]]$fields$name)
   )
   ei <- lapply(
      x[[tableName]]$indexes,
      function(y){
         identical(sort(y$fields), sort(fieldNames))
      }
   ) %>%
      unlist() %>%
      as.logical() %>%
      which()
   if(length(ei)==0){
      warning("The index does not exists ==> no change")
      return(x)
   }
   x <- unclass(x)
   x[[tableName]]$indexes <- x[[tableName]]$indexes[-ei]
   return(RelDataModel(x))
}

###############################################################################@
#' Set table index uniqueness in a [RelDataModel]
#'
#' @param x a [RelDataModel]
#' @param tableName the name of the table to modify (a single character)
#' @param fieldNames the names of the fields composing the index
#' @param unique a logical value
#'
#' @return A [RelDataModel]
#'
#' @export
#'
set_unique_index <- function(x, tableName, fieldNames, unique){
   stopifnot(is.RelDataModel(x))
   fieldNames <- as.character(fieldNames)
   stopifnot(
      is.character(tableName), length(tableName)==1,
      tableName %in% names(x),
      all(fieldNames %in% x[[tableName]]$fields$name),
      is.logical(unique), length(unique)==1, !is.na(unique)
   )
   ei <- lapply(
      x[[tableName]]$indexes,
      function(y){
         identical(sort(y$fields), sort(fieldNames))
      }
   ) %>%
      unlist() %>%
      as.logical() %>%
      which()
   if(length(ei)==0){
      warning("The index does not exists ==> no change")
      return(x)
   }
   x <- unclass(x)
   x[[tableName]]$indexes[[ei]]$unique <- unique
   return(RelDataModel(x))
}

###############################################################################@
#' Update field information in a table of a [RelDataModel]
#'
#' @param x a [RelDataModel]
#' @param tableName the name of the table to modify (a single character)
#' @param fieldName the name of the field to modify (a single character)
#' @param type the type of the field (a single character)
#' @param nullable if the field is nullable (a single logical)
#' @param unique if the values are unique (a single logical)
#' @param comment a description (a single character)
#'
#' @return A [RelDataModel]
#'
#' @export
#'
update_field <- function(
   x, tableName, fieldName, type=NULL, nullable=NULL, unique=NULL, comment=NULL
){
   stopifnot(is.RelDataModel(x))
   stopifnot(
      is.character(tableName), length(tableName)==1,
      tableName %in% names(x),
      is.character(fieldName), length(fieldName)==1,
      fieldName %in% x[[tableName]]$fields$name
   )
   curType <- x[[tableName]]$fields %>%
      dplyr::filter(.data$name==fieldName) %>%
      dplyr::pull("type")
   type <- ifelse(
      is.null(type),
      curType,
      type
   )
   nullable <- ifelse(
      is.null(nullable),
      x[[tableName]]$fields %>%
         dplyr::filter(.data$name==fieldName) %>%
         dplyr::pull("nullable"),
      nullable
   )
   unique <- ifelse(
      is.null(unique),
      x[[tableName]]$fields %>%
         dplyr::filter(.data$name==fieldName) %>%
         dplyr::pull("unique"),
      unique
   )
   comment <- ifelse(
      is.null(comment),
      x[[tableName]]$fields %>%
         dplyr::filter(.data$name==fieldName) %>%
         dplyr::pull("comment"),
      comment
   )
   type <- as.character(type)
   nullable <- as.logical(nullable)
   unique <- as.logical(unique)
   comment <- as.character(comment)
   stopifnot(
      is.character(type), length(type)==1, !is.na(type),
      is.logical(nullable), length(nullable)==1, !is.na(nullable),
      is.logical(unique), length(unique)==1, !is.na(unique),
      is.character(comment), length(comment)==1
   )

   x <- unclass(x)
   if(is.MatrixModel(x[[tableName]]) && curType %in% c("row", "column")){
      ## row/column type ----
      if(!type %in% c("row", "column")){
         stop("row and column can only be mutated in row and column")
      }
      if(nullable || unique){
         stop("row and column cannot be nullable nor unique")
      }
      ## Updating field information ----
      d2CurType <- setdiff(c("row", "column"), curType)
      d2Type <- setdiff(c("row", "column"), type)
      d2fn <- x[[tableName]]$fields %>%
         dplyr::filter(.data$type==!!d2CurType) %>%
         dplyr::pull("name")
      x[[tableName]]$fields <- x[[tableName]]$fields %>%
         dplyr::mutate(
            type=ifelse(
               .data$name==fieldName, !!type,
               ifelse(.data$name==d2fn, !!d2Type, .data$type)
            ),
            comment=ifelse(.data$name==fieldName, !!comment, .data$comment)
         )
   }else{
      ## Other type ----
      if(type %in% c("row", "column")){
         stop("Cannot set this field as row or column")
      }
      if(is.MatrixModel(x[[tableName]]) && type=="base64"){
         stop("A matrix cannot store base64 documents")
      }
      check_types(type)
      ## Foreign keys ----
      if(type != curType){
         refk <- lapply(
            x[[tableName]]$foreignKeys,
            function(y){
               fieldName %in% y$key$from
            }
         ) %>%
            unlist() %>%
            as.logical() %>%
            which()
         tefk <- lapply(
            x,
            function(y){
               lapply(
                  y$foreignKeys,
                  function(z){
                     tableName==z$refTable &
                        fieldName %in% z$key$to
                  }
               ) %>%
                  unlist() %>%
                  as.logical() %>%
                  which()
            }
         )
         if(length(refk)>0 || any(unlist(lapply(tefk, length))>0)){
            stop(
               "Cannot change the type of a field involved in a foreign key.\n",
               "You should first remove the foreign key involving it."
            )
         }
      }
      ## Updating field information ----
      x[[tableName]]$fields <- x[[tableName]]$fields %>%
         dplyr::mutate(
            type=ifelse(.data$name==fieldName, !!type, .data$type),
            nullable=ifelse(.data$name==fieldName, !!nullable, .data$nullable),
            unique=ifelse(.data$name==fieldName, !!unique, .data$unique),
            comment=ifelse(.data$name==fieldName, !!comment, .data$comment)
         )
   }

   return(RelDataModel(x))
}


###############################################################################@
#' Update the display of a table of a [RelDataModel]
#'
#' @param x a [RelDataModel]
#' @param tableName the name of the table to modify (a single character)
#' @param px the position of the table: x value
#' @param py the position of the table: y value
#' @param color the color of the table
#' @param comment a table description/comment
#'
#' @return A [RelDataModel]
#'
#' @export
#'
update_table_display <- function(
   x, tableName, px=NULL, py=NULL, color=NULL, comment=NULL
){
   stopifnot(is.RelDataModel(x))
   px <- ifelse(
      is.null(px),
      x[[tableName]]$display$x,
      px
   )
   py <- ifelse(
      is.null(py),
      x[[tableName]]$display$y,
      py
   )
   color <- ifelse(
      is.null(color),
      x[[tableName]]$display$color,
      color
   )
   comment <- ifelse(
      is.null(comment),
      x[[tableName]]$display$comment,
      comment
   )
   px <- as.numeric(px)
   py <- as.numeric(py)
   color <- as.character(color)
   comment <- as.character(comment)
   stopifnot(
      is.character(tableName), length(tableName)==1,
      tableName %in% names(x),
      is.numeric(px), length(px)==1,
      is.numeric(py), length(py)==1,
      is.character(color), length(color)==1,
      is.character(comment), length(comment)==1
   )
   ## Updating table display ----
   x <- unclass(x)
   x[[tableName]]$display$x <- px
   x[[tableName]]$display$y <- py
   x[[tableName]]$display$color <- color
   x[[tableName]]$display$comment <- comment
   return(RelDataModel(x))
}

###############################################################################@
#' Copy fields from one table to another in a [RelDataModel]
#'
#' @param x a [RelDataModel]
#' @param from the name of the table from which the fields are taken
#' @param to the name of the table to which the fields are copied
#' @param fields the names of the fields to copy
#'
#' @return A [RelDataModel]
#'
#' @export
#'
copy_fields <- function(x, from, to, fields){
   stopifnot(
      is.RelDataModel(x),
      is.character(from) && length(from)==1 && !is.na(from) &&
         from %in% names(x),
      is.character(to) && length(to)==1 && !is.na(to) &&
         to %in% names(x),
      is.character(fields) && length(fields)>0 && !any(is.na(fields)) &&
         all(fields %in% x[[from]]$fields$name) &&
         !any(fields %in% x[[to]]$fields$name)
   )
   for(f in fields){
      ff <- dplyr::filter(x[[from]]$fields, .data$name==f)
      x <- add_field(
         x,
         tableName=to,
         name=f,
         type=ff$type,
         nullable=ff$nullable,
         unique=ff$unique,
         comment=ff$comment
      )
   }
   return(x)
}

###############################################################################@
#' Pre-compute [RelDataModel] layout when missing any x or y table position
#'
#' @param x a [RelDataModel]
#' @param layout character name of igraph layout function to use
#' (Default: "layout_nicely").
#' @param lengthMultiplier a numeric value to scale x and y coordinate
#' (default: 40*length(x))
#' @param force if TRUE autolayout even if all tables have coordinates
#' (default: FALSE)
#'
#' @return A [RelDataModel]
#'
#' @export
#'
auto_layout <- function(
   x,
   layout="layout_nicely",
   lengthMultiplier=40*length(x),
   force=FALSE
){
   stopifnot(is.RelDataModel(x))
   if(length(x)==0){
      return(x)
   }
   mn <- modelToVn(x)
   if(force){
      mn$nodes$x <- NA
      mn$nodes$y <- NA
   }
   if(any(is.na(mn$nodes$x)) || any(is.na(mn$nodes$y))){
      vp <- mn$nodes %>%
         dplyr::select(-"x", -"y") %>%
         visNetwork::visNetwork(mn$edges) %>%
         visNetwork::visIgraphLayout(layout=layout, randomSeed=2)
      x <- lapply(
         x,
         function(n){
            i <- which(vp$x$nodes$id==n$tableName)
            n$display$x <- vp$x$nodes$x[i]*lengthMultiplier
            n$display$y <- vp$x$nodes$y[i]*lengthMultiplier
            return(n)
         }
      )
      class(x) <- c("RelDataModel", "list")
   }
   return(x)
}

###############################################################################@
#' Confront a [RelDataModel] to actual data
#'
#' @param x a [RelDataModel]
#' @param data a list of data frames to be confronted with the model.
#' @param paths a character vector with file paths taken into account if
#' the data is empty.
#' The file [basename] without extension
#' will be considered as the table name.
#' @param returnData a logical indicating if the data should be returned
#' with the report (default: FALSE).
#' @param verbose a single logical value indicating if some process information
#' should be displayed (default: TRUE)
#' @param n_max maximum number of records to read (default: Inf).
#' @param checks a character vector with the name of optional checks to be done
#' (Default: if n_max==Inf ==> all of
#' them c("unique", "not nullable", "foreign keys"),
#' else ==> none)
#' @param delim single character used to separate fields within a record
#' (default: "\\t")
#' @param ... supplementary parameters for the [read_delim] function.
#'
#' @return A report as a list
#'
#' @example inst/examples/ex_read_json.R
#'
#' @export
#'
confront_data <- function(
   x,
   data=list(),
   paths=NULL,
   returnData=FALSE,
   verbose=TRUE,
   n_max=Inf,
   checks=if(n_max==Inf){
      c("unique", "not nullable", "foreign keys")
   }else{
      as.character()
   },
   delim="\t",
   ...
){
   stopifnot(is.RelDataModel(x))

   read_td <- function(tm, tn){
      if(is.MatrixModel(tm)){
         ismm <- is_MM(paths[tn])
         if(ismm){
            td <- read_named_MM(
               paths[tn],
               n_max=n_max
            )
         }else{
            r1 <- attr(ismm, "r1")
            cn <- r1 %>%
               strsplit(split=delim) %>%
               unlist()
            cn <- gsub("['`]", "", cn)
            cn <- gsub('["]', "", cn)

            vt <- tm$fields %>%
               dplyr::filter(!.data$type %in% c("row", "column")) %>%
               dplyr::pull("type")
            ctypes <- do.call(
               readr::cols,
               structure(
                  list(
                     readr::col_character(),
                     .default = switch(
                        vt,
                        "integer"=readr::col_integer(),
                        "numeric"=readr::col_double(),
                        "logical"=readr::col_logical(),
                        "character"=readr::col_character(),
                        "Date"=readr::col_date(),
                        "POSIXct"=readr::col_datetime()
                     )
                  ),
                  .Names=c("___ROWNAMES___", ".default")
               )
            )
            td <- readr::read_delim(
               paths[tn],
               delim=delim, n_max=n_max, skip=1,
               col_types=ctypes,
               col_names=c("___ROWNAMES___", cn[-1]),
               ...
            ) %>% as.data.frame(stringsAsFactors=FALSE)
            stopifnot(
               !any(duplicated(colnames(td))),
               !any(duplicated(td[[1]]))
            )
            rownames(td) <- td[[1]]
            td <- as.matrix(td[, -1, drop=FALSE])

         }

      }else{
         td <- readr::read_delim(
            paths[tn],
            delim=delim, n_max=n_max,
            col_types=col_types(tm),
            ...
         )
      }
      return(td)
   }

   ## Optional checks ----
   if(length(checks)>0){
      checks <- match.arg(
         checks,
         c("unique", "not nullable", "foreign keys"),
         several.ok=TRUE
      )
   }

   ## Empty model and data
   if(length(x)==0 && length(data)==0 && length(paths)==0){
      toRet <- list(
         model=x,
         checks=checks,
         n_max=n_max,
         missingTables=NULL,
         suppTables=NULL,
         availableTables=NULL,
         constraints=list(),
         success=TRUE
      )
      if(verbose){
         cat(format_confrontation_report(toRet))
      }
      return(invisible(toRet))
   }

   ## Data files ----
   if(length(data)==0){
      stopifnot(is.character(paths), length(paths)>0)
      nf <- paths[which(!file.exists(paths))]
      if(length(nf)>0){
         stop(
            "Cannot find the following files:\n   - ",
            paste(nf, collapse="\n   - ")
         )
      }
      names(paths) <- sub(
         pattern="(\\.[[:alnum:]]+)(\\.gz)?$", replacement="",
         x=basename(paths)
      )
      dtn <- names(paths)
   }else{
      dtn <- names(data)
      stopifnot(length(dtn)>0)
   }

   ## Available tables ----
   missingTables <- setdiff(names(x), dtn)
   suppTables <- setdiff(dtn, names(x))
   availableTables <- intersect(names(x), dtn)
   toRet <- list(
      model=x,
      checks=checks,
      n_max=n_max,
      missingTables=missingTables,
      suppTables=suppTables,
      availableTables=availableTables,
      constraints=list(),
      success=length(missingTables)==0 && length(suppTables)==0
   )

   ## Table checks ----
   if(length(availableTables)>0) for(ti in 1:length(availableTables)){
      tn <- availableTables[ti]
      if(verbose){
         message(sprintf(
            'Processing "%s" (table %s / %s) ',
            tn, ti, length(availableTables)
         ))
      }
      tm <- x[[tn]]
      if(tn %in% names(data)){
         td <- data[[tn]]
      }else{
         td <- read_td(tm, tn)
         if(returnData){
            data[[tn]] <- td
         }
      }
      ## _+ Simple table ----
      tr <- confront_table_data(x=tm, d=td, checks=checks)
      toRet$success <- toRet$success && tr$success
      ## _+ Foreign keys ----
      tfk <- tm$foreignKeys
      if("foreign keys" %in% checks && length(tfk)>0){
         fkr <- list()
         for(i in 1:length(tfk)){
            tfki <- tfk[[i]]
            rtn <- tfki$refTable
            if(rtn %in% missingTables){
               fkr[[i]] <- list(
                  "success"=FALSE,
                  "message"="Missing reference table."
               )
               tr$success <- FALSE
               toRet$success <- FALSE
               next()
            }
            ##
            if(tfki$cardinality["fmin"]==0 && tfki$cardinality["tmin"]==0){
               fkr[[i]] <- list("success"=TRUE)
               next()
            }
            ##
            if(rtn==tn){
               rtd <- td
               rtm <- tm
            }else{
               rtm <- x[[rtn]]
               if(rtn %in% names(data)){
                  rtd <- data[[rtn]]
               }else{
                  rtd <- read_td(rtm, rtn)
                  if(returnData){
                     data[[rtn]] <- rtd
                  }
               }
            }
            ##
            success <- TRUE
            message <- NULL
            if(tfki$cardinality["fmin"]>0){
               if(is.MatrixModel(tm) || is.MatrixModel(rtm)){
                  for(j in 1:length(tfki$key$from)){
                     ff <- tfki$key$from[j]
                     if(is.MatrixModel(tm)){
                        fft <- tm$fields$type[which(tm$fields$name==ff)]
                        if(fft=="row"){
                           ffv <- rownames(td)
                        }else{
                           ffv <- colnames(td)
                        }
                     }else{
                        ffv <- unique(td[[ff]])
                     }
                     tf <- tfki$key$to[j]
                     if(is.MatrixModel(rtm)){
                        tft <- rtm$fields$type[which(rtm$fields$name==tf)]
                        if(tft=="row"){
                           tfv <- rownames(rtd)
                        }else{
                           tfv <- colnames(rtd)
                        }
                     }else{
                        tfv <- unique(rtd[[tf]])
                     }
                     fkissue <- !all(tfv %in% ffv)
                     if(fkissue){
                        break()
                     }
                  }
               }else{
                  . <- NULL
                  mt <- dplyr::anti_join(
                     dplyr::select(rtd, dplyr::all_of(tfki$key$to)) %>%
                        dplyr::filter_all(dplyr::any_vars(!is.na(.))),
                     dplyr::select(td, dplyr::all_of(tfki$key$from)),
                     by=dplyr::all_of(magrittr::set_names(
                        tfki$key$from, tfki$key$to
                     ))
                  )
                  fkissue <- nrow(mt)>0
               }
               if(fkissue>0){
                  success <- FALSE
                  message <- paste(c(
                     message,
                     sprintf(
                        "All keys of %s should be available in %s.",
                        rtn, tn
                     )
                  ), collapse=" ")
               }
            }
            if(tfki$cardinality["tmin"]>0){
               if(is.MatrixModel(tm) || is.MatrixModel(rtm)){
                  for(j in 1:length(tfki$key$from)){
                     ff <- tfki$key$from[j]
                     if(is.MatrixModel(tm)){
                        fft <- tm$fields$type[which(tm$fields$name==ff)]
                        if(fft=="row"){
                           ffv <- rownames(td)
                        }else{
                           ffv <- colnames(td)
                        }
                     }else{
                        ffv <- unique(td[[ff]])
                     }
                     tf <- tfki$key$to[j]
                     if(is.MatrixModel(rtm)){
                        tft <- rtm$fields$type[which(rtm$fields$name==tf)]
                        if(tft=="row"){
                           tfv <- rownames(rtd)
                        }else{
                           tfv <- colnames(rtd)
                        }
                     }else{
                        tfv <- unique(rtd[[tf]])
                     }
                     fkissue <- !all(ffv %in% tfv)
                     if(fkissue){
                        break()
                     }
                  }
               }else{
                  . <- NULL
                  mt <- dplyr::anti_join(
                     dplyr::select(td, dplyr::all_of(tfki$key$from)) %>%
                        dplyr::filter_all(dplyr::any_vars(!is.na(.))),
                     dplyr::select(rtd, dplyr::all_of(tfki$key$to)),
                     by=dplyr::all_of(magrittr::set_names(
                        tfki$key$to, tfki$key$from
                     ))
                  )
                  fkissue <- nrow(mt)>0
               }
               if(fkissue){
                  success <- FALSE
                  message <- paste(c(
                     message,
                     sprintf(
                        "All keys of %s should be available in %s.",
                        tn, rtn
                     )
                  ), collapse=" ")
               }
            }
            fkr[[i]] <- list(success=success, message=message)
            tr$success <- tr$success && success
            toRet$success <- toRet$success && success
         }
         tr$foreignKey <- fkr
      }
      toRet$constraints[[tn]] <- tr
   }
   if(returnData){
      toRet$data <- data
   }

   if(verbose){
      cat(format_confrontation_report(toRet))
   }

   ## Return the results
   invisible(toRet)
}

###############################################################################@
#' Check if two [RelDataModel] are identical
#'
#' @param x a [RelDataModel]
#' @param y a [RelDataModel]
#' @param ... additional parameters for [identical_RelTableModel()]
#'
#' @return A logical: TRUE if the 2 models are identical
#'
#' @export
#'
identical_RelDataModel <- function(x, y, ...){
   stopifnot(is.RelDataModel(x), is.RelDataModel(y))
   toRet <- length(x)==length(y) && all(sort(names(x))==sort(names(y)))
   if(toRet && length(x)>0){
      for(i in 1:length(x)){
         itoRet <- identical_RelTableModel(
            x[[names(x)[i]]], y[[names(x)[i]]],
            ...
         )
         if(!itoRet){
            message(sprintf("Tables %s are different", names(x)[i]))
         }
         toRet <- toRet && itoRet
      }
   }else{
      if(length(x)>0){
         message("Not the same tables")
      }
   }
   return(toRet)
}



###############################################################################@
#' Get foreign keys in [RelDataModel]
#'
#' @param x a [RelDataModel]
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
get_foreign_keys.RelDataModel <- function(x){
   do.call(rbind, lapply(x, get_foreign_keys))
}
