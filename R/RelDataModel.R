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
#' @return Nothing. The function throws an error if there is an issue with
#' foreign keys.
#'
#' @export
#'
check_foreign_keys.RelDataModel <- function(x){
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
                  tot <- ft$fields$type[which(ft$fields$name==to)]
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
#' Get the number of tables in a [RelDataModel] object
#'
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
            if(!is.null(tm$foreignKeys)){
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
'names<-.RelDataModel' <- function(x, value, ...){
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
   return(RelDataModel(
      l=l,
      ...
   ))
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
         tables <- tibble(
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
            mutate(
               table=tm$tableName
            )
         ##
         if(length(tm$primaryKey)==0){
            primaryKeys <- tibble(
               table=character(),
               field=character()
            )
         }else{
            primaryKeys <- tibble(table=tm$tableName, field=tm$primaryKey)
         }
         ##
         if(length(tm$foreignKeys)==0){
            foreignKeys <- tibble(
               table=character(),
               fki=integer(),
               field=character(),
               refTable=character(),
               refField=character()
            )
         }else{
            foreignKeys <- do.call(bind_rows, lapply(
               1:length(tm$foreignKeys),
               function(fki){
                  tm$foreignKeys[[fki]]$key %>%
                     mutate(
                        table=tm$tableName,
                        refTable=tm$foreignKeys[[fki]]$refTable,
                        fki=fki
                     ) %>%
                     rename("field"="from", "refField"="to") %>%
                     select(table, fki, field, refTable, refField)
               }
            ))
         }
         ##
         if(length(tm$indexes)==0){
            indexes <- tibble(
               table=character(),
               idx=integer(),
               field=character(),
               unique=logical()
            )
         }else{
            indexes <- do.call(bind_rows, lapply(
               1:length(tm$indexes),
               function(idx){
                  if(length(idx)==0){
                     return(NULL)
                  }
                  toRet <- tibble(
                     table=tm$tableName,
                     idx=idx,
                     unique=tm$indexes[[idx]]$unique,
                     field=tm$indexes[[idx]]$fields
                  ) %>% select(table, idx, field, unique)
                  return(toRet)
                  # tm$indexes[[idx]] %>%
                  #    mutate(
                  #       table=tm$tableName,
                  #       idx=idx
                  #    ) %>%
                  #    select(table, idx, field, unique)
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
      tables=do.call(bind_rows, lapply(toRet, function(x) x$tables)),
      fields=do.call(bind_rows, lapply(toRet, function(x) x$fields)),
      primaryKeys=do.call(bind_rows, lapply(toRet, function(x) x$primaryKeys)),
      foreignKeys=do.call(bind_rows, lapply(toRet, function(x) x$foreignKeys)),
      indexes=do.call(bind_rows, lapply(toRet, function(x) x$indexes))
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
   toRet <- list()
   for(tn in dbm$tables$name){
      tm <- list()
      tm$tableName <- tn
      tm$fields <- dbm$fields %>%
         filter(table==tn) %>%
         select(-table)
      tm$primaryKey <- dbm$primaryKeys %>%
         filter(table==tn) %>%
         pull(field)
      if(length(tm$primaryKey)==0){
         tm$primaryKey <- NULL
         tm <- c(tm, list(primaryKey=NULL))
      }
      tm$foreignKeys <- dbm$foreignKeys %>%
         filter(table==tn) %>%
         select(-table)
      if(nrow(tm$foreignKeys)>0){
         tm$foreignKeys <- split(tm$foreignKeys, tm$foreignKeys$fki) %>%
            lapply(function(x){
               x %>%
                  select(-fki) %>%
                  split(x$refTable) %>%
                  lapply(function(y){
                     list(
                        refTable=unique(y$refTable),
                        key=y %>%
                           select(field, refField) %>%
                           rename("from"="field", "to"="refField")
                     )
                  }) %>%
                  structure(.Names=NULL)
            }) %>%
            structure(.Names=NULL) %>%
            do.call(c, .)
      }else{
         tm$foreignKeys <- NULL
         tm <- c(tm, list(foreignKeys=NULL))
      }
      tm$indexes <- dbm$indexes %>%
         filter(table==tn) %>%
         select(-table)
      if(nrow(tm$indexes)>0){
         tm$indexes <- split(tm$indexes, tm$indexes$idx) %>%
            # lapply(select, -idx) %>%
            lapply(function(x){
               list(fields=x$field, unique=unique(x$unique))
            }) %>%
            structure(.Names=NULL)
      }else{
         tm$indexes <- NULL
         tm <- c(tm, list(indexes=NULL))
      }
      tm$display <- dbm$tables %>%
         filter(name==tn) %>%
         select(-name) %>%
         as.list()
      toRet <- c(toRet, list(RelTableModel(tm)))
   }
   return(RelDataModel(toRet))
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
rename_table.RelDataModel <- function(x, old, new){
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
add_table.RelDataModel <- function(x, newTable){
   stopifnot(is.RelTableModel(newTable) || is.character(newTable))
   if(is.character(newTable)){
      stopifnot(
         length(newTable)==1
      )
      newTable <- RelTableModel(list(
         "tableName"=newTable,
         "fields"=tibble(
            name=character(),
            type=character(),
            nullable=logical(),
            unique=logical(),
            comment=character()
         ),
         "primaryKey"=NULL,
         "foreignKeys"=NULL,
         "indexes"=NULL,
         "display"=list(
            x=as.numeric(NA), y=as.numeric(NA),
            color=as.character(NA),
            comment=as.character(NA)
         )
      ))
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
remove_table.RelDataModel <- function(x, tableName, rmForeignKeys=FALSE){
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
add_field.RelDataModel <- function(
   x, tableName,
   name, type, nullable, unique, comment
){
   stopifnot(
      is.character(tableName), length(tableName)==1,
      !is.na(tableName),
      tableName %in% names(x),
      is.character(name), length(name)==1,
      !is.na(name),
      !name %in% x[[tableName]]$fields$name,
      is.character(type), length(type)==1,
      !is.na(type), type %in% SUPPTYPES,
      is.logical(nullable), length(nullable)==1,
      !is.na(nullable),
      is.logical(unique), length(unique)==1,
      !is.na(unique),
      is.character(comment), length(comment)==1
   )
   x <- unclass(x)
   x[[tableName]]$fields <- bind_rows(
      x[[tableName]]$fields,
      tibble(
         name=name,
         type=type,
         nullable=nullable,
         unique=unique,
         comment=comment
      )
   )
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
rename_field.RelDataModel <- function(x, tableName, current, new){
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
   ## Adapting indexes
   if(length(tm$indexes)>0) for(i in 1:length(tm$indexes)){
      tm$indexes[[i]][
         which(tm$indexes[[i]]$field==current),
         "field"
      ] <- new
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
remove_field.RelDataModel <- function(
   x,
   tableName,
   fieldName,
   rmForeignKeys=FALSE
){
   stop("NOT IMPLEMENTED YET")
   stopifnot(
      is.character(tableName), length(tableName)==1,
      tableName %in% names(x),
      is.character(fieldName), length(fieldName)==1,
      fieldName %in% x[[tableName]]$fields$name
   )
   x <- unclass(x)
   tm <- x[[tableName]]
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
set_primary_key.RelDataModel <- function(x, tableName, fieldNames){
   stop("NOT IMPLEMENTED YET")
   fieldNames <- as.character(fieldNames)
   stopifnot(
      is.character(tableName), length(tableName)==1,
      tableName %in% names(x),
      all(fieldNames %in% x[[tableName]]$fields$name)
   )
}

###############################################################################@
#'
#' @param x a [RelDataModel]
#'
#' @return A [RelDataModel]
#'
#' @export
#'
add_foreign_key.RelDataModel <- function(x, fromTable, fromFields, toTable, toFields){
   stop("NOT IMPLEMENTED YET")
}

###############################################################################@
#'
#' @param x a [RelDataModel]
#'
#' @return A [RelDataModel]
#'
#' @export
#'
remove_foreign_key.RelDataModel <- function(x, fromTable, fromFields, toTable, toFields){
   stop("NOT IMPLEMENTED YET")
}

###############################################################################@
#'
#' @param x a [RelDataModel]
#'
#' @return A [RelDataModel]
#'
#' @export
#'
add_index.RelDataModel <- function(x, tableName, fieldNames, uniques){
   stop("NOT IMPLEMENTED YET")
}

###############################################################################@
#'
#' @param x a [RelDataModel]
#'
#' @return A [RelDataModel]
#'
#' @export
#'
remove_index.RelDataModel <- function(x, tableName, fieldNames){
   stop("NOT IMPLEMENTED YET")
}

###############################################################################@
#'
#' @param x a [RelDataModel]
#'
#' @return A [RelDataModel]
#'
#' @export
#'
update_table_display.RelDataModel <- function(x, tableName, px, py, color, comment){
   stop("NOT IMPLEMENTED YET")
}

###############################################################################@
#'
#' @param x a [RelDataModel]
#'
#' @return A [RelDataModel]
#'
#' @export
#'
update_field.RelDataModel <- function(x, tableName, fieldName, type, nullable, comment){
   stop("NOT IMPLEMENTED YET")
}

