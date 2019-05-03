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
      checkForeignKeys(toRet)
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
checkForeignKeys.RelDataModel <- function(x){
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
               misRefFields <- setdiff(fks$key$to, ft$fields$name)
               if(length(misRefFields)>0){
                  stop(sprintf(
                     "Cannot find the following fields in the %s table: %s",
                     fks$refTable,
                     paste(misRefFields, collapse=", ")
                  ))
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
                  tm$indexes[[idx]] %>%
                     mutate(
                        table=tm$tableName,
                        idx=idx
                     ) %>%
                     select(table, idx, field, unique)
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
            lapply(select, -idx) %>%
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
#' Plot a [RelDataModel] object
#'
#' This function draw a visNetwork of the model.
#'
#' @importFrom visNetwork visNetwork visPhysics visLayout visOptions
#' @importFrom magrittr %>%
#'
#' @export
#'
plot.RelDataModel <- function(x){

   modelToVn <- function(model){
      nodes <- do.call(rbind, lapply(
         model,
         function(m){
            f <- m$fields
            pk <- m$primaryKey
            it <- indexTable(m)
            ind <- NULL
            uq <- NULL
            if(!is.null(it)){
               it <- it %>% filter(index!=0)
               ind <- unique(it$field)
               uq <- unique(it$field[which(it$unique)])
            }
            f$i <- unlist(lapply(
               f$name,
               function(n){
                  paste(sort(it$index[which(it$field==n)]), collapse=",")
               }
            ))
            flab <- paste(sprintf(
               '    - %s%s%s%s%s {%s%s}%s',
               ifelse(f$nullable, "(", ""),
               ifelse(f$name %in% pk, "<b>", ""),
               ifelse(f$name %in% uq, "*", ""),
               f$name,
               ifelse(f$name %in% pk, "</b>", ""),
               f$type,
               ifelse(
                  f$name %in% ind,
                  paste0(" - idx.", f$i),
                  ""
               ),
               ifelse(f$nullable, ")", "")
            ), collapse="\n")
            label <- paste(
               sprintf('<b>%s</b>', m$tableName),
               flab,
               sep="\n"
            )
            ftit <- paste(sprintf(
               ' - %s%s%s',
               f$name,
               ifelse(is.na(f$comment)|f$comment=="", "", ": "),
               ifelse(is.na(f$comment)|f$comment=="", "", f$comment)
            ), collapse="<br>")
            title <- paste(
               sprintf('<b>%s</b>', m$tableName),
               ftit,
               sep="<br>"
            )
            return(tibble(
               tableName=m$tableName,
               label=label,
               title=title,
               shape="box",
               font.multi=TRUE,
               font.align="left"
            ))
         }
      ))
      nodes$id <- names(model)

      edges <- do.call(rbind, lapply(
         model,
         function(m){
            mn <- m$tableName
            mt <- m$tableName
            fk <- m$foreignKeys
            if(is.null(fk)){
               return(NULL)
            }
            toRet <- do.call(rbind, lapply(
               fk,
               function(k){
                  to <- k$refTable
                  title <- paste0(
                     '<tr style="border: 1px solid black">',
                     '<td style="border: 1px solid black">', k$key$from,
                     '</td>',
                     '<td style="border: 1px solid black">', k$key$to,
                     '</td>',
                     '</tr>'
                  )
                  title <- paste0(
                     '<table style="border: 1px solid black">',
                     '<tr style="border: 1px solid black">',
                     sprintf(
                        '<th style="border: 1px solid black">%s</th>', mt
                     ),
                     sprintf(
                        '<th style="border: 1px solid black">%s</th>',
                        k$refTable
                     ),
                     '</tr>',
                     paste(title, collapse=""),
                     '</table>'
                  )
                  return(tibble(to=to, title=title))
               }
            ))
            toRet$from <- mn
            toRet$arrows <- "to"
            toRet$font.align <- "bottom"
            return(toRet)
         }
      ))
      if(is.null(edges)){
         edges <- tibble(from=character(), to=character())
      }

      return(list(nodes=nodes, edges=edges))
   }

   toPlot <- modelToVn(x)

   visNetwork(nodes=toPlot$nodes, edges=toPlot$edges) %>%
      visPhysics(
         solver="repulsion",
         repulsion=list(
            nodeDistance=100,
            springLength=100,
            springConstant=0.001,
            damping=1
         )
      ) %>%
      visLayout(randomSeed=2) #%>%
      # visOptions(selectedBy="tableName", highlightNearest=TRUE) %>%
      # visIgraphLayout(smooth=TRUE, type="full", randomSeed=2)

}
