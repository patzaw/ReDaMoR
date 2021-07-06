#' Read a data model from an SQL file from the MySQL Workbench
#'
#' @param f the SQL file to read
#' @param typeRef the reference for type conversion
#' (Default: "MySQLWB"; see [list_type_ref()])
#' @param mysqlcomments if MySQL comments (starting with #) should be removed
#' (Default: TRUE)
#'
#' @details Database, table and field names should be surrounded by "`".
#'
#' @return A [RelDataModel] object
#'
#' @example inst/examples/ex_read_sql.R
#'
#' @export
#'
read_SQL_data_model <- function(f, typeRef="MySQLWB", mysqlcomments=TRUE){

   ############################################################################@
   ## Helpers ----

   ###################################################@
   getBrackPos <- function(x){
      oBrackPos <- setdiff(gregexpr('[(]', x)[[1]], -1)
      if(length(oBrackPos)==0){
         return(data.frame(s=1, e=1)[-1,])
      }
      cBrackPos <- setdiff(gregexpr('[)]', x)[[1]], -1)
      if(length(cBrackPos)!=length(oBrackPos)){
         stop("Closing brackets not matching opening brackets")
      }
      posBrack <- dplyr::tibble(
         pos=c(oBrackPos, cBrackPos),
         brack=c(rep("(", length(oBrackPos)), rep(")", length(cBrackPos)))
      ) %>%
         dplyr::arrange(.data$pos) %>%
         dplyr::mutate(oc=ifelse(.data$brack=="(", 1, -1)) %>%
         dplyr::mutate(ib=cumsum(.data$oc))
      e <- which(posBrack$ib==0)
      s <- c(1, e[-length(e)]+1)
      return(dplyr::tibble(
         s=posBrack$pos[s],
         e=posBrack$pos[e]
      ))
   }

   ###################################################@
   getVarNamePos <- function(x){
      varNamePos <- gregexpr('`[^`]*`', x)[[1]]
      if(varNamePos[1]==-1){
         return(dplyr::tibble(s=1, e=1)[-1,])
      }
      return(dplyr::tibble(
         s=varNamePos,
         e=varNamePos+attr(varNamePos, "match.length")-1
      ))
   }

   ###################################################@
   splitByPos <- function(x, pos, keepPos=c('NO', 'right', 'left')){
      keepPos <- match.arg(keepPos)
      stopifnot(all(pos>0), all(pos<=nchar(x)), length(x)==1)
      if(length(pos)==0){
         return(x)
      }
      einc <- switch(
         keepPos,
         "NO"=-1,
         "right"=0,
         "left"=-1
      )
      sinc <- switch(
         keepPos,
         "NO"=+1,
         "right"=+1,
         "left"=0
      )
      toRet <- list()
      s <- 1
      for(i in 1:length(pos)){
         p <- pos[i]
         e <- p + einc
         toRet[[i]] <- substr(x, s, e)
         s <- p+sinc
      }
      e <- nchar(x)
      if(s <= e){
         toRet[[i+1]] <- substr(x, s, e)
      }else{
         toRet[[i+1]] <- ""
      }
      return(unlist(toRet))
   }

   ###################################################@
   getSubStatements <- function(x, sep=","){
      varNamePos <- getVarNamePos(x)
      brackPos <- getBrackPos(x)
      toExcl <- union(
         unlist(apply(varNamePos, 1, function(x) x[1]:x[2])),
         unlist(apply(brackPos, 1, function(x) x[1]:x[2]))
      )
      splitPos <- setdiff(gregexpr(sep, x)[[1]], c(-1, toExcl))
      toRet <- splitByPos(x, splitPos, keepPos = "NO")
      toRet <- sub("^[[:space:]]*", "", toRet)
      toRet <- sub("[[:space:]]*$", "", toRet)
      return(toRet)
   }

   ###################################################@
   parseCreateTableStatement <- function(cs, typeRef){

      ## Table ----
      header <- sub("[[:space:]]*[(].*", "", cs)
      p <- gregexpr('(`[^`]*`[.])?`[^`]*`', header)[[1]]
      tableFullName <- substr(header, p, p+attr(p, "match.length")-1) %>%
         strsplit(split="[.]") %>%
         unlist()
      tableFullName <- gsub("`", "", tableFullName)
      if(length(tableFullName)==1){
         tableFullName <- c("", tableFullName)
      }
      dbName <- tableFullName[1]
      tableName <- tableFullName[2]
      tableFullName <- paste0("`", dbName, "`.`", tableName, "`")

      ##
      body <- sub("[^(]*[(]", "", cs)
      body <- sub("[)][^)]*$", "", body)
      subStatements <- getSubStatements(body, sep=",")

      ## Fields ----
      fields <- grep("^[`]", subStatements, value=TRUE)
      types <- sub(" .*$", "", sub("^.*` *", "", fields))
      nullable <- rep(TRUE, length(fields))
      nullable[grep("NOT NULL", fields)] <- FALSE
      # comPos <- gregexpr(" *COMMENT *'[^']*'", fields)
      comPos <- gregexpr(" *COMMENT *'.*'", fields)
      comments <- rep("", length(fields))
      for(i in 1:length(comPos)){
         p <- comPos[[i]]
         if(p!=-1){
            toAdd <- substr(fields[i], p, p+attr(p, 'match.length')-1)
            toAdd <- sub(" *COMMENT *'", "", toAdd)
            toAdd <- sub("'$", "", toAdd)
            toAdd <- gsub('\\\\"' , '"', toAdd)
            toAdd <- gsub("\\\\'" , "'", toAdd)
            comments[i] <- toAdd
         }
      }
      fields <- dplyr::tibble(
         name=sub("[`].*$", "", sub("[`]", "", fields)),
         type=conv_type_ref(types, from=typeRef),
         nullable=nullable,
         comment=comments
      )

      ## Primary key ----
      primaryKey <- grep("^PRIMARY KEY ", subStatements, value=TRUE)
      primaryKey <- sub("^PRIMARY KEY [(]", "", primaryKey)
      primaryKey <- sub("[)]$", "", primaryKey)
      if(length(primaryKey) > 0){
         primaryKey <- getSubStatements(gsub('`', '', primaryKey), sep=",")
      }else{
         primaryKey <- NULL
      }

      ## Foreign keys ----
      foreignKeys <- grep(
         "^CONSTRAINT `[^`]*` FOREIGN KEY [(]",
         subStatements,
         value=TRUE
      )
      foreignKeys <- do.call(c, lapply(
         foreignKeys,
         function(x){
            fk <- sub(
               '[)].*$', "",
               sub( "^CONSTRAINT `[^`]*` FOREIGN KEY [(]", "", x)
            )
            fk <- gsub('`', '', getSubStatements(fk, sep=","))
            ref <- sub(
               "^CONSTRAINT `[^`]*` FOREIGN KEY [(].*[)] REFERENCES ", "", x
            )
            p <- gregexpr('(`[^`]*`[.])?`[^`]*`', ref)[[1]]
            refTableFullName <- substr(ref, p, p+attr(p, "match.length")-1) %>%
               strsplit(split="[.]") %>%
               unlist()
            refTableFullName <- gsub("`", "", refTableFullName)
            if(length(refTableFullName)==1){
               refTableFullName <- c("", refTableFullName)
            }
            refDB <- refTableFullName[1]
            refTable <- refTableFullName[2]
            refTableFullName <- paste0("`", refDB, "`.`", refTable, "`")

            refKeys <- gsub('`', '', getSubStatements(sub(
               '^.*[(]', "",
               sub('[)].*$', "", ref)
            ), sep=","))
            toRet <- dplyr::tibble(
               from=fk, to=refKeys
            )
            toRet <- list(list(
               refDB=refDB,
               refTable=refTable,
               key=toRet
            ))
            names(toRet) <- refTableFullName
            return(toRet)
         }
      ))

      ## Indexes ----
      indexes <- grep("^INDEX ", subStatements, value=TRUE)
      indexes <- sub("[)][^)]*$", "", sub("^INDEX [^(]*[(]", "", indexes))
      indexes <- lapply(
         indexes,
         function(x){
            toRet <- getSubStatements(x, sep=",")
            if(toRet[1]==""){
               return(NULL)
            }
            toRet <- do.call(rbind, lapply(
               toRet,
               function(y){
                  toRet <- getSubStatements(y, sep=" ")
                  toRet <- gsub('`', '', toRet)
                  return(toRet)
               }
            ))
            colnames(toRet) <- c("field", "order")
            toRet <- list(fields=toRet[,"field"], unique=FALSE)
            return(toRet)
         }
      )
      if(length(indexes)>0){
         indexes <- indexes[which(!unlist(lapply(indexes, is.null)))]
      }else{
         indexes <- NULL
      }

      ## Unique indexes ----
      uindexes <- grep("^UNIQUE INDEX ", subStatements, value=TRUE)
      uindexes <- sub(
         "[)][^)]*$", "",
         sub("^UNIQUE INDEX [^(]*[(]", "", uindexes)
      )
      uindexes <- unlist(lapply(
         uindexes,
         function(x){
            toRet <- getSubStatements(x, sep=",")
            if(toRet[1]==""){
               return(NULL)
            }
            toRet <- do.call(rbind, lapply(
               toRet,
               function(y){
                  toRet <- getSubStatements(y, sep=" ")
                  toRet <- gsub('`', '', toRet)
                  return(toRet)
               }
            ))
            colnames(toRet) <- c("field", "order")
            toRet <- toRet[,"field"]
            return(toRet)
         }
      ))
      fields <- fields %>%
         dplyr::mutate(unique=ifelse(.data$name %in% uindexes, TRUE, FALSE))

      toRet <- list(list(
         dbName=dbName,
         tableName=tableName,
         fields=fields,
         primaryKey=primaryKey,
         foreignKeys=foreignKeys,
         indexes=indexes, #c(indexes, uindexes),
         display=list(
            x=as.numeric(NA),
            y=as.numeric(NA),
            color=as.character(NA),
            comment=as.character(NA)
         )
      ))
      names(toRet) <- tableFullName
      return(toRet)

   }

   ############################################################################@
   ## Main ----

   typeRef <- match.arg(typeRef, list_type_ref())
   txt <- readLines(f) %>%
      paste(collapse="\n")
   txt <- gsub("/\\*((?!\\*/)(.|\n))*\\**/", "", txt, perl=T) %>%
      strsplit(split="\n") %>%
      unlist()
   txt <- sub("--.*$", "", txt)
   if(mysqlcomments){
      txt <- sub("#.*$", "", txt)
   }
   txt <- sub("^[[:space:]]*", "", txt)
   txt <- txt[which(txt!="")] %>%
      paste(collapse=" ") %>%
      strsplit(split=";") %>%
      unlist()
   txt <- sub("^[[:space:]]*", "", txt)
   txt <- sub("[[:space:]]*$", "", txt)
   txt <- gsub("[[:space:]]+", " ", txt)
   tdTxt <- grep("^CREATE TABLE", txt, ignore.case=TRUE, value=TRUE)
   unsupported <- grep("`", tdTxt, value=TRUE, invert=TRUE)
   if(length(unsupported)>0){
      stop(
         sprintf(
            '%s statement%s %s not supported because without "`":\n',
            length(unsupported),
            ifelse(length(unsupported)>1, "s", ""),
            ifelse(length(unsupported)>1, "are", "is")
         ),
         paste("   -", unsupported) %>% paste(collapse="\n")
      )
   }
   toRet <- do.call(c, lapply(
      tdTxt,
      parseCreateTableStatement,
      typeRef=typeRef
   ))
   dbNames <- unlist(lapply(toRet, function(x) x$dbName))
   if(length(unique(dbNames))>1){
      stop(
         "Several database names are mentioned in the SQL model. ",
         "There should be only one."
      )
   }
   toRet <- lapply(toRet, function(x)x[which(names(x)!="dbName")])
   toRet <- lapply(toRet, RelTableModel)
   toRet <- RelDataModel(toRet, checkFK=TRUE)
   return(toRet)

}

#' @describeIn read_SQL_data_model
#'
#' Deprecated version of read_SQL_data_model
#'
#' @param ... params for `read_SQL_data_model`
#'
#' @export
readSQLDataModel <- function(...){
   warning("Deprecated. Use read_SQL_data_model instead")
   read_SQL_data_model(...)
}
