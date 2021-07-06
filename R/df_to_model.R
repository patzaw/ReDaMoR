###############################################################################@
#' Create a RelTableModel object from column names of data frames
#'
#' @param ... the data frame objects, as names (unquoted) or character strings
#' (quoted)
#' @param list a character vector naming data frame objects
#' @param pos where to get the objects.
#' By default, uses the current environment.
#' See ‘details’ for other possibilities.
#' @param envir the environment to use. See ‘details’.
#'
#' @details The pos argument can specify the environment from which to get
#' the objects in any of several ways:
#' as an integer (the position in the search list);
#' as the character string name of an element in the search list;
#' or as an environment.
#' The envir argument is an alternative way to specify an environment,
#' but is primarily there for back compatibility.
#'
#' @return A RelTableModel object.
#'
#' @example inst/examples/ex_from_df.R
#'
#' @export
#'
df_to_model <- function(
   ...,
   list=character(),
   pos=-1,
   envir=as.environment(pos)
){
   ## Get object names ----
   ## This part has been taken from the `rm` function
   dots <- match.call(expand.dots=FALSE)$...
   if(
      length(dots) &&
      !all(vapply(
         dots,
         function(x){is.symbol(x) || is.character(x)},
         NA, USE.NAMES=FALSE
      ))
   ){
      stop("... must contain names or character strings")
   }
   names <- vapply(dots, as.character, "")
   if(length(names)==0){
      names <- character()
   }
   list <- c(list, names) %>% unique()
   ## Get and check object values ----
   values <- lapply(list, get, envir=envir, mode="any", inherits=TRUE)
   names(values) <- list
   isdf <- lapply(
      values, function(x){
         is.data.frame(x) || is.matrix(x)
      }
   ) %>%
      unlist()
   if(any(!isdf)){
      stop(
         "The following objects are not data frames: ",
         paste(names(isdf)[!isdf], collapse=", ")
      )
   }
   ## Build the data model ----
   toRet <- list()
   for(tn in names(values)){
      df <- values[[tn]]
      if(is.matrix(df)){
         toRet[[tn]] <- RelTableModel(l=list(
            "tableName"=tn,
            "fields"=dplyr::tibble(
               name=c("row", "column", "value"),
               type=c("row", "column", class(df[1])),
               nullable=c(FALSE, FALSE, TRUE),
               unique=c(FALSE, FALSE, FALSE),
               comment=as.character(NA)
            ),
            "primaryKey"=c("row", "column"),
            "foreignKeys"=NULL,
            "indexes"=NULL,
            "display"=list(
               x=as.numeric(NA), y=as.numeric(NA),
               color=as.character(NA),
               comment=as.character(NA)
            )
         ))
      }else{
         types <- character()
         for(cn in colnames(df)){
            ct <- df %>% dplyr::pull(!!cn) %>% class() %>% `[`(1)
            if(!ct %in% SUPPTYPES){
               stop(
                  sprintf(
                     'Type "%s" of column "%s" is not supported. ', ct, cn
                  ),
                  'Supported types are provided in "SUPPTYPES".'
               )
            }
            types <- c(types, ct)
         }
         toRet[[tn]] <- RelTableModel(l=list(
            "tableName"=tn,
            "fields"=dplyr::tibble(
               name=colnames(df),
               type=types,
               nullable=TRUE,
               unique=FALSE,
               comment=as.character(NA)
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
   }
   return(RelDataModel(toRet))
}
