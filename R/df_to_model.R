###############################################################################@
#' Create a [RelDataModel] object from column names of data frames
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
#' @return A [RelDataModel] object.
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
      values,
      inherits, c("data.frame", "matrix", "Matrix")
   ) %>%
      unlist()
   if(any(!isdf)){
      stop(
         "The following objects are neither of class ",
         "data.frame, matrix nor Matrix: ",
         paste(names(isdf)[!isdf], collapse=", ")
      )
   }
   ## Build the data model ----
   toRet <- list()
   for(tn in names(values)){
      df <- values[[tn]]
      if(inherits(df, c("matrix", "Matrix"))){
         toRet[[tn]] <- RelTableModel(
            tableName=tn,
            fields=dplyr::tibble(
               name=c("row", "column", "value"),
               type=c("row", "column", class(df[1])),
               nullable=c(FALSE, FALSE, TRUE),
               unique=c(FALSE, FALSE, FALSE),
               comment=as.character(NA)
            ),
            primaryKey=c("row", "column"),
            foreignKeys=NULL,
            indexes=NULL,
            display=list(
               x=as.numeric(NA), y=as.numeric(NA),
               color=as.character(NA),
               comment=as.character(NA)
            )
         )
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
         toRet[[tn]] <- RelTableModel(
            tableName=tn,
            fields=dplyr::tibble(
               name=colnames(df),
               type=types,
               nullable=TRUE,
               unique=FALSE,
               comment=as.character(NA)
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
   }
   return(RelDataModel(toRet))
}

###############################################################################@
#' Guess [RelDataModel] constraints based on the provided or existing tables
#'
#' @param x a [RelDataModel]
#' @param data a named list of tables. All names of x should exist in data. If
#' NULL, the data are taken from env.
#' @param env the R environment in which to find the tables
#' @param constraints the type of constraints to guess
#'
#' @return A [RelDataModel]
#'
#' @details
#' The guessed constraints should be carefully review, especially the foreign
#' keys.
#'
#' Complex foreign keys involving multiple fields are not guessed.
#'
#' @example inst/examples/ex_from_df.R
#'
#' @export
#'
guess_constraints <- function(
      x,
      data = NULL,
      env = parent.frame(n=1),
      constraints = c("unique", "not nullable", "foreign keys")
){
   dml <- unclass(x)
   if(is.null(data)){
      data <- lapply(names(x), get, envir = env) %>%
         stats::setNames(names(x))
   }
   stopifnot(all(names(x) %in% names(data)))
   stopifnot(all(unlist(
      lapply(data, inherits, what = c("matrix", "Matrix", "data.frame"))
   )))
   for(n in names(dml)){
      d <- data[[n]]
      if(is.data.frame(d)){
         if("not nullable" %in% constraints){
            nullable <- lapply(
               d,
               function(x) any(is.na(x))
            ) %>%
               unlist()
            dml[[n]]$fields$nullable <- nullable
         }
         if("unique" %in% constraints){
            uni <- lapply(
               d,
               function(x) !is.numeric(x) && !any(duplicated(x))
            ) %>%
               unlist()
            dml[[n]]$fields$unique <- uni
         }
      }else{
         if("not nullable" %in% constraints){
            nullable <- any(is.na(d))
            dml[[n]]$fields$nullable[which(
               ! dml[[n]]$fields$type %in% c("row", "column")
            )] <- nullable
         }
      }
   }
   dm <- RelDataModel(dml)
   if("foreign keys" %in% constraints){
      dbm <- toDBM(dm)
      pfk <- dplyr::inner_join(
         dbm$fields %>%
            dplyr::filter(!.data$nullable & .data$unique) %>%
            select(-"nullable", -"unique", -"comment", -"fieldOrder"),
         dbm$fields %>%
            select(-"comment", -"fieldOrder"),
         by = c("name", "type"),
         suffix = c(".p", ".f"),
         relationship = "many-to-many"
      ) %>%
         dplyr::filter(.data$table.p != .data$table.f)
      if(nrow(pfk) > 0){
         drows <- lapply(data, nrow) %>% unlist()
         pfk <- pfk %>%
            dplyr::mutate(
               nrow.p = drows[.data$table.p],
               nrow.f = drows[.data$table.f]
            ) %>%
            dplyr::filter(
               !.data$unique | .data$nrow.f <= .data$nrow.p
            )
         if(nrow(pfk) > 0){
            for(i in 1:nrow(pfk)){
               if(all(
                  setdiff(data[[pfk$table.f[i]]][[pfk$name[i]]], NA) %in%
                  data[[pfk$table.p[i]]][[pfk$name[i]]]
               ))
                  dm <- add_foreign_key(
                     dm,
                     fromTable = pfk$table.f[i],
                     fromFields = pfk$name[i],
                     toTable = pfk$table.p[i],
                     toFields = pfk$name[i],
                     tmin = ifelse(pfk$nullable[i], 0L, 1L)
                  )
            }
         }
      }
   }
   return(dm)
}
