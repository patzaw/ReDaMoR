###############################################################################@
#' Read a data model from JSON
#'
#' @param txt a JSON string, URL or file
#'
#' @example inst/examples/ex_read_json.R
#'
#' @export
#'
read_json_data_model <- function(txt){
   mfj <- jsonlite::fromJSON(txt, simplifyVector=FALSE)
   lapply(
      mfj,
      function(x){
         ## table name ----
         x$tableName <- unlist(x$tableName)
         ## fields ----
         if(length(x$fields)>0){
            x$fields <- do.call(rbind, lapply(x$fields, as_tibble)) %>%
               mutate(
                  name=as.character(.data$name),
                  type=as.character(.data$type),
                  nullable=as.logical(.data$nullable),
                  unique=as.logical(.data$unique),
                  comment=as.character(.data$comment)
               )
         }else{
            x$fields <- tibble(
               name=character(),
               type=character(),
               nullable=logical(),
               unique=logical(),
               comment=character()
            )
         }
         ## primary key ----
         x$primaryKey <- as.character(x$primaryKey)
         ## foreign keys ----
         x$foreignKeys <- lapply(
            x$foreignKeys,
            function(y){
               y$refTable <- unlist(y$refTable)
               y$key <- do.call(rbind, lapply(y$key, as_tibble))
               y$cardinality <- as.integer(unlist(y$cardinality))
               names(y$cardinality) <- c("fmin", "fmax", "tmin", "tmax")
               return(y)
            }
         )
         ## indexes ----
         x$indexes <- lapply(
            x$indexes,
            function(y){
               y$fields <- unlist(y$fields)
               y$unique <- unlist(y$unique)
               return(y)
            }
         )
         ## display ----
         x$display <- lapply(x$display, function(y){
            y <- unlist(y)
            ifelse(y=="NA", NA, y)
         })
         x$display$x <- as.numeric(x$display$x)
         x$display$y <- as.numeric(x$display$y)
         x$display$color <- as.character(x$display$color)
         x$display$comment <- as.character(x$display$comment)
         RelTableModel(x)
      }
   ) %>% RelDataModel()
}

###############################################################################@
#' Write a data model in a JSON file
#'
#' @param x the model to be written
#' @param path file on disk
#'
#' @export
#'
write_json_data_model <- function(x, path){
   jsonlite::write_json(x, path=path, na="string", pretty=TRUE)
}
