###############################################################################@
#' Identify if a file is in MatrixMarket text format
#'
#' @param file the file to read
#'
#' @return A logical. If FALSE, the first line of the file is returned as
#' an attribute named "r1": `attr(is_MM, "r1")`
#'
#' @export
#'
is_MM <- function(file){
   r1 <- readLines(file, n=1)
   sms <- "%%MatrixMarket"
   toRet <- substr(r1, 1, nchar(sms))==sms
   if(!toRet){
      attr(toRet, "r1") <- r1
   }
   return(toRet)
}


###############################################################################@
#' Read the header of a named sparse matrix in MatrixMarket text format
#'
#' @param file the file to read
#' @param guess_max the number of lines to read to find the header.
#' (4 should be sufficient. Default: 20)
#'
#' @return A list with the following fields:
#' - rownames: a character vector with the matrix row names
#' - colnames: a character vector with the matrix column names
#' - rows: the number of matrix rows
#' - columns: the number of matrix columns
#' - values: the number of values in the matrix
#' - header_length: the number of lines in the header
#'
#' @export
#'
read_named_MM_header <- function(file, guess_max=20){
   h <- readLines(file, n=guess_max)
   rn <- strsplit(
      sub("^%%Rownames: ", "", h[grep("^%%Rownames: ", h)]),
      split="\t"
   )[[1]]
   cn <- strsplit(
      sub("^%%Colnames: ", "", h[grep("^%%Colnames: ", h)]),
      split="\t"
   )[[1]]
   hl <- grep("^%%", h, invert=TRUE)[1]
   n <- as.numeric(strsplit(
      h[hl],
      split=" "
   )[[1]])
   nr <- n[1]
   nc <- n[2]
   nv <- n[3]
   stopifnot(nr==length(rn), nc==length(cn))
   return(list(
      rownames=rn,
      colnames=cn,
      rows=nr,
      columns=nc,
      values=nv,
      header_length=hl
   ))
}

###############################################################################@
#' Read a named sparse matrix in MatrixMarket text format
#'
#' @param file the file to read
#' @param skip the number of records to skip (default: 0)
#' @param n_max the maximum number of records to read (default: Inf)
#' @param class the class of object to return. By default a "dgCMatrix".
#' If "tibble" is chosen, the sparse matrix is returned as a tibble with
#' 3 columns: i (row index), j (column index) and x (values) and
#' an "header" attribute containing the matrix rownames and colnames.
#' @param guess_max the number of lines to read to find the header.
#' (see [read_named_MM_header()])
#'
#' @return By default a dgCMatrix.
#' If the "tibble" class is chosen, the sparse matrix is returned as
#' a tibble with 3 columns: i (row index), j (column index) and x (values) and
#' an "header" attribute containing the matrix rownames and colnames.
#'
#' @export
#'
read_named_MM <- function(
   file, skip=0, n_max=Inf,
   class=c("dgCMatrix", "tibble"),
   guess_max=20
){
   class <- match.arg(class)
   h <- read_named_MM_header(file, guess_max=guess_max)

   ## The following chunck is about dealing with many rownames or colnames
   ## in gz compressed files
   ovcs <- Sys.getenv("VROOM_CONNECTION_SIZE")
   on.exit(Sys.setenv("VROOM_CONNECTION_SIZE"=ovcs))
   Sys.setenv(
      "VROOM_CONNECTION_SIZE"=max(
         c(
            utils::object.size(h$rownames), utils::object.size(h$colnames),
            as.numeric(ovcs)
         ),
         na.rm=TRUE
      )
   )
   ##

   td <- readr::read_delim(
      file, delim="\t",
      col_names=c("i", "j", "x"),
      col_types="iin",
      skip=h$header_length+skip, n_max=n_max
   )
   if(class=="dgCMatrix"){
      toRet <- Matrix::sparseMatrix(
         i=td$i, j=td$j, x=td$x,
         dimnames=list(h$rownames[1:max(td$i)], h$colnames[1:max(td$j)])
      )
   }
   if(class=="tibble"){
      toRet <- td
      attr(toRet, "header") <- h
   }
   return(toRet)
}
