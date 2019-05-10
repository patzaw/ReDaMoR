

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
            if(length(fk)==0){
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
