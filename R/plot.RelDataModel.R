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
plot.RelDataModel <- function(
   x,
   reproducible_layout=TRUE,
   ...
){

   toPlot <- modelToVn(x, ...)

   toShow <- visNetwork::visNetwork(nodes=toPlot$nodes, edges=toPlot$edges) %>%
      visNetwork::visNodes(
         labelHighlightBold=FALSE,
         borderWidth=2
      ) %>%
      visNetwork::visEdges(
         # color=list(
         #    color=border,
         #    highlight=highlightBorder
         # ),
         width=2,
         selectionWidth=2
      ) %>%
      visNetwork::visInteraction(multiselect=TRUE)
   if(reproducible_layout){
      toShow <- toShow %>%
         ################################################@
         ## The code below is useless when edge smooth is
         ## define by edge
         # visNetwork::visPhysics(
         #    solver="repulsion",
         #    repulsion=list(
         #       nodeDistance=100,
         #       springLength=100,
         #       springConstant=0.001,
         #       damping=1,
         #       avoidOverlap=1
         #    )
         # ) %>%
         ################################################@
         visNetwork::visLayout(randomSeed=2) #%>%
   }
   toShow %>% visPhysics(enabled=FALSE)

}

###############################################################################@
#' VisNetwork representation of a [RelDataModel] object
#'
#' Internal function
#'
modelToVn <- function(
   model,
   border="black",
   color="lightgrey",
   highlightBorder="orange"
){
   nodes <- do.call(rbind, lapply(
      model,
      function(m){
         f <- m$fields
         pk <- m$primaryKey
         it <- index_table(m)
         ind <- NULL
         # uq <- NULL
         if(!is.null(it)){
            it <- it %>% filter(index!=0)
            ind <- unique(it$field)
            # uq <- unique(it$field[which(it$unique)])
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
            ifelse(f$unique & !f$name %in% pk, "*", ""),
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
            sprintf(
               '<b>%s</b>',
               m$tableName
            ),
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
            sprintf(
               '<b>%s</b>%s',
               m$tableName,
               ifelse(
                  is.na(m$display$comment), "",
                  sprintf(" (%s)", m$display$comment)
               )
            ),
            ftit,
            sep="<br>"
         )
         return(tibble(
            tableName=m$tableName,
            label=label,
            title=title,
            shape="box",
            font.multi=TRUE,
            font.align="left",
            x=m$display$x,
            y=m$display$y,
            color.background=m$display$color
         ))
      }
   ))
   if(!is.null(nodes) && nrow(nodes)>0){
      nodes <- nodes %>%
         mutate(
            color.border=!!border,
            color.highlight.border=!!highlightBorder,
            color.background=ifelse(
               is.na(color.background), !!color, color.background
            )
         ) %>%
         mutate(
            color.highlight.background=color.background
         )
      nodes$id <- names(model)
   }

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
               kt <- k$key %>% arrange(from, to)
               title <- paste0(
                  '<tr style="border: 1px solid black">',
                  '<td style="border: 1px solid black">', kt,
                  '</td>',
                  '<td style="border: 1px solid black">', kt,
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
               id <- paste(kt$from, kt$to, sep="->")
               id <- paste(id, collapse=" && ")
               id <- paste(to, id, sep=": ")
               return(tibble(id=id, to=to, title=title))
            }
         ))
         toRet$from <- mn
         toRet$id <- paste(mn, toRet$id, sep="->")
         toRet$arrows <- "to"
         toRet$font.align <- "bottom"
         return(toRet)
      }
   ))
   if(is.null(edges)){
      edges <- tibble(id=character(), from=character(), to=character())
   }else{
      edges$smooth.type <- "curvedCW"
      edges$smooth.roundness <- 0
      edges$ue <- edges %>% select(from, to) %>%
         apply(1, function(x) paste(sort(x), collapse="<->"))
      edges <- edges %>%
         group_by(ue) %>%
         mutate(
            smooth.roundness={
               mr <- min(1, 0.2*(length(ue)%/%2))
               seq(-mr, mr, length.out=length(ue))
            }
         ) %>%
         ungroup() %>%
         mutate(
            color.color=border,
            color.highlight=highlightBorder
         )
   }

   return(list(nodes=nodes, edges=edges))
}
