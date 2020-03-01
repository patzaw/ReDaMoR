###############################################################################@
#' Plot a [RelDataModel] object
#'
#' This function draw a visNetwork of the [RelDataModel].
#'
#' @param x a [RelDataModel]
#' @param ... additional parameters:
#' - **color** default table background color
#' - **border** border color (single character)
#' - **highlightBorder** color of highlighted borders
#'
#' @example inst/examples/ex_plot_model.R
#'
#' @importFrom graphics plot
#' @import visNetwork
#'
#' @export
#'
plot.RelDataModel <- function(
   x,
   ...
){

   toPlot <- modelToVn(x, ...)

   visNetwork::visNetwork(nodes=toPlot$nodes, edges=toPlot$edges) %>%
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
      visNetwork::visInteraction(multiselect=TRUE) %>%
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
      visNetwork::visLayout(randomSeed=2) %>%
      visPhysics(enabled=FALSE)

}

###############################################################################@
#' VisNetwork representation of a [RelDataModel] object
#'
#' @param model a [RelDataModel]
#' @param color default table background color
#' @param border border color (single character)
#' @param highlightBorder color of highlighted borders
#'
#'
#' Internal function
#'
modelToVn <- function(
   model,
   color="lightgrey",
   border="black",
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
            it <- it %>% filter(.data$index!=0)
            ind <- unique(it$field)
            uind <- it %>% filter(.data$uniqueIndex) %>%
               pull("field") %>%
               unique()
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
            # ifelse(f$unique & !f$name %in% pk, "*", ""),
            ifelse(f$unique, "*", ""),
            f$name,
            ifelse(f$name %in% pk, "</b>", ""),
            f$type,
            ifelse(
               f$name %in% ind,
               ifelse(
                  f$name %in% uind,
                  paste0(" - uidx.", f$i),
                  paste0(" - idx.", f$i)
               ),
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
            '<li><strong>%s</strong>%s%s</li>',
            f$name,
            ifelse(is.na(f$comment)|f$comment=="", "", ": "),
            ifelse(is.na(f$comment)|f$comment=="", "", f$comment)
         ), collapse=" ")
         title <- paste(
            sprintf(
               '<p><strong style="text-decoration:underline;">%s</strong>%s</p>',
               m$tableName,
               ifelse(
                  is.na(m$display$comment), "",
                  sprintf(" (%s)", m$display$comment)
               )
            ),
            "<ul>", ftit, "</ul>",
            sep=" "
         )
         title <- sprintf(
            paste0(
               '<div ',
               'style="',
               'max-width:400px; max-height:300px;',
               'overflow:scroll;',
               '">%s</div>'
            ),
            title
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
               is.na(.data$color.background), !!color, .data$color.background
            )
         ) %>%
         mutate(
            color.highlight.background=.data$color.background
         )
      nodes$id <- names(model)
   }

   edges <- do.call(rbind, lapply(
      model,
      function(m){
         mt <- m$tableName
         fk <- m$foreignKeys
         if(length(fk)==0){
            return(NULL)
         }
         toRet <- do.call(rbind, lapply(
            fk,
            function(k){
               to <- k$refTable
               kt <- k$key %>% arrange(.data$from, .data$to)
               kcard <- ifelse(k$cardinality==-1, "n", k$cardinality)
               fcard <- paste(kcard["fmin"], kcard["fmax"], sep="..")
               tcard <- paste(kcard["tmin"], kcard["tmax"], sep="..")
               title <- paste0(
                  '<tr style="border: 1px solid black; padding:1px;">',
                  '<td style="border: 1px solid black; padding:1px;">',
                     c(kt$from),
                  '</td>',
                  '<td style="border: 1px solid black; padding:1px;">',
                     c(kt$to),
                  '</td>',
                  '</tr>'
               )
               title <- paste0(
                  '<table style="border: 1px solid black; padding:1px;">',
                  '<tr style="border: 1px solid black; padding:1px;">',
                  sprintf(
                     paste0(
                        '<th style=',
                        '"border: 1px solid black; padding:1px;',
                        'text-align:center;"',
                        '>',
                        '%s<br>(%s)',
                        '</th>'
                     ),
                     mt, fcard
                  ),
                  sprintf(
                     paste0(
                        '<th style=',
                        '"border: 1px solid black; padding:1px;',
                        'text-align:center;"',
                        '>',
                        '%s<br>(%s)',
                        '</th>'
                     ),
                     k$refTable, tcard
                  ),
                  '</tr>',
                  paste(title, collapse=""),
                  '</table>'
               )
               id <- paste(kt$from, kt$to, sep="->")
               id <- paste(id, collapse=" && ")
               id <- paste(to, id, sep=": ")
               return(tibble(
                  id=id, to=to, title=title,
                  ff=list(kt$from), tf=list(kt$to)
               ))
            }
         ))
         toRet$from <- mt
         toRet$id <- paste(mt, toRet$id, sep="->")
         toRet$arrows <- "to"
         toRet$font.align <- "bottom"
         return(toRet)
      }
   ))
   if(is.null(edges)){
      edges <- tibble(id=character(), from=character(), to=character())
   }else{
      edges$smooth.type <- "curvedCCW"
      edges$smooth.roundness <- 0
      edges$selfReferenceSize  <- 30
      edges <- bind_cols(edges, edges %>% select("from", "to") %>%
         apply(1, function(x) c(sort(x), paste(sort(x), collapse="<->"))) %>%
         t() %>%
         magrittr::set_colnames(c("uef", "uet", "ue")) %>%
         as_tibble()
      )
      edges <- edges %>%
         group_by(.data$ue) %>%
         mutate(
            smooth.roundness={
               mr <- min(1, 0.2*(length(.data$ue)%/%2))
               seq(-mr, mr, length.out=length(.data$ue))
            },
            selfReferenceSize={
               seq(30, 50, length.out=length(.data$ue))
            }
         ) %>%
         ungroup() %>%
         mutate(
            smooth.roundness=ifelse(
               .data$uef==.data$from,
               .data$smooth.roundness,
               -.data$smooth.roundness
            )
         ) %>%
         mutate(
            color.color=border,
            color.highlight=highlightBorder
         )
   }

   return(list(nodes=nodes, edges=edges))
}
