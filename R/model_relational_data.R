###############################################################################@
buildUi <- function(fromR){
   shinyUI(fluidPage(
      title="ReDaMoR",

      ## Settings ----
      useShinyjs(),

      ## HEAD ----
      tags$head(
         tags$link(
            rel="icon",
            href=paste(
               "data:image/png;base64,",
               base64enc::base64encode(system.file(
                  "www/ReDaMoR.png",
                  package = packageName()
               ))
            )
         ),
         includeCSS(system.file(
            "www/cerulean.css",
            package = packageName()
         )),
         includeCSS(system.file(
            "www/defChanges.css",
            package = packageName()
         ))
      ),

      ## Main menu ----
      fluidRow(
         column(
            2,
            if(fromR){
               actionButton(
                  "done",
                  list(icon("check", "fa-2x"), "Done"),
                  style="margin-top:25px;margin-bottom:25px;"
               )
            }else{
               img(
                  src=paste(
                     "data:image/png;base64,",
                     base64enc::base64encode(system.file(
                        "www/ReDaMoR.png",
                        package = packageName()
                     ))
                  ),
                  height="100px", style="margin-bottom:10px;margin-top:5px;"
               )
            }
         ),
         column(
            2,
            actionButton(
               "import",
               list(icon("file-import", "fa-2x"), "Import"),
               style="margin-top:25px;margin-bottom:25px;"
            )
         ),
         column(
            2,
            actionButton(
               "export",
               list("Export", icon("file-export", "fa-2x")),
               style="margin-top:25px;margin-bottom:25px;"
            )
         ),
         column(
            3,
            actionButton(
               "undo",
               list("Undo", icon("undo", "fa-2x")),
               style="margin-top:25px;margin-bottom:25px;"
            ),
            actionButton(
               "redo",
               list(icon("redo", "fa-2x"), "Redo"),
               style="margin-top:25px;margin-bottom:25px;"
            )
         ),
         column(
            2,
            actionButton(
               "addTable", "Add table",
               icon=icon("plus-square", "fa-2x"),
               style="margin-top:25px;margin-bottom:25px;"
            )
         ),
         column(
            1,
            actionButton(
               "doc", "", icon=icon("question-circle", "fa-2x"),
               style="margin-top:25px;margin-bottom:25px;"
            )
         )
      ),

      fluidRow(

         ## Model view ----
         column(
            7,
            div(
               visNetworkOutput("modelNet", height="85vh", width="100%"),
               style="border:solid; min-height:85vh;"
            )
         ),

         ## Edit table ----
         column(
            5,
            uiOutput("editTable")
         )
      )


   ))
}

###############################################################################@
buildServer <- function(modelInput, fromR, bcko){

   function(input, output, session) {

      #########################################################################@
      ## The model ----
      #########################################################################@

      model <- reactiveValues(
         x=modelInput,              # The current model
         new=NULL,                  # A new model to add in history
         history=list(modelInput),  # The model history
         current=1,                 # The position of current model in history
         toImport=NULL,             # Model to import from file
         merged=NULL                # merge: c(x, toImport)
      )
      replot <- reactiveValues(
         x=1                        # Replot the model
      )
      selection <- reactiveValues(
         tables=NULL
      )


      #########################################################################@
      ## Model network ----
      #########################################################################@

      output$modelNet <- renderVisNetwork({
         replot$x
         selection$tables <-NULL
         plot(isolate(model$x), reproducible_layout=TRUE) %>%
            visEvents(
               release = "function(nodes) {
                Shiny.onInputChange('modelNet_release', Math.random());
                ;}"
            )
      })

      observe({
         input$modelNet_release
         visNetworkProxy("modelNet") %>% visGetSelectedNodes()
         visNetworkProxy("modelNet") %>% visGetNodes()
      })

      observe({
         replot$new
         visNetworkProxy("modelNet") %>% visGetSelectedNodes()
      })

      observe({
         selection$tables <- intersect(
            input$modelNet_selectedNodes,
            names(model$x)
         )
      })

      #########################################################################@
      ## Import model ----
      #########################################################################@

      observeEvent(input$import, {
         showModal(modalDialog(
            title="Import",
            uiOutput("import"),
            size="l",
            easyClose=TRUE
         ))
      })

      output$import <- renderUI({
         list(
            fileInput(
               "impModel", "Choose an sql or a json file",
                multiple=FALSE,
                accept=c(".sql", ".json", ".sql.gz", ".json.gz")
            ),
            uiOutput("impModel")
         )
      })

      output$impModel <- renderUI({
         mi <- model$toImport
         validate(need(!is.null(mi), ""))
         if(!is.RelDataModel(mi)){
            list(p(mi, style="color:red;font-weight: bold;"))
         }else{

         list(
            div(
               visNetworkOutput("impModelNet", height="65vh", width="100%"),
               style="border:solid; min-height:65vh;"
            ),
            uiOutput("impMessage")
         )

         }

      })

      output$impModelNet <- renderVisNetwork({
         mi <- model$toImport
         validate(need(mi, ""))
         plot(mi)
      })

      observe({
         mi <- model$toImport
         validate(need(mi, ""))
         m <- isolate(model$x)
         mm <- try(c(m, mi), silent=TRUE)
         model$merged <- mm
      })

      output$impMessage <- renderUI({
         mm <- model$merged
         if(is.RelDataModel(mm)){
            actionButton(
               "importValidate",
               list(icon("file-import", "fa-2x"), "Merge with current model"),
               style="margin-top:25px;margin-bottom:25px;"
            )
         }else{
            list(p(mm, style="color:red;font-weight: bold;"))
         }
      })

      observe({
         fi <- input$impModel
         validate(need(fi, ""))
         fiext <- regexpr(
            "(\\.[[:alnum:]]+)(\\.gz)?$", fi$name, ignore.case=TRUE
         )
         fiext <- substr(
            fi$name, fiext, fiext+attr(fiext, "match.length")-1
         ) %>% tolower()
         mi <- NULL
         if(fiext %in% c(".sql", ".sql.gz")){
            mi <- suppressWarnings(try(
               read_SQL_data_model(fi$datapath),
               silent=TRUE
            ))
         }
         if(fiext %in% c(".json", ".json.gz")){
            mi <- suppressWarnings(try(
               read_json_data_model(fi$datapath),
               silent=TRUE
            ))
         }
         model$toImport <- mi
      })

      observe({
         validate(need(input$importValidate, ""))
         mm <- isolate(model$merged)
         validate(need(mm, ""))

         cmn <- modelToVn(isolate(model$x))
         if(
            !is.null(cmn$nodes)>0 && nrow(cmn$nodes)>0 &&
            all(!is.na(cmn$nodes$x)) && all(!is.na(cmn$nodes$y))
         ){
            toReplot <- FALSE
            cmxrange <- c(min(cmn$nodes$x), max(cmn$nodes$x))
            cmyrange <- c(min(cmn$nodes$x), max(cmn$nodes$y))
            validate(need(isolate(model$toImport), ""))
            tin <- modelToVn(isolate(model$toImport))
            if(any(is.na(tin$nodes$x)) || any(is.na(tin$nodes$y))){
               tin$nodes$x <- runif(
                  nrow(tin$nodes), min=cmxrange[1], max=cmxrange[2]
               )
               tin$nodes$y <- runif(
                  nrow(tin$nodes), min=cmyrange[1], max=cmyrange[2]
               )
            }
            tixrange <- c(min(tin$nodes$x), max(tin$nodes$x))
            tiyrange <- c(min(tin$nodes$x), max(tin$nodes$y))
            xshift <- cmxrange[2]-tixrange[1]
            yshift <- cmyrange[2]-tiyrange[1]
            tin$nodes$x <- tin$nodes$x+xshift
            tin$nodes$y <- tin$nodes$y+yshift
            for(i in 1:nrow(tin$nodes)){
               mm <- mm %>%
                  update_table_display(
                     tableName=tin$nodes$id[i],
                     px=tin$nodes$x[i], py=tin$nodes$y[i]
                  )
            }
            visNetworkProxy("modelNet") %>%
               visUpdateNodes(tin$nodes) %>%
               visUpdateEdges(tin$edges) %>%
               visFit()
         }else{
            toReplot <- TRUE
         }

         model$new <- mm
         if(toReplot){
            replot$x <- isolate(replot$x)+1
         }
         model$merged <- NULL
         model$toImport <- NULL
         removeModal()
      })

      #########################################################################@
      ## Add table ----
      #########################################################################@

      observeEvent(input$addTable, {
         showModal(modalDialog(
            title="Add table",
            uiOutput("addTable"),
            size="m",
            easyClose=TRUE
         ))
      })

      output$addTable <- renderUI({
         list(
            fluidRow(
               column(10, textInput("newTableName", "Name", width="100%")),
               column(2, actionButton("confirmAddTable", "Add"))
            ),
            fluidRow(uiOutput("newTableExists"))
         )
      })

      output$newTableExists <- renderUI({
         ntn <- input$newTableName
         m <- isolate(model$x)
         if(ntn %in% names(m)){
            p("Table name already used", style="color:red;font-weight: bold;")
         }else{
            list()
         }
      })

      observe({
         ntn <- input$newTableName
         m <- isolate(model$x)
         if(is.null(ntn) || ntn=="" || ntn %in% names(m)){
            disable("confirmAddTable")
         }else{
            enable("confirmAddTable")
         }
      })

      observe({
         validate(need(input$confirmAddTable, ""))
         m <- add_table(
            isolate(model$x), newTable=isolate(input$newTableName)
         )
         model$new <- m
         if(length(isolate(model$x))>0){
            toReplot <- FALSE
            mn <- modelToVn(m)
            visNetworkProxy("modelNet") %>%
               visUpdateNodes(mn$nodes) %>%
               visUpdateEdges(mn$edges) %>%
               visFit()
         }else{
            toReplot <- TRUE
         }
         if(toReplot){
            replot$x <- isolate(replot$x)+1
         }
         removeModal()
      })

      #########################################################################@
      ## Edit table ----
      #########################################################################@

      output$editTable <- renderUI({
         selTable <- selection$tables
         validate(need(selTable, ""))
         validate(need(length(selTable)==1, ""))
         h1(selTable)
      })

      #########################################################################@
      ## Node positions ----
      #########################################################################@

      observe({
         dispNodes <- input$modelNet_nodes
         validate(need(dispNodes, ""))
         m <- isolate(model$x)
         for(tn in names(dispNodes)){
            m <- m %>% update_table_display(
               tn,
               px=dispNodes[[tn]]$x,
               py=dispNodes[[tn]]$y
            )
         }
         model$new <- m
      })

      #########################################################################@
      ## History ----
      #########################################################################@

      observe({
         nm <- model$new
         validate(need(nm, ""))
         ch <- isolate(model$history)
         cm <- isolate(model$current)
         ch <- ch[-((cm:length(ch))+1)]
         ch <- c(ch, list(nm))
         cm <- cm+1
         model$history <- ch
         model$current <- cm
         model$x <- nm
      })

      observe({
         validate(need(input$undo, ""))
         ch <- isolate(model$history)
         cm <- isolate(model$current)
         cm <- cm -1
         validate(need(cm>0, ""))

         ###########################@
         ## The commented code below was first used to
         ## replot the network from scratch
         # model$x <- ch[[cm]]
         # model$current <- cm
         # replot$x <- isolate(replot$x)+1
         ###########################@

         dm <- isolate(model$x)
         tdm <- ch[[cm]]
         ndm <- modelToVn(dm)
         ntdm <- modelToVn(tdm)
         edgeToDel <- setdiff(ndm$edges$id, ntdm$edges$id)
         if(length(edgeToDel)>0){
            visNetworkProxy("modelNet") %>%
               visRemoveEdges(edgeToDel)
         }
         nodeToDel <- setdiff(names(dm), names(tdm))
         if(length(nodeToDel)>0){
            visNetworkProxy("modelNet") %>%
               visRemoveNodes(nodeToDel)
         }
         visNetworkProxy("modelNet") %>%
            visUpdateNodes(ntdm$nodes) %>%
            visUpdateEdges(ntdm$edges)
         model$x <- tdm
         model$current <- cm
         selection$tables <- intersect(isolate(selection$tables), names(tdm))
      })

      observe({
         validate(need(input$redo, ""))
         ch <- isolate(model$history)
         cm <- isolate(model$current)
         cm <- cm +1
         validate(need(cm<=length(ch), ""))

         ###########################@
         ## The commented code below was first used to
         ## replot the network from scratch
         # model$x <- ch[[cm]]
         # model$current <- cm
         # replot$x <- isolate(replot$x)+1
         ###########################@

         dm <- isolate(model$x)
         tdm <- ch[[cm]]
         ndm <- modelToVn(dm)
         ntdm <- modelToVn(tdm)
         edgeToDel <- setdiff(ndm$edges$id, ntdm$edges$id)
         if(length(edgeToDel)>0){
            visNetworkProxy("modelNet") %>%
               visRemoveEdges(edgeToDel)
         }
         nodeToDel <- setdiff(names(dm), names(tdm))
         if(length(nodeToDel)>0){
            visNetworkProxy("modelNet") %>%
               visRemoveNodes(nodeToDel)
         }
         visNetworkProxy("modelNet") %>%
            visUpdateNodes(ntdm$nodes) %>%
            visUpdateEdges(ntdm$edges)
         model$x <- tdm
         model$current <- cm
         selection$tables <- intersect(isolate(selection$tables), names(tdm))
      })

      observe({
         if(model$current==1){
            disable("undo")
         }
         if(model$current > 1){
            enable("undo")
         }
         if(model$current >= length(model$history)){
            disable("redo")
         }
         if(model$current < length(model$history)){
            enable("redo")
         }
      })

      #########################################################################@
      ## Export model ----
      #########################################################################@

      observeEvent(input$export, {
         showModal(modalDialog(
            title="Import",
            uiOutput("export"),
            size="s",
            easyClose=TRUE
         ))
      })

      output$export <- renderUI({
         fluidRow(
            column(6, downloadButton(
               "exportJson",
               list(icon("file-code", "fa-2x"), "JSON"),
               style="margin-top:25px;margin-bottom:25px;"
            )),
            column(6, downloadButton(
               "exportHtml",
               list(icon("map", "fa-2x"), "HTML"),
               style="margin-top:25px;margin-bottom:25px;"
            ))
         )
      })

      ## _+ JSON ----
      output$exportJson <- downloadHandler(
         filename = function() {
            paste0("Data-model", ".json")
         },
         content = function(file) {
            m <- isolate(model$x)
            validate(need(m, ""))
            write_json_data_model(m, file)
         }
      )

      ## _+ HTML ----
      output$exportHtml <- downloadHandler(
         filename = function() {
            paste0("Data-model", ".html")
         },
         content = function(file) {
            m <- isolate(model$x)
            validate(need(m, ""))
            plot(m, reproducible_layout=TRUE) %>% visSave(file)
         }
      )

      #########################################################################@
      ## From R ----
      #########################################################################@

      if(fromR){
         ## _+ Cache object ----
         observe({
            assign(bcko, model$x, envir=.GlobalEnv)
         })
         ## _+ Done button ----
         observeEvent(input$done, {
            stopApp(invisible(model$x))
         })
      }

   }

}

###############################################################################@
#' Relational data modeler GUI
#'
#' @param modelInput the [RelDataModel] to start from
#' @param fromR a logical indicating if the application is launched from R
#'
#' @export
#'
model_relational_data <- function(
   modelInput=RelDataModel(list()), fromR=interactive()
){

   bcko <- paste(
      c(".RelDataModel_", sample(letters, 10, replace=TRUE)), collapse=""
   )
   while(exists(bcko, where=.GlobalEnv)){
      bcko <- paste(
         c(".RelDataModel_", sample(letters, 10, replace=TRUE)), collapse=""
      )
   }
   ui <- buildUi(fromR=fromR)
   server <- buildServer(modelInput=modelInput, fromR=fromR, bcko=bcko)

   if(fromR){
      ## From R ----
      on.exit(
         message(
            sprintf(
               "Your model has been saved in the '%s' object in .GlobalEnv",
               bcko
            ),
            "\n Use `list_RelDataModel_in_cache()`",
            " and `clean_RelDataModel_in_cache()`",
            " to list and remove them respectively."
         )
      )
      runApp(shinyApp(ui, server))
   }else{
      ## Remote app ----
      shinyApp(ui, server)
   }

}

###############################################################################@
#' List cached [RelDataModel] in .GlobalEnv
#'
#' @export
#'
list_RelDataModel_in_cache <- function(){
   ls(envir=.GlobalEnv, all.names=TRUE ) %>%
      grep("[.]RelDataModel_", ., value=TRUE)
}

###############################################################################@
#' Remove all cached [RelDataModel] from .GlobalEnv
#'
#' @export
#'
clean_RelDataModel_in_cache <- function(){
   rm(list=list_RelDataModel_in_cache(), envir=.GlobalEnv)
}
