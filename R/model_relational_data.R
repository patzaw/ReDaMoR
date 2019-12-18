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
         )),
         tags$script(
            '
            $(document).keyup(function(event) {
                if ($("#newTableName").is(":focus") * (event.key == "Enter")) {
                    $("#confirmAddTable").click();
                }
            });
            $(document).keyup(function(event) {
                if ((event.key == "Delete")) {
                    $("#removeTables").click();
                }
            });
            $(document).keyup(function(event) {
                if ((event.key == "Delete")) {
                    $("#removeMTables").click();
                }
            });
            '
         )
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
            uiOutput("editTable"),

         ## Multiple tables ----
            uiOutput("multiTables")

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
         tables=NULL,               # Selected tables
         fk=NULL                    # Selected foreign keys
      )


      #########################################################################@
      ## Model network ----
      #########################################################################@

      output$modelNet <- renderVisNetwork({
         replot$x
         selection$tables <- NULL
         selection$fk <- NULL
         plot(isolate(model$x), reproducible_layout=TRUE) %>%
            visEvents(
               release="function(nodes) {
                Shiny.onInputChange('modelNet_release', Math.random());
                ;}"
            )
      })

      observe({
         input$modelNet_release
         visNetworkProxy("modelNet") %>% visGetSelectedNodes()
         visNetworkProxy("modelNet") %>% visGetSelectedEdges()
         visNetworkProxy("modelNet") %>% visGetNodes()
      })

      observe({
         selection$tables <- intersect(
            input$modelNet_selectedNodes,
            names(model$x)
         )
      })

      observe({
         selection$fk <- intersect(
            input$modelNet_selectedEdges,
            modelToVn(model$x)$edges$id
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
         if(is.RelDataModel(mi)){
            model$toImport <- auto_layout(mi, lengthMultiplier=300)
         }else{
            model$toImport <- mi
         }
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
            mm <- lapply(
               mm,
               function(n){
                  i <- which(tin$nodes$id==n$tableName)
                  if(length(i)==1){
                     n$display$x=tin$nodes$x[i]
                     n$display$y=tin$nodes$y[i]
                  }
                  return(n)
               }
            )
            class(mm) <- c("RelDataModel", "list")
         }
         model$new <- mm
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
         tn <- isolate(input$newTableName)
         m <- isolate(model$x)
         if(!is.null(tn) && tn!="" && !tn %in% names(m)){
            m <- add_table(m, newTable=tn)
            m <- m %>% update_table_display(tn, px=0, py=0)
            model$new <- m
            removeModal()
         }
      })

      #########################################################################@
      ## Edit table ----
      #########################################################################@

      output$editTable <- renderUI({
         selTable <- selection$tables
         validate(need(selTable, ""))
         validate(need(length(selTable)==1, ""))
         div(
            fluidRow(
               column(8, h3(selTable)),
               column(
                  4,
                  actionButton(
                     "removeTables", "Remove",
                     icon=icon("trash-alt", "fa-2x")
                  ),
                  style="text-align:right;"
               )
            ),
            fluidRow(
               column(
                  4,
                  actionButton(
                     "addForeignKey1", "Add foreign key",
                     icon=icon("external-link-alt", "fa-2x")
                  ),
                  style="text-align:left;"
               )
            ),
            style=paste(
               "border:solid; border-radius:15px;",
               "min-height:85vh; padding:15px;"
            )
         )
      })

      #########################################################################@
      ## Multiple tables ----
      #########################################################################@

      output$multiTables <- renderUI({
         selTable <- selection$tables
         validate(need(selTable, ""))
         validate(need(length(selTable)>1, ""))
         toRet <- list(
            column(
               6,
               actionButton(
                  "removeMTables", "Remove",
                  icon=icon("trash-alt", "fa-2x")
               ),
               style="text-align:center;"
            )
         )
         if(length(selTable)==2){
            toRet <- c(list(
               column(
                  6,
                  actionButton(
                     "addForeignKey2", "Add foreign key",
                     icon=icon("external-link-alt", "fa-2x")
                  ),
                  style="text-align:center;"
               )
            ), toRet)
         }
         return(
            div(
               do.call(fluidRow, toRet),
               style=paste(
                  "border:solid; border-radius:15px;",
                  "padding:15px;"
               )
            )
         )
      })

      #########################################################################@
      ## Remove tables ----
      #########################################################################@

      observe({
         takeAction <- (
            (!is.null(input$removeTables) && input$removeTables > 0) ||
            (!is.null(input$removeMTables) && input$removeMTables > 0)
         )
         validate(need(takeAction, ""))
         tns <- isolate(selection$tables)
         m <- isolate(model$x)
         if(length(tns)>0 && all(tns!="") && all(tns %in% names(m))){
            for(tn in tns){
               m <- try(remove_table(m, tableName=tn), silent=TRUE)
               if(!is.RelDataModel(m)){
                  break()
               }
            }
            if(is.RelDataModel(m)){
               model$new <- m
            }else{
               showModal(modalDialog(
                  title="Unable to remove table",
                  p(
                     HTML(paste(
                        sprintf(
                           "%s is referenced by other tables.",
                           sprintf("<strong>%s</strong>", tn)
                        ),
                        "<br>Remove foreign keys before removing this table."
                     )),
                     style="color:red;"#font-weight: bold;"
                  ),
                  size="m",
                  easyClose=TRUE
               ))
            }
         }
      })

      #########################################################################@
      ## Add foreign keys ----
      #########################################################################@

      foreignKey <- reactiveValues(
         triggered=0,
         fromTable=NULL,
         toTable=NULL,
         fromFields=NULL,
         toFields=NULL
      )

      observe({
         validate(need(input$addForeignKey1 > 0, ""))
         tns <- isolate(selection$tables)
         m <- isolate(model$x)
         foreignKey$fromTable <- foreignKey$toTable <-
            foreignKey$fromFields <- foreignKey$toFields <- NULL
         foreignKey$triggered=isolate(foreignKey$triggered)+1
         if(length(tns)>0 && all(tns!="") && all(tns %in% names(m))){
            showModal(modalDialog(
               title="Add foreign key",
               uiOutput("addForeignKey"),
               size="l",
               easyClose=TRUE
            ))
         }
      })

      observe({
         validate(need(input$addForeignKey2 > 0, ""))
         tns <- isolate(selection$tables)
         m <- isolate(model$x)
         foreignKey$fromTable <- foreignKey$toTable <-
            foreignKey$fromFields <- foreignKey$toFields <- NULL
         foreignKey$triggered=isolate(foreignKey$triggered)+1
         if(length(tns)>0 && all(tns!="") && all(tns %in% names(m))){
            showModal(modalDialog(
               title="Add foreign key",
               uiOutput("addForeignKey"),
               size="l",
               easyClose=TRUE
            ))
         }
      })

      output$addForeignKey <- renderUI({
         tns <- selection$tables
         validate(need(length(tns)>0, ""))
         validate(need(foreignKey$triggered>0, ""))
         foreignKey$fromTable <- tns[1]
         foreignKey$toTable <- tns[length(tns)]
         div(
            fluidRow(
               column(5, h2(tns[1]), style="text-align:center;"),
               if(length(tns)==1){
                  column(
                     2,
                     actionButton("confirmAddFK", "Add", disabled=TRUE),
                     tags$br(),
                     icon("long-arrow-alt-right", "fa-2x"),
                     style="text-align:center;"
                  )
               }else{
                  column(
                     2,
                     actionButton("confirmAddFK", "Add", disabled=TRUE),
                     tags$br(),
                     actionButton(
                        "fkDirection", "", icon=icon("long-arrow-alt-right", "fa-2x")
                     ),
                     style="text-align:center;"
                  )
               },
               column(5, h2(tns[length(tns)]), style="text-align:center;"),
               style="border-bottom:solid;"
            ),
            fluidRow(
              uiOutput("fkFields"),
              style="border-bottom:solid; margin:15px; padding:15px;"
            ),
            fluidRow(
               uiOutput("possibleFkFields"),
               style="border-bottom:solid; margin:15px; padding:15px;"
            )
            # fluidRow(div(
            #    actionButton("confirmAddFK", "Add", disabled=TRUE),
            #    style="width:50px; margin:auto;"
            # ))
         )
      })

      observe({
         # print(foreignKey$fromTable)
         # print(foreignKey$toTable)
         # print(foreignKey$fromFields)
         # print(foreignKey$toFields)
         if(
            length(foreignKey$fromTable)==0 || length(foreignKey$toTable)==0 ||
            length(foreignKey$fromFields)==0 || length(foreignKey$toFields)==0
         ){
            # print("disable")
            disable("confirmAddFK")
         }else{
            # print("enable")
            enable("confirmAddFK")
         }
      })

      observe({
         validate(need(input$fkDirection>0, ""))
         tns <- isolate(selection$tables)
         validate(need(length(tns)==2, ""))
         ft <- isolate(foreignKey$toTable)
         tt <- isolate(foreignKey$fromTable)
         validate(need(ft, ""))
         validate(need(tt, ""))
         foreignKey$fromTable <- ft
         foreignKey$toTable <- tt
         foreignKey$fromFields <- foreignKey$toFields <- NULL
         if(ft==tns[1]){
            updateActionButton(
               session, "fkDirection",
               icon=icon("long-arrow-alt-right", "fa-2x")
            )
         }else{
            updateActionButton(
               session, "fkDirection",
               icon=icon("long-arrow-alt-left", "fa-2x")
            )
         }
      })

      output$possibleFkFields <- renderUI({
         tns <- isolate(selection$tables)
         ft <- foreignKey$fromTable
         tt <- foreignKey$toTable
         validate(need(ft, ""))
         validate(need(tt, ""))
         m <- isolate(model$x)
         ftfields <- m[[ft]]$fields$name
         ttfields <- m[[tt]]$fields$name
         toRet <- list(
            column(
               5,
               div(
                  selectInput(
                     "fkFromField", "", ftfields, multiple=FALSE, width="100%"
                  ),
                  style="width:300px; margin:auto;"
               )
            ),
            column(2, uiOutput("addFkFields"), style="text-align:center;"),
            column(
               5,
               div(
                  selectInput(
                     "fkToField", "", ttfields, multiple=FALSE, width="100%"
                  ),
                  style="width:300px; margin:auto;"
               )
            )
         )
         if(tns[1]!=ft){
            toRet <- toRet[c(3,2,1)]
         }
         return(toRet)
      })

      output$addFkFields <- renderUI({
         ft <- foreignKey$fromTable
         tt <- foreignKey$toTable
         validate(need(ft, ""))
         validate(need(tt, ""))
         m <- isolate(model$x)
         ftfields <- m[[ft]]$fields
         ttfields <- m[[tt]]$fields
         from <- input$fkFromField
         to <- input$fkToField
         validate(need(from %in% ftfields$name, ""))
         validate(need(to %in% ttfields$name, ""))
         if(
            ftfields[which(ftfields$name==from),]$type !=
            ttfields[which(ttfields$name==to),]$type
         ){
            return(list(tags$br(), p("Incompatible types", style="color:red;")))
         }else{
            selFrom <- foreignKey$fromFields
            selTo <- foreignKey$toFields
            alreadyIn <- length(which(selFrom==from & selTo==to))>0
            if(alreadyIn){
               return(list(tags$br(), p("Already in key", style="color:red;")))
            }else{
               return(list(tags$br(), actionButton(
                  "addFkField", label="",
                  icon=icon("plus-circle", "fa-2x")
               )))
            }
         }
      })

      observe({
         validate(need(input$addFkField>0, ""))
         foreignKey$fromFields <- c(
            isolate(foreignKey$fromFields), isolate(input$fkFromField)
         )
         foreignKey$toFields <- c(
            isolate(foreignKey$toFields), isolate(input$fkToField)
         )
      })

      output$fkFields <- renderUI({
         from <- foreignKey$fromFields
         to <- foreignKey$toFields
         validate(need(from, ""))
         validate(need(to, ""))
         div(
            DT::DTOutput("fkFieldTable"),
            div(
               uiOutput("rmFkField"),
               style="text-align:center;"
            ),
            style="width:50%; margin:auto"
         )
      })
      output$fkFieldTable <- DT::renderDT({
         from <- foreignKey$fromFields
         to <- foreignKey$toFields
         tns <- isolate(selection$tables)
         ft <- isolate(foreignKey$fromTable)
         tt <- isolate(foreignKey$toTable)
         validate(need(from, ""))
         validate(need(to, ""))
         if(tns[1]==ft){
            left <- from
            right <- to
         }else{
            left <- to
            right <- from
         }
         DT::datatable(
            tibble(l=left, m="", r=right),
            rownames=FALSE,
            colnames=c("", "", ""),
            options=list(
               dom=ifelse(length(left)>10, "tip", "t"),
               columnDefs = list(
                  list(targets=c(0), visible=TRUE, width='40%'),
                  list(targets=c(1), visible=TRUE, width='20%'),
                  list(targets=c(2), visible=TRUE, width='40%')
               )
            )
         ) %>%
            DT::formatStyle(c("l", "m", "r"), "text-align"="center")
      })
      output$rmFkField <- renderUI({
         sel <- input$fkFieldTable_rows_selected
         validate(need(sel, ""))
         return(
            actionButton(
               "confirmRmFkField",
               # label="",
               label=HTML(paste(
                  '<i class="fa fa-minus-circle fa-2x" style="color:red;">',
                  '</i>'
               ))
               # icon=icon("minus-circle", "fa-2x")
            )
         )
      })
      observe({
         validate(need(input$confirmRmFkField, ""))
         sel <- isolate(input$fkFieldTable_rows_selected)
         validate(need(length(sel)>0, ""))
         foreignKey$fromFields <- isolate(foreignKey$fromFields)[-sel]
         foreignKey$toFields <- isolate(foreignKey$toFields)[-sel]
      })

      observe({
         validate(need(input$confirmAddFK > 0, ""))
         model$new <- isolate(model$x) %>%
            add_foreign_key(
               fromTable=isolate(foreignKey$fromTable),
               toTable=isolate(foreignKey$toTable),
               fromFields=isolate(foreignKey$fromFields),
               toFields=isolate(foreignKey$toFields)
            )
         removeModal()
      })

      #########################################################################@
      ## Node positions ----
      #########################################################################@

      observe({
         dispNodes <- input$modelNet_nodes
         validate(need(dispNodes, ""))
         m <- isolate(model$x)
         m <- lapply(
            m,
            function(n){
               if(n$tableName %in% names(dispNodes)){
                  n$display$x=dispNodes[[n$tableName]]$x
                  n$display$y=dispNodes[[n$tableName]]$y
               }
               return(n)
            }
         )
         class(m) <- c("RelDataModel", "list")
         model$new <- m
      })

      #########################################################################@
      ## Update model ----
      #########################################################################@

      observe({
         nm <- model$new
         validate(need(nm, ""))
         dm <- isolate(model$x)

         ##
         tdm <- nm
         if(length(dm)==0){
            toReplot <- TRUE
         }else{
            toReplot <- FALSE
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
         }
         ##

         ch <- isolate(model$history)
         cm <- isolate(model$current)
         ch <- ch[-((cm:length(ch))+1)]
         ch <- c(ch, list(nm))
         cm <- cm+1
         model$history <- ch
         model$current <- cm
         model$x <- nm
         model$new <- NULL
         if(toReplot){
            replot$x <- isolate(replot$x)+1
         }
      })

      observe({
         selTables <- intersect(names(model$x), selection$tables)
         selection$tables <- selTables
         visNetworkProxy("modelNet") %>%
            visSelectNodes(selTables)
      })

      observe({
         selFK <- intersect(modelToVn(model$x)$edges$id, selection$fk)
         selection$fk <- selFK
         selTables <- intersect(names(model$x), selection$tables)
         if(length(selTables)==0){
            visNetworkProxy("modelNet") %>%
               visSelectEdges(selFK)
         }
      })

      observe({
         message("")
         print(selection$tables)
         print(selection$fk)
         message("")
      })

      #########################################################################@
      ## Manage history ----
      #########################################################################@

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

   modelInput <- auto_layout(modelInput, lengthMultiplier=300)

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
