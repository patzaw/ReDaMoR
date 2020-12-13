###############################################################################@
buildUi <- function(fromR){

   addResourcePath(
      "www",
      system.file("www", package=utils::packageName())
   )
   addResourcePath(
      "doc",
      system.file("doc", package=utils::packageName())
   )

   shinyUI(fluidPage(
      title="ReDaMoR",
      windowTitle="ReDaMoR",
      id="MainApp",

      ## Settings ----
      shinyjs::useShinyjs(),
      rintrojs::introjsUI(),

      ## HEAD ----
      tags$head(
         tags$link(
            rel="icon",
            href='www/ReDaMoR.png'
         ),
         tags$link(
            rel="stylesheet", type="text/css", href="www/cerulean.css"
         ),
         tags$link(
            rel="stylesheet", type="text/css", href="www/defChanges.css"
         ),
         tags$link(
            rel="stylesheet", type="text/css", href="www/appElements.css"
         ),
         tags$script(src='www/interactions.js'),
         if(fromR) NULL else tags$script(src='www/fromWeb.js')
      ),

      ## Main menu ----
      fluidRow(
         id="mainMenu",
         column(
            11,
            id="mainLColumn",
            div(
               id="mainDone",
               if(fromR){
                  actionButton(
                     "done",
                     list(icon("check", "fa-2x"), "Done")
                  ) %>%
                     div(title="Return the model in R session")
               }else{
                  img(src='www/ReDaMoR.png', id="mainLogo")
               }
            ),
            div(
               class="mainButton",
               actionButton(
                  "import",
                  list(icon("file-import", "fa-2x"), "Import")
               ),
               actionButton(
                  "export",
                  list("Export", icon("file-export", "fa-2x"))
               )
            ),
            div(
               class="mainButton",
               actionButton(
                  "undo",
                  list("Undo", icon("undo", "fa-2x"))
               ),
               actionButton(
                  "redo",
                  list(icon("redo", "fa-2x"), "Redo")
               ),
               title="Undo (Ctrl+Z) / Redo (Ctrl+Shift+Z)"
            ),
            div(
               class="mainButton",
               actionButton(
                  "addTable", "Add table",
                  icon=icon("plus-square", "fa-2x")
               )
            ),
            div(
               class="mainButton",
               uiOutput("modelSummary")
            ),
            div(
               class="mainButton",
               actionButton(
                  "doc", "", icon=icon("question-circle", "fa-2x")
               ) %>% div(title="Help tour")
            )
         ),
         column(
            1,
            id="mainRColumn",
            tags$a(
               "About ReDaMoR",
               href="doc/ReDaMoR.html",
               target="_blank",
               id="about"
            )
         )
      ),

      fluidRow(

         ## Model view ----
         column(
            7,
            fluidRow(
               id="viewMenu",
               column(
                  8,
                  id="findTableDiv",
                  selectInput(
                     "findTable",
                     label=NULL,
                     choices=NULL,
                     multiple=TRUE,
                     selected=NULL,
                     width="100%"
                  )
               ),
               column(
                  4,
                  id="viewButtons",
                  div(
                     class="viewButton",
                     actionButton(
                        "selectAll",
                        label=NULL,
                        icon=icon("object-group", "fa-2x"),
                        class="shrunkenButton"
                     ) %>% div(title="Select all tables")
                  ),
                  div(
                     class="viewButton",
                     actionButton(
                        "autoLayout",
                        label=NULL,
                        icon=icon("pencil-ruler", "fa-2x"),
                        class="shrunkenButton"
                     ) %>% div(title="Auto layout the model")
                  ),
                  div(
                     class="viewButton",
                     actionButton(
                        "fitNet",
                        label=NULL,
                        icon=icon("vector-square", "fa-2x"),
                        class="shrunkenButton"
                     ) %>% div(title="Fit model")
                  )
               )
            ),
            fluidRow(
               id="modelFrame",
               visNetworkOutput("modelNet", height="75vh", width="100%")
            )
         ),

         column(
            5,

            ## Edit menu ----
            div(
               id="editMenu",
               uiOutput(
                  "setTableColor",
                  class="editMenuSection"
               ),
               uiOutput(
                  "dupTablesInput",
                  class="editMenuSection"
               ),
               uiOutput(
                  "rmTablesInput",
                  class="editMenuSection"
               ),
               uiOutput(
                  "addFKInput",
                  class="editMenuSection"
               ),
               uiOutput(
                  "editFKInput",
                  class="editMenuSection"
               ),
               uiOutput(
                  "rmFKInput",
                  class="editMenuSection"
               )
            ),

            ## Edit table ----
            uiOutput("editTable")

         )
      )


   ))
}

###############################################################################@
buildServer <- function(
   modelInput, fromR, bcko,
   defaultColor, availableColors,
   example, forceIntro
){

   rintrosteps <- jsonlite::fromJSON(system.file(
      "Documentation/rintrojs-steps.json",
      package = utils::packageName()
   )) %>% lapply(
      function(x){
         toRet <- as_tibble(x) %>%
            select("element", "intro")
         if(!fromR){
            toRet <- toRet %>%
               filter(is.na(.data$element) | .data$element!="#done")
         }
         return(toRet)
      }
   )


   function(input, output, session) {

      #########################################################################@
      ## Help tour ----
      #########################################################################@

      context <- reactiveValues(
         x="main"
      )
      observe(
         if(length(selection$tables)==0 && length(selection$fk)==0){
            context$x <- "main"
         }else{
            ctxt <- c()
            if(length(selection$tables) > 0){
               ctxt <- c(ctxt, "tables")
               if(length(selection$tables)==2){
                  ctxt <- c(ctxt, "twoTables")
               }
               if(length(selection$tables)==1){
                  ctxt <- c(ctxt, "oneTable")
                  if(nrow(model$x[[selection$tables]]$fields)>0){
                     ctxt <- c(ctxt, "withFields")
                  }
                  if(length(model$x[[selection$tables]]$indexes)>0){
                     ctxt <- c(ctxt, "withIndexes")
                  }
                  if(length(input$fieldTable_rows_selected)>0){
                     ctxt <- c(ctxt, "selectedField")
                  }
                  if(length(input$indexTable_rows_selected)>0){
                     ctxt <- c(ctxt, "selectedIndex")
                  }
               }
            }
            if(length(selection$fk)>0){
               ctxt <- c(ctxt, "withFK")
               if(length(selection$fk)==1){
                  ctxt <- c(ctxt, "oneFK")
               }
            }
            context$x <- ctxt
         }
      )
      observeEvent(input$doc, {
         docx <- do.call(rbind, rintrosteps[context$x])
         rintrojs::introjs(session, options = list(steps=docx))
      })
      observeEvent(input$docImp, {
         docx <- do.call(rbind, rintrosteps[c("Import")])
         rintrojs::introjs(session, options = list(steps=docx))
      })

      if(forceIntro){
         rintrojs::introjs(session, options = list(steps=rintrosteps[["main"]]))
      }

      #########################################################################@
      ## Settings ----
      #########################################################################@

      settings <- reactiveValues()
      settings$defaultColor <- defaultColor
      settings$availableColors <- availableColors
      observe({
         settings$availableColors <- unique(c(
            settings$defaultColor,
            lapply(model$x, function(x) x$display$color) %>%
               unlist() %>% setdiff(NA),
            isolate(settings$availableColors)
         ))
      })

      #########################################################################@
      ## The model ----
      #########################################################################@

      model <- reactiveValues(
         x=modelInput,              # The current model
         vn=modelToVn(modelInput),  # VisNet representation
         new=NULL,                  # A new model to add in history
         history=list(modelInput),  # The model history
         current=1,                 # The position of current model in history
         toImport=NULL,             # Model to import from file
         merged=NULL,               # merge: c(x, toImport)
         table=NULL,                # The table to edit
         indexTable=tibble(         # Indexes of the table to edit
            fields=character(),
            unique=logical()
         ),
         fieldTable=tibble(         # Fields of the table to edit
            name=character(),
            type=character(),
            nullable=logical(),
            unique=logical(),
            comment=character()
         )
      )
      observe(
         model$vn <- modelToVn(model$x, color=isolate(settings$defaultColor))
      )

      replot <- reactiveValues(
         x=1                        # Used for triggering model re-plot
      )
      selection <- reactiveValues(
         release=0,                 # Used for refreshing the visNetwork
         tables=NULL,               # Selected tables
         fk=NULL,                   # Selected foreign keys
         fromVN=FALSE               # Used for refreshing the visNetwork
      )

      #########################################################################@
      ## Notifications ----
      #########################################################################@

      warningMessage <- reactiveValues(
         n=0,
         message=NULL
      )
      sendWarning <- function(message){
         warningMessage$n <- isolate(warningMessage$n) + 1
         warningMessage$message <- message
      }
      observe({
         validate(need(warningMessage$n>0, ""))
         showNotification(
            isolate(warningMessage$message),
            duration=5,
            type="warning"
         )
      })

      errorMessage <- reactiveValues(
         n=0,
         message=NULL
      )
      sendError <- function(message){
         errorMessage$n <- isolate(errorMessage$n) + 1
         errorMessage$message <- message
      }
      observe({
         validate(need(errorMessage$n>0, ""))
         showNotification(
            isolate(errorMessage$message),
            duration=5,
            type="error"
         )
      })

      #########################################################################@
      ## Model view ----
      #########################################################################@

      observe({
         m <- model$x
         updateSelectInput(
            session,
            "findTable",
            choices=sort(names(m)),
            selected=intersect(isolate(selection$tables), names(m))
         )
      })
      observe({
         selTables <- selection$tables
         updateSelectInput(
            session,
            "findTable",
            selected=as.character(selTables)
         )
      })
      observe({
         selTables <- sort(input$findTable)
         validate(need(!identical(selTables, isolate(selection$tables)), ""))
         mn <- isolate(model$vn)
         selFK <- mn$edges %>%
            filter(.data$from %in% selTables | .data$to %in% selTables) %>%
            pull("id")
         selection$fromVN <- FALSE
         if(length(selTables)==0){
            selection$tables <- NULL
         }else{
            selection$tables <- selTables
         }
         selection$release <- isolate(selection$release)+1
      })
      observeEvent(input$selectAll, {
         m <- isolate(model$x)
         updateSelectInput(
            session,
            "findTable",
            selected=sort(names(m))
         )
      })
      observeEvent(input$autoLayout, {
         m <- auto_layout(isolate(model$x), force=TRUE)
         model$new <- m
      })

      output$modelSummary <- renderUI({
         m <- model$x
         mn <- model$vn
         nt <- length(m)
         nfk <- nrow(mn$edges)
         np <- lapply(m, function(x) nrow(x$fields)) %>% unlist() %>% sum()
         tagList(
            tags$strong("Tables:"), nt, "-",
            tags$strong("Foreign keys:"), nfk, "-",
            tags$strong("Fields:"), np
         )
      })

      #########################################################################@
      ## Model network ----
      #########################################################################@

      output$modelNet <- renderVisNetwork({
         replot$x
         selection$fromVN <- FALSE
         selection$tables <- NULL
         selection$fk <- NULL
         plot(isolate(model$x), color=isolate(settings$defaultColor)) %>%
            visEvents(release="releaseVn")
      })

      observe({
         input$modelNet_release
         selection$release <- isolate(selection$release)+1
      })
      observe({
         selection$release
         visNetworkProxy("modelNet") %>% visGetSelectedNodes()
         visNetworkProxy("modelNet") %>% visGetSelectedEdges()
         visNetworkProxy("modelNet") %>% visGetNodes()
      })


      modelNet_selectedNodes <- reactive({
         input$modelNet_selectedNodes
      })
      observe({
         selTables <- intersect(
            modelNet_selectedNodes(),
            names(model$x)
         ) %>% sort()
         selection$fromVN <- TRUE
         if(length(selTables)==0){
            selection$tables <- NULL
         }else{
            selection$tables <- selTables
         }
      })

      modelNet_selectedEdges <- reactive({input$modelNet_selectedEdges})
      observe({
         selFK <- intersect(
            modelNet_selectedEdges(),
            model$vn$edges$id
         ) %>% sort()
         selection$fromVN <- TRUE
         if(length(selFK)==0){
            selection$fk <- NULL
         }else{
            selection$fk <- selFK
         }
      })

      observeEvent(input$fitNet, {
         visNetworkProxy("modelNet") %>% visFit()
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
            fluidRow(
               column(
                  6,
                  class="leftBox",
                  fileInput(
                     "impModel", "Choose an sql or a json file",
                     multiple=FALSE,
                     accept=c(".sql", ".json", ".sql.gz", ".json.gz"),
                     width="100%"
                  )
               ),
               column(
                  2,
                  class="leftBox",
                  uiOutput("exampleModel")
               ),
               column(
                  4,
                  class="rightBox",
                  div(
                     class="mainButton",
                     actionButton(
                        "docImp", "", icon=icon("question-circle", "fa-2x")
                     ),
                     title="Help tour"
                  )
               )
            ),
            fluidRow(uiOutput("impModel"))
         )
      })

      ## _+ Import preview ----
      output$impModel <- renderUI({
         mi <- model$toImport
         validate(need(!is.null(mi), ""))
         if(!is.RelDataModel(mi)){
            list(p(mi, class="errorMessage"))
         }else{
            list(
               div(
                  visNetworkOutput("impModelNet", height="65vh", width="100%"),
                  id="impModelFrame"
               ),
               uiOutput("impMessage")
            )
         }
      })

      output$impModelNet <- renderVisNetwork({
         mi <- model$toImport
         validate(need(mi, ""))
         plot(mi, color=isolate(settings$defaultColor))
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
               list(icon("file-import", "fa-2x"), "Merge with current model")
            )
         }else{
            list(p(mm, class="errorMessage"))
         }
      })

      ## _+ From model ----
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
            model$toImport <- auto_layout(mi, lengthMultiplier=45*length(mi))
         }else{
            model$toImport <- mi
         }
      })

      ## _+ From example ----
      output$exampleModel <- renderUI({
         validate(need(file.exists(example), ""))
         m <- try(read_json_data_model(example), silent=TRUE)
         if(!is.RelDataModel(m)){
            m <- try(read_SQL_data_model(example), silent=TRUE)
         }
         validate(need(m, ""))
         actionLink("exampleLink", label="Try an example")
      })
      observeEvent(input$exampleLink, {
         validate(need(file.exists(example), ""))
         mi <- try(read_json_data_model(example), silent=TRUE)
         if(!is.RelDataModel(mi)){
            mi <- try(read_SQL_data_model(example), silent=TRUE)
         }
         validate(need(mi, ""))
         model$toImport <-  auto_layout(mi, lengthMultiplier=45*length(mi))
      })

      ## _+ Validate import ----
      observe({
         validate(need(input$importValidate, ""))
         mm <- isolate(model$merged)
         validate(need(mm, ""))

         cmn <- isolate(model$vn)
         if(
            !is.null(cmn$nodes)>0 && nrow(cmn$nodes)>0 &&
            all(!is.na(cmn$nodes$x)) && all(!is.na(cmn$nodes$y))
         ){
            toReplot <- FALSE
            cmxrange <- c(min(cmn$nodes$x), max(cmn$nodes$x))
            cmyrange <- c(min(cmn$nodes$x), max(cmn$nodes$y))
            validate(need(isolate(model$toImport), ""))
            tin <- modelToVn(
               isolate(model$toImport),
               color=isolate(settings$defaultColor)
            )
            if(any(is.na(tin$nodes$x)) || any(is.na(tin$nodes$y))){
               tin$nodes$x <- stats::runif(
                  nrow(tin$nodes), min=cmxrange[1], max=cmxrange[2]
               )
               tin$nodes$y <- stats::runif(
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
            div(
               fluidRow(
                  column(
                     10,
                     textInput(
                        "newTableName", label=NULL, width="100%",
                        placeholder="Table name"
                     )
                  ),
                  column(2, actionButton("confirmAddTable", "Add"))
               ),
               fluidRow(uiOutput("newTableExists"))
            ),
            size="s",
            easyClose=TRUE
         ))
      })

      output$newTableExists <- renderUI({
         ntn <- input$newTableName
         m <- isolate(model$x)
         if(ntn %in% names(m)){
            p("Table name already used", class="errorMessage")
         }else{
            list()
         }
      })

      observe({
         ntn <- input$newTableName
         m <- isolate(model$x)
         if(is.null(ntn) || ntn=="" || ntn %in% names(m)){
            shinyjs::disable("confirmAddTable")
         }else{
            shinyjs::enable("confirmAddTable")
         }
      })

      observe({
         validate(need(input$confirmAddTable, ""))
         tn <- isolate(input$newTableName)
         m <- isolate(model$x)
         xs <- 100
         ys <- 100
         if(!is.null(tn) && tn!="" && !tn %in% names(m)){
            m <- add_table(m, newTable=tn)
            pr <- stats::rbeta(1, 9, 1)
            pa <- stats::runif(1, 0, 2*pi)
            m <- m %>% update_table_display(
               tn,
               px=xs*pr*cos(pa),
               py=ys*pr*sin(pa)
            )
            model$new <- m
            removeModal()
         }
      })

      #########################################################################@
      ## Edit table ----
      #########################################################################@

      observe({
         selTables <- selection$tables
         m <- model$x
         if(!is.RelDataModel(m)){
            model$table <- NULL
         }else{
            if(length(selTables)==1 && selTables %in% names(m)){
               model$table <- m[[selTables]]
            }else{
               model$table <- NULL
            }
         }
      })

      output$editTable <- renderUI({
         mt <- model$table
         validate(need(mt, ""))
         selTable <- mt$tableName
         div(
            fluidRow(
               column(8, h3(selTable)),
               column(
                  4,
                  class="rightBox",
                  actionButton("renameTable", "Rename") %>%
                     div(title="Rename the table")
               )
            ),
            uiOutput("tableCommentUI"),
            tags$hr(class="editSeparator"),
            uiOutput("fields"),
            tags$hr(class="editSeparator"),
            uiOutput("primaryKey"),
            tags$hr(class="editSeparator"),
            uiOutput("indexes")
         )
      })

      ## _+ Rename table ----
      observeEvent(input$renameTable, {
         showModal(modalDialog(
            title="Rename table",
            div(
               fluidRow(
                  column(
                     10,
                     textInput(
                        "tableNewName", label=NULL, width="100%",
                        placeholder="Table new name"
                     )
                  ),
                  column(2, actionButton("confirmRenameTable", "Rename"))
               ),
               fluidRow(uiOutput("newNameExists"))
            ),
            size="s",
            easyClose=TRUE
         ))
      })
      output$newNameExists <- renderUI({
         ntn <- input$tableNewName
         m <- isolate(model$x)
         if(ntn %in% names(m)){
            p("Table name already used", class="errorMessage")
         }else{
            list()
         }
      })
      observe({
         ntn <- input$tableNewName
         m <- isolate(model$x)
         if(is.null(ntn) || ntn=="" || ntn %in% names(m)){
            shinyjs::disable("confirmRenameTable")
         }else{
            shinyjs::enable("confirmRenameTable")
         }
      })
      observe({
         validate(need(input$confirmRenameTable, ""))
         tn <- isolate(input$tableNewName)
         mt <- isolate(model$table)
         validate(need(mt, ""))
         selTable <- mt$tableName
         m <- isolate(model$x)
         if(!is.null(tn) && tn!="" && !tn %in% names(m)){
            m <- rename_table(m, old=selTable, new=tn)
            model$new <- m
            removeModal()
         }
      })

      ## _+ Table commment ----
      output$tableCommentUI <- renderUI({
         mt <- model$table
         validate(need(mt, ""))
         selTable <- mt$tableName
         fluidRow(
            column(
               8,
               textAreaInput(
                  "tableComment",
                  label=NULL,
                  value=ifelse(
                     is.na(mt$display$comment), "", mt$display$comment
                  ),
                  width="100%",
                  placeholder="Table description"
               )
            ),
            column(
               4,
               uiOutput("refreshCommentH", class="updateHighlight"),
               actionButton(
                  "refreshComment",
                  label=NULL,
                  icon=icon("check", "fa-1x"),
                  class="disabled"
               ) %>% div(title="Update table comment", class="iblock"),
               class="rightBox"
            )
         )
      })
      observe({
         input$refreshComment
         ntn <- input$tableComment
         validate(need(length(ntn)>0, ""))
         mt <- model$table
         validate(need(mt, ""))
         selTable <- mt$tableName
         cc <- mt$display$comment
         ntn <- ifelse(is.na(ntn), "", ntn)
         cc <- ifelse(is.na(cc), "", cc)
         if(ntn==cc){
            shinyjs::disable("refreshComment")
         }else{
            shinyjs::enable("refreshComment")
         }
      })
      output$refreshCommentH <- renderUI({
         input$refreshComment
         ntn <- input$tableComment
         validate(need(length(ntn)>0, ""))
         mt <- model$table
         validate(need(mt, ""))
         selTable <- mt$tableName
         cc <- mt$display$comment
         ntn <- ifelse(is.na(ntn), "", ntn)
         cc <- ifelse(is.na(cc), "", cc)
         validate(need(ntn!=cc, ""))
         icon("arrow-right", "fa-1x")
      })
      observe({
         input$refreshComment
         nc <- isolate(input$tableComment)
         validate(need(!is.na(nc), ""))
         if(nc==""){
            nc <- NA
         }
         mt <- isolate(model$table)
         validate(need(mt, ""))
         selTable <- mt$tableName
         cc <- mt$display$comment
         m <- isolate(model$x)
         if(
            xor(is.na(nc), is.na(cc)) ||
            (!is.na(nc) && !is.na(cc) && nc!=cc)
         ){
            model$new <- m %>% update_table_display(
               tableName=selTable,
               comment=nc
            )
         }
      })

      ## _+ Table fields ----
      output$fields <- renderUI({
         mt <- model$table
         validate(need(mt, ""))
         selTable <- mt$tableName
         list(
            fluidRow(
               column(6, h4("Fields")),
               column(
                  6,
                  uiOutput("updateFieldDiv", inline=TRUE),
                  actionButton(
                     "addField", label="",
                     icon=icon("plus-square", "fa-1x"),
                     class="shrunkenButton"
                  ) %>%
                     div(
                        title="Add a new field",
                        class="iblock"
                     ),
                  class="rightBox"
               )
            ),
            fluidRow(
               column(12, DT::DTOutput("fieldTable"))
            ),
            uiOutput("fieldCommentDisplay")
         )
      })
      output$fieldTable <- DT::renderDT({
         mt <- model$table
         # validate(need(mt, ""))
         # selTable <- mt$tableName
         isolate(model$fieldTable) %>%
            select(-"comment") %>%
            DT::datatable(
               rownames=TRUE,
               filter="top",
               selection=list(mode='single', selected=c(), target='row'),
               options=list(
                  dom="tip",
                  columnDefs = list(
                     list(targets=c(0), visible=TRUE, width='4%'),
                     list(targets=c(1), visible=TRUE, width='24%'),
                     list(targets=c(2), visible=TRUE, width='24%'),
                     list(targets=c(3), visible=TRUE, width='24%')
                  )
               )
            )
      })
      proxyFieldTable <- DT::dataTableProxy("fieldTable")
      observe({
         mt <- model$table
         validate(need(mt, ""))
         selTable <- mt$tableName
         model$fieldTable <- mt$fields
      })
      observe({
         DT::replaceData(
            proxyFieldTable,
            data=model$fieldTable %>% select(-"comment"),
            clearSelection="all"
         )
      })
      # ## __- Display field comment
      output$fieldCommentDisplay <- renderUI({
         seli <- input$fieldTable_rows_selected
         validate(need(length(seli)==1, ""))
         validate(need(nrow(model$fieldTable)>0, ""))
         mt <- isolate(model$table)
         validate(need(mt, ""))
         selTable <- mt$tableName
         validate(need(nrow(mt$fields)>0, ""))
         validate(need(seli>=1 & seli <= nrow(mt$fields), ""))
         p(mt$fields$comment[seli])
      })
      # ## __- Modify fields ----
      output$updateFieldDiv <- renderUI({
         seli <- input$fieldTable_rows_selected
         validate(need(length(seli)==1, ""))
         validate(need(nrow(model$fieldTable)>0, ""))
         mt <- isolate(model$table)
         validate(need(mt, ""))
         selTable <- mt$tableName
         validate(need(nrow(mt$fields)>0, ""))
         validate(need(seli>=1 & seli <= nrow(mt$fields), ""))
         div(
            actionButton(
               "moveFieldUp",
               label="",
               icon=icon("arrow-alt-circle-up", "fa-1x"),
               class="shrunkenButton"
            ) %>%
               div(
                  title="Move up",
                  class="iblock"
               ),
            actionButton(
               "moveFieldDown",
               label="",
               icon=icon("arrow-alt-circle-down", "fa-1x"),
               class="shrunkenButton"
            ) %>%
               div(
                  title="Move up",
                  class="iblock"
               ),
            actionButton(
               "updateField",
               label="",
               icon=icon("edit", "fa-1x"),
               class="shrunkenButton"
            ) %>%
               div(
                  title="Edit field properties",
                  class="iblock"
               ),
            actionButton(
               "removeField",
               label="",
               icon=icon("minus-square", "fa-1x"),
               class="shrunkenButton"
            ) %>% div(title="Remove field", class="iblock"),
            class="iblock"
         )
      })
      # ## __- Remove field ----
      observe({
         validate(need(input$removeField>0, ""))
         seli <- isolate(input$fieldTable_rows_selected)
         validate(need(length(seli)==1, ""))
         mt <- isolate(model$table)
         validate(need(mt, ""))
         selTable <- mt$tableName
         m <- isolate(model$x)
         validate(need(nrow(mt$fields)>0, ""))
         fn <- mt$fields$name[seli]
         m <- try(m %>% remove_field(
            tableName=selTable,
            fieldName=fn
         ), silent=TRUE)
         if(is.RelDataModel(m)){
            model$new <- m
         }else{
            showModal(modalDialog(
               title="Unable to remove field",
               p(
                  HTML(paste(
                     sprintf("<u>%s</u> is used in foreign key(s).", fn),
                     "Remove the foreign key(s) before removing this fields.",
                     sep="<br>"
                  )),
                  class="errorMessage"
               ),
               size="s",
               easyClose=TRUE
            ))
         }
      })
      # ## __- Move field ----
      observe({
         validate(need(input$moveFieldUp>0, ""))
         seli <- isolate(input$fieldTable_rows_selected)
         validate(need(length(seli)==1, ""))
         mt <- isolate(model$table)
         validate(need(mt, ""))
         selTable <- mt$tableName
         m <- isolate(model$x)
         validate(need(nrow(mt$fields)>0, ""))
         fn <- mt$fields$name[seli]
         alli <- 1:nrow(m[[selTable]]$fields)
         fi <- which(m[[selTable]]$fields$name==fn)
         validate(need(fi>1, ""))
         o <- c(
            alli[which(alli < (fi-1))],
            fi, fi-1,
            alli[which(alli > fi)]
         )
         m <- m %>% order_fields(
            tableName=selTable,
            order=o
         )
         model$new <- m
      })
      observe({
         validate(need(input$moveFieldDown>0, ""))
         seli <- isolate(input$fieldTable_rows_selected)
         validate(need(length(seli)==1, ""))
         mt <- isolate(model$table)
         validate(need(mt, ""))
         selTable <- mt$tableName
         m <- isolate(model$x)
         validate(need(nrow(mt$fields)>0, ""))
         fn <- mt$fields$name[seli]
         alli <- 1:nrow(m[[selTable]]$fields)
         fi <- which(m[[selTable]]$fields$name==fn)
         validate(need(fi<length(alli), ""))
         o <- c(
            alli[which(alli < fi)],
            fi+1, fi,
            alli[which(alli > (fi+1))]
         )
         m <- m %>% order_fields(
            tableName=selTable,
            order=o
         )
         model$new <- m
      })
      ## __- Add field ----
      observeEvent(input$addField, {
         mt <- isolate(model$table)
         validate(need(mt, ""))
         selTable <- mt$tableName
         fields <- mt$fields
         showModal(modalDialog(
            title="Add field",
            fluidRow(
               column(
                  10,
                  textInput(
                     "newFieldName", label="Field",
                     placeholder="Field name",
                     width="100%"
                  ),
                  uiOutput("existingNewField"),
                  selectInput(
                     "newFieldType", label="Type",
                     choices=SUPPTYPES,
                     selected=NULL, multiple=FALSE
                  ),
                  checkboxInput(
                     "newFieldNullable", label="Nullable?",
                     value=FALSE,
                  ),
                  checkboxInput(
                     "newFieldUnique", label="Unique?",
                     value=FALSE,
                  ),
                  textAreaInput(
                     "newFieldComment", label="Comment",
                     placeholder="Field description",
                     width="100%"
                  )
               ),
               column(
                  2,
                  actionButton("confirmAddField", "Add")
               )
            ),
            size="s",
            easyClose=TRUE
         ))
      })
      observe({
         nfn <- input$newFieldName
         mt <- isolate(model$table)
         validate(need(mt, ""))
         selTable <- mt$tableName
         fields <- mt$fields
         if(
            length(nfn)==0 ||
            is.na(nfn) ||
            nfn=="" ||
            nfn %in% fields$name
         ){
            shinyjs::disable("confirmAddField")
         }else{
            shinyjs::enable("confirmAddField")
         }
      })
      output$existingNewField <- renderUI({
         mt <- isolate(model$table)
         validate(need(mt, ""))
         selTable <- mt$tableName
         fields <- mt$fields
         validate(need(input$newFieldName %in% fields$name, ""))
         p("Field name already used", class="errorMessage")
      })
      observeEvent(input$confirmAddField, {
         mt <- isolate(model$table)
         validate(need(mt, ""))
         selTable <- mt$tableName
         fields <- mt$fields
         nfn <- isolate(input$newFieldName)
         validate(need(
            !is.null(nfn) &&
            nfn!="" &&
            !nfn %in% fields$name,
            ""
         ))
         nm <- isolate(model$x) %>%
            add_field(
               tableName=selTable,
               name=nfn,
               type=isolate(input$newFieldType),
               nullable=isolate(input$newFieldNullable),
               unique=isolate(input$newFieldUnique),
               comment=as.character(isolate(input$newFieldComment))
            )
         if(!identical(nm, isolate(model$x))){
            model$new <- nm
         }
         removeModal()
      })

      ## __- Update field ----
      observeEvent(input$updateField, {
         seli <- isolate(input$fieldTable_rows_selected)
         validate(need(length(seli)==1, ""))
         mt <- isolate(model$table)
         validate(need(mt, ""))
         selTable <- mt$tableName
         fields <- mt$fields
         validate(need(nrow(fields)>0, ""))
         showModal(modalDialog(
            title="Update field",
            fluidRow(
               column(
                  10,
                  textInput(
                     "fieldName", label="Field",
                     value=fields$name[seli],
                     placeholder="Field name",
                     width="100%"
                  ),
                  uiOutput("existingField"),
                  selectInput(
                     "fieldType", label="Type",
                     choices=SUPPTYPES,
                     selected=fields$type[seli], multiple=FALSE
                  ),
                  checkboxInput(
                     "fieldNullable", label="Nullable?",
                     value=fields$nullable[seli],
                  ),
                  checkboxInput(
                     "fieldUnique", label="Unique?",
                     value=fields$unique[seli],
                  ),
                  textAreaInput(
                     "fieldComment", label="Comment",
                     value=fields$comment[seli],
                     placeholder="Field description",
                     width="100%"
                  ),
                  uiOutput("updateFieldError")
               ),
               column(
                  2,
                  actionButton("confirmUpdateField", "Update")
               )
            ),
            size="s",
            easyClose=TRUE
         ))
      })
      observe({
         nfn <- input$fieldName
         seli <- isolate(input$fieldTable_rows_selected)
         validate(need(length(seli)==1, ""))
         mt <- isolate(model$table)
         validate(need(mt, ""))
         selTable <- mt$tableName
         fields <- mt$fields
         if(
            length(nfn)==0 ||
            is.na(nfn) ||
            nfn=="" ||
            nfn %in% fields$name[-seli]
         ){
            shinyjs::disable("confirmUpdateField")
         }else{
            shinyjs::enable("confirmUpdateField")
         }
      })
      output$existingField <- renderUI({
         nfn <- input$fieldName
         seli <- isolate(input$fieldTable_rows_selected)
         validate(need(length(seli)==1, ""))
         mt <- isolate(model$table)
         validate(need(mt, ""))
         selTable <- mt$tableName
         fields <- mt$fields
         validate(need(nfn %in% fields$name[-seli], ""))
         p("Field name already used", class="errorMessage")
      })
      updateField <- reactiveValues(error=NULL)
      observeEvent(input$confirmUpdateField, {
         seli <- isolate(input$fieldTable_rows_selected)
         validate(need(length(seli)==1, ""))
         mt <- isolate(model$table)
         validate(need(mt, ""))
         selTable <- mt$tableName
         fields <- mt$fields
         nfn <- isolate(input$fieldName)
         validate(need(
            !is.null(nfn) &&
               nfn!="" &&
               !nfn %in% fields$name[-seli],
            ""
         ))
         nm <- isolate(model$x)
         if(nfn != fields$name[seli]){
            nm <- nm %>% rename_field(
               tableName=selTable,
               current=fields$name[seli],
               new=nfn
            )
         }
         nm <- try(nm %>%
            update_field(
               tableName=selTable,
               fieldName=nfn,
               type=isolate(input$fieldType),
               nullable=isolate(input$fieldNullable),
               unique=isolate(input$fieldUnique),
               comment=as.character(isolate(input$fieldComment))
            ), silent=TRUE)
         if(is.RelDataModel(nm)){
            updateField$error <- NULL
            if(!identical(nm, isolate(model$x))){
               model$new <- nm
               sendWarning(paste(
                  "Uniqueness or mandatory contraints may not have been",
                  "changed if they are required for",
                  "existing indexes or foreign keys."
               ))
            }else{
               sendError(paste(
                  "The field has not been modified because",
                  "uniqueness or mandatory constraints are required for",
                  "existing indexes or foreign keys."
               ))
            }
            removeModal()
         }else{
            updateField$error <- nm
         }
      })
      output$updateFieldError <- renderUI({
         e <- updateField$error
         validate(need(!is.null(e), ""))
         p(e, class="errorMessage")
      })


      ## _+ Table primary key ----
      output$primaryKey <- renderUI({
         mt <- model$table
         validate(need(mt, ""))
         selTable <- mt$tableName
         fnames <- mt$fields$name
         validate(need(fnames, ""))
         fluidRow(
            column(3, h4("Primary key")),
            column(
               5,
               selectInput(
                  "primaryKey", label=NULL,
                  choices=fnames,
                  selected=mt$primaryKey,
                  multiple=TRUE,
                  width="100%"
               )
            ),
            column(
               4,
               uiOutput("refreshPKH", class="updateHighlight"),
               actionButton(
                  "refreshPrimaryKey",
                  label=NULL,
                  icon=icon("check", "fa-1x"),
                  class="disabled"
               ) %>% div(title="Update table primary key", class="iblock"),
               class="rightBox"
            )
         )
      })
      observe({
         input$refreshPrimaryKey
         npk <- input$primaryKey
         mt <- isolate(model$table)
         validate(need(mt, ""))
         selTable <- mt$tableName
         cpk <- mt$primaryKey
         if(length(cpk)!=length(npk) || any(sort(cpk)!=sort(npk))){
            shinyjs::enable("refreshPrimaryKey")
         }else{
            shinyjs::disable("refreshPrimaryKey")
         }
      })
      output$refreshPKH <- renderUI({
         input$refreshPrimaryKey
         npk <- input$primaryKey
         mt <- isolate(model$table)
         validate(need(mt, ""))
         selTable <- mt$tableName
         cpk <- mt$primaryKey
         validate(need(
            length(cpk)!=length(npk) || any(sort(cpk)!=sort(npk)),
            ""
         ))
         icon("arrow-right", "fa-1x")
      })
      observe({
         validate(need(input$refreshPrimaryKey>0, ""))
         npk <- isolate(input$primaryKey)
         # npk <- input$primaryKey
         mt <- isolate(model$table)
         validate(need(mt, ""))
         selTable <- mt$tableName
         m <- isolate(model$x)
         cpk <- mt$primaryKey
         if(length(cpk)!=length(npk) || any(sort(cpk)!=sort(npk))){
            model$new <- m %>%
               set_primary_key(tableName=selTable, fieldNames=npk)
            sendWarning(paste(
               "Some indexes, uniqueness and mandatory constraints",
               "may have been added to support the primary key."
            ))
         }
      })

      ## _+ Table indexes ----
      output$indexes <- renderUI({
         mt <- model$table
         validate(need(mt, ""))
         selTable <- mt$tableName
         fnames <- mt$fields$name
         validate(need(fnames, ""))
         list(
            fluidRow(
               column(6, h4("Indexes")),
               column(
                  6,
                  uiOutput("updateIndexDiv", inline=TRUE),
                  actionButton(
                     "addIndex", label="",
                     icon=icon("plus-square", "fa-1x"),
                     class="shrunkenButton"
                  ) %>%
                     div(title="Add an index", class="iblock"),
                  class="rightBox"
               )
            ),
            fluidRow(
               column(12, DT::DTOutput("indexTable"))
            )
         )
      })
      output$indexTable <- DT::renderDT({
         mt <- model$table
         # validate(need(mt, ""))
         # selTable <- mt$tableName
         isolate(model$indexTable) %>%
            DT::datatable(
               rownames=TRUE,
               filter="top",
               selection=list(mode='single', selected=c(), target='row'),
               options=list(
                  dom="tip",
                  columnDefs = list(
                     list(targets=c(0), visible=TRUE, width='10%'),
                     list(targets=c(1), visible=TRUE, width='70%'),
                     list(targets=c(2), visible=TRUE, width='20%')
                  )
               )
            )
      })
      proxyIndexTable <- DT::dataTableProxy("indexTable")
      observe({
         mt <- model$table
         validate(need(mt, ""))
         selTable <- mt$tableName
         if(length(mt$indexes)>0){
            indexTable <- mt$indexes %>%
               lapply(function(x){
                  tibble(
                     Fields=sprintf("[%s]", paste(x$fields, collapse="], [")),
                     Unique=x$unique
                  )
               })
            model$indexTable <- do.call(rbind, indexTable)
         }else{
            model$indexTable <- tibble(
               Fields=character(),
               Unique=logical()
            )
         }
      })
      observe({
         DT::replaceData(
            proxyIndexTable,
            data=model$indexTable,
            clearSelection="all"
         )
      })
      ## __- Update index ----
      output$updateIndexDiv <- renderUI({
         seli <- input$indexTable_rows_selected
         validate(need(length(seli)==1, ""))
         validate(need(nrow(model$indexTable)>0, ""))
         mt <- isolate(model$table)
         validate(need(mt, ""))
         selTable <- mt$tableName
         validate(need(length(mt$indexes)>0, ""))
         validate(need(seli>=1 & seli <= length(mt$indexes), ""))
         ui <- mt$indexes[[seli]]$unique
         list(
            actionButton(
               "updateIndex",
               label="",
               icon=icon("edit", "fa-1x"),
               class="shrunkenButton"
            ) %>%
               div(
                  title="Update index properties",
                  class="iblock"
               ),
            actionButton(
               "removeIndex",
               label="",
               icon=icon("minus-square", "fa-1x"),
               class="shrunkenButton"
            ) %>%
               div(
                  title="Remove index",
                  class="iblock"
               )
         )
      })
      observeEvent(input$updateIndex,{
         seli <- isolate(input$indexTable_rows_selected)
         validate(need(length(seli)==1, ""))
         validate(need(nrow(model$indexTable)>0, ""))
         mt <- isolate(model$table)
         validate(need(mt, ""))
         selTable <- mt$tableName
         validate(need(length(mt$indexes)>0, ""))
         validate(need(seli>=1 & seli <= length(mt$indexes), ""))
         ui <- mt$indexes[[seli]]$unique
         showModal(modalDialog(
            title="Update field",
            checkboxInput("setUniqueIndex", "Unique?", value=ui),
            size="s",
            easyClose=TRUE
         ))
      })
      observe({
         ui <- input$setUniqueIndex
         validate(need(!is.null(ui) && !is.na(ui), ""))
         seli <- isolate(input$indexTable_rows_selected)
         validate(need(length(seli)==1, ""))
         mt <- isolate(model$table)
         validate(need(mt, ""))
         selTable <- mt$tableName
         validate(need(length(mt$indexes)>0, ""))
         m <- isolate(model$x)
         if(mt$indexes[[seli]]$unique!=ui){
            m <- m %>% set_unique_index(
               tableName=selTable,
               fieldNames=mt$indexes[[seli]]$fields,
               unique=ui
            )
            if(!identical(m, isolate(model$x))){
               model$new <- m
            }else{
               sendError(paste(
                  "The index could not be modified because it is required",
                  "for primary or foreign keys."
               ))
            }
            removeModal()
         }
      })
      ## __- Remove index ----
      observe({
         validate(need(input$removeIndex>0, ""))
         seli <- isolate(input$indexTable_rows_selected)
         validate(need(length(seli)==1, ""))
         mt <- isolate(model$table)
         validate(need(mt, ""))
         selTable <- mt$tableName
         validate(need(length(mt$indexes)>0, ""))
         m <- isolate(model$x)
         nm <- m %>% remove_index(
            tableName=selTable,
            fieldNames=mt$indexes[[seli]]$fields
         )
         model$new <- nm
         if(!identical(nm, m)){
            model$new <- nm
         }else{
            sendError(paste(
               "The index could not be removed because it is required",
               "for primary or foreign keys."
            ))
         }
      })
      ## __- Add index ----
      observeEvent(input$addIndex, {
         mt <- isolate(model$table)
         validate(need(mt, ""))
         selTable <- mt$tableName
         fnames <- mt$fields$name
         showModal(modalDialog(
            title="Add index",
            div(
               fluidRow(
                  column(
                     8,
                     selectInput(
                        "newIndexFields", label=NULL,
                        choices=fnames,
                        multiple=TRUE
                     )
                  ),
                  column(
                     2,
                     checkboxInput("uniqueNewIndex", "Unique?", value=FALSE)
                  ),
                  column(2, actionButton("confirmAddIndex", "Add"))
               )
            ),
            size="s",
            easyClose=TRUE
         ))
      })
      observe({
         if(length(input$newIndexFields)==0){
            shinyjs::disable("confirmAddIndex")
         }else{
            shinyjs::enable("confirmAddIndex")
         }
      })
      observeEvent(input$confirmAddIndex, {
         mt <- isolate(model$table)
         validate(need(mt, ""))
         selTable <- mt$tableName
         validate(need(length(input$newIndexFields)>0, ""))
         nm <- isolate(model$x) %>%
            add_index(
               tableName=selTable,
               fieldNames=input$newIndexFields,
               unique=input$uniqueNewIndex
            )
         if(!identical(nm, isolate(model$x))){
            model$new <- nm
         }else{
            sendError(paste(
               "The index could not be added:",
               "it may already exist or it may not fit other constraints."
            ))
         }
         removeModal()
      })

      #########################################################################@
      ## Edit menu ----
      #########################################################################@

      output$addFKInput <- renderUI({
         selTable <- selection$tables
         validate(need(length(selTable)>0 & length(selTable)<=2, ""))
         actionButton(
            "addForeignKey", "Key",
            icon=icon("plus", "fa-2x"),
            # icon=icon("external-link-alt", "fa-2x"),
            class="shrunkenButton"
         ) %>% div(
            title="Add a foreign key"
         )
      })
      output$editFKInput <- renderUI({
         selFK <- selection$fk
         validate(need(length(selFK)==1, ""))
         actionButton(
            "editFK",
            label=HTML(paste(
               '<i class="far fa-edit fa-2x"></i>',
               'keys'
            )),
            class="shrunkenButton"
         ) %>% div(
            title="Edit cardinalities of the selected foreign key (F2)"
         )
      })
      output$rmFKInput <- renderUI({
         selFK <- selection$fk
         validate(need(length(selFK)>0, ""))
         actionButton(
            "removeFK",
            label=HTML(paste(
               '<i class="far fa-trash-alt fa-2x"></i>',
               'keys'
            )),
            class="shrunkenButton"
         ) %>% div(
            title="Remove selected foreign keys (del)"
         )
      })
      output$rmTablesInput <- renderUI({
         selTable <- selection$tables
         validate(need(length(selTable)>0, ""))
         actionButton(
            "removeTables",
            label=HTML(paste(
               '<i class="fas fa-trash fa-2x"></i>',
               'tables'
            )),
            class="shrunkenButton"
         ) %>% div(
            title="Remove selected tables (del)"
         )
      })
      output$dupTablesInput <- renderUI({
         selTable <- selection$tables
         validate(need(length(selTable)>0, ""))
         actionButton(
            "duplicateTables",
            label=HTML(paste(
               '<i class="fas fa-copy fa-2x"></i>',
               'tables'
            )),
            class="shrunkenButton"
         ) %>% div(
            title="Duplicate selected tables"
         )
      })

      #########################################################################@
      ## Table color ----
      #########################################################################@

      output$setTableColor <- renderUI({
         selTables <- selection$tables
         validate(need(length(selTables)>0, ""))
         tval <- lapply(
            isolate(model$x),
            function(x) x$display$color
         ) %>% unlist()
         tval <- tval[selTables] %>% unique()
         if(length(tval)>1 || is.na(tval)){
            tval=""
         }
         colourpicker::colourInput(
            "tableColor",
            label=NULL,
            value=tval,
            showColour="background",
            palette="limited",
            allowedCols=c(
               "", isolate(settings$availableColors)
            ),
            allowTransparent=TRUE
         ) %>% div(
            id="tabColPick",
            title="Select table color"
         )
      })
      observe({
         newCol <- input$tableColor
         validate(need(newCol!="", ""))
         selTables <- isolate(selection$tables)
         validate(need(length(selTables)>0, ""))
         m <- isolate(model$x)
         tval <- lapply(
            m,
            function(x) x$display$color
         ) %>% unlist()
         tval <- tval[selTables] %>% unique()
         if(length(tval)>1 || is.na(tval) || tval!=newCol){
            for(tn in selTables){
               m <- m %>%  update_table_display(
                  tableName=tn,
                  color=newCol
               )
            }
            model$new <- m
         }
      })

      #########################################################################@
      ## Remove tables ----
      #########################################################################@

      observe({
         takeAction <- (
            (!is.null(input$removeTables) && input$removeTables > 0)
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
                        sprintf("<u>%s</u> is referenced by other tables.", tn),
                        "Remove foreign key(s) before removing this table.",
                        sep="<br>"
                     )),
                     class="errorMessage"
                  ),
                  size="s",
                  easyClose=TRUE
               ))
            }
         }
      })

      #########################################################################@
      ## Duplicate tables ----
      #########################################################################@

      observe({
         takeAction <- (
            (!is.null(input$duplicateTables) && input$duplicateTables > 0)
         )
         validate(need(takeAction, ""))
         tns <- isolate(selection$tables)
         m <- isolate(model$x)
         vn <- isolate(model$vn)
         xs <- 100
         ys <- 100
         nm <- unclass(m)
         if(length(tns)>0 && all(tns!="") && all(tns %in% names(m))){
            for(tn in tns){
               i <- 1
               ntn <- paste(tn, "COPY", i, sep="_")
               while(ntn %in% names(m)){
                  i <- i+1
                  ntn <- paste(tn, "COPY", i, sep="_")
               }
               toAdd <- nm[tn]
               toAdd[[1]]$tableName <- ntn
               pr <- stats::rbeta(1, 9, 1)
               pa <- stats::runif(1, 0, 2*pi)
               toAdd[[1]]$display$x <- toAdd[[1]]$display$x +
                  xs*pr*cos(pa)
               toAdd[[1]]$display$y <- toAdd[[1]]$display$y +
                  ys*pr*sin(pa)
               names(toAdd) <- ntn
               nm <- c(nm, toAdd)
            }
         }
         model$new <- RelDataModel(nm)
      })

      #########################################################################@
      ## Add foreign keys ----
      #########################################################################@

      foreignKey <- reactiveValues(
         triggered=0,
         fromTable=NULL,
         toTable=NULL,
         fromFields=NULL,
         toFields=NULL,
         fmin=NULL,
         fmax=NULL,
         tmin=NULL,
         tmax=NULL
      )

      observe({
         validate(need(input$addForeignKey > 0, ""))
         tns <- isolate(selection$tables)
         m <- isolate(model$x)
         foreignKey$fromTable <- foreignKey$toTable <-
            foreignKey$fromFields <- foreignKey$toFields <- NULL
         foreignKey$triggered=isolate(foreignKey$triggered)+1
         if(length(tns)>0 && all(tns!="") && all(tns %in% names(m))){
            showModal(modalDialog(
               title="Add foreign key",
               uiOutput("addForeignKey"),
               size="m",
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
               column(
                  5,
                  fluidRow(h4(tns[1]), class="centerBox"),
                  fluidRow(uiOutput("ilcard"))
               ),
               if(length(tns)==1){
                  column(
                     2,
                     actionButton("confirmAddFK", "Add", disabled=TRUE),
                     tags$br(),
                     icon("long-arrow-alt-right", "fa-2x"),
                     class="centerBox"
                  )
               }else{
                  column(
                     2,
                     actionButton("confirmAddFK", "Add", disabled=TRUE),
                     tags$br(),
                     actionButton(
                        "fkDirection", "",
                        icon=icon("long-arrow-alt-right", "fa-2x")
                     ) %>% div(title="Change foreign key direction"),
                     class="centerBox"
                  )
               },
               column(
                  5,
                  fluidRow(h4(tns[length(tns)]), class="centerBox"),
                  fluidRow(uiOutput("ircard"))
               )
            ),
            tags$hr(class="editSeparator"),
            ##
            fluidRow(uiOutput("fkFields")),
            tags$hr(class="editSeparator"),
            ##
            fluidRow(uiOutput("possibleFkFields"))
         )
      })

      observe({
         if(
            length(foreignKey$fromTable)==0 || length(foreignKey$toTable)==0 ||
            length(foreignKey$fromFields)==0 || length(foreignKey$toFields)==0
         ){
            shinyjs::disable("confirmAddFK")
         }else{
            shinyjs::enable("confirmAddFK")
         }
      })

      observe({
         validate(need(input$fkDirection>0, ""))
         ft <- isolate(foreignKey$toTable)
         tt <- isolate(foreignKey$fromTable)
         validate(need(ft, ""))
         validate(need(tt, ""))
         validate(need(ft!=tt, ""))
         tns <- sort(c(ft, tt))
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
         ft <- foreignKey$fromTable
         tt <- foreignKey$toTable
         validate(need(ft, ""))
         validate(need(tt, ""))
         tns <- sort(c(ft, tt))
         m <- isolate(model$x)
         ftfields <- m[[ft]]$fields$name
         ttfields <- m[[tt]]$fields$name
         toRet <- list(
            column(
               5,
               div(
                  selectInput(
                     "fkFromField", "", ftfields, multiple=FALSE, width="100%",
                     selectize=FALSE
                  ),
                  class="fkFieldSel"
               )
            ),
            column(2, uiOutput("addFkFields"), class="centerBox"),
            column(
               5,
               div(
                  selectInput(
                     "fkToField", "", ttfields, multiple=FALSE, width="100%",
                     selectize=FALSE
                  ),
                  class="fkFieldSel"
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
            return(tagList(
               tags$br(),
               p("Incompatible types", class="errorMessage")
            ))
         }else{
            selFrom <- foreignKey$fromFields
            selTo <- foreignKey$toFields
            alreadyIn <- length(which(selFrom==from & selTo==to))>0
            if(alreadyIn){
               return(tagList(
                  tags$br(),
                  p("Already in key", class="errorMessage")
               ))
            }else{
               return(tagList(
                  tags$br(),
                  actionButton(
                     "addFkField", label="",
                     icon=icon("plus-square", "fa-1x")
                  ) %>% div(title="Add key field")))
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
         fluidRow(
            column(1, ""),
            column(10, DT::DTOutput("fkFieldTable")),
            column(1,
               uiOutput("rmFkField"),
               class="rightBox"
            )
         )
      })
      output$fkFieldTable <- DT::renderDT({
         from <- foreignKey$fromFields
         to <- foreignKey$toFields
         validate(need(from, ""))
         validate(need(to, ""))
         ft <- isolate(foreignKey$fromTable)
         tt <- isolate(foreignKey$toTable)
         tns <- sort(c(ft, tt))
         if(tns[1]==ft){
            left <- from
            right <- to
         }else{
            left <- to
            right <- from
         }
         DT::datatable(
            tibble(l=left, r=right),
            rownames=FALSE,
            colnames=c("", ""),
            options=list(
               dom=ifelse(length(left)>10, "tip", "t"),
               columnDefs = list(
                  list(targets=c(0), visible=TRUE, width='50%'),
                  list(targets=c(1), visible=TRUE, width='50%')
               )
            )
         ) %>%
            DT::formatStyle(1, "text-align"="left") %>%
            DT::formatStyle(2, "text-align"="right")
      })
      output$rmFkField <- renderUI({
         sel <- input$fkFieldTable_rows_selected
         validate(need(sel, ""))
         return(
            actionButton(
               "confirmRmFkField",
               label="",
               icon=icon("minus-square", "fa-1x")
            ) %>% div(title="Remove key field")
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
         m <- isolate(model$x)
         suppressWarnings(
               nm <- m %>%
               add_foreign_key(
                  fromTable=isolate(foreignKey$fromTable),
                  toTable=isolate(foreignKey$toTable),
                  fromFields=isolate(foreignKey$fromFields),
                  toFields=isolate(foreignKey$toFields),
                  fmin=isolate(foreignKey$fmin),
                  fmax=isolate(foreignKey$fmax),
                  tmin=isolate(foreignKey$tmin),
                  tmax=isolate(foreignKey$tmax)
               )
         )
         removeModal()
         if(identical(nm, m)){
            sendError(paste(
               "The foreign key could not be added:",
               "it may already exist or it may not fit other constraints."
            ))
         }else{
            sendWarning(paste(
               "Some indexes, uniqueness and mandatory constraints may",
               "have been added to fields to support the foreign key",
               "cardinalities."
            ))
         }
         model$new <- nm
         foreignKey$fromTable <- NULL
         foreignKey$toTable <- NULL
         foreignKey$fromFields <- NULL
         foreignKey$toFields <- NULL
         foreignKey$fmin <- NULL
         foreignKey$fmax <- NULL
         foreignKey$tmin <- NULL
         foreignKey$tmax <- NULL
      })

      ## _+ Cardinality ----
      output$ilcard <- renderUI({
         ft <- foreignKey$fromTable
         tt <- foreignKey$toTable
         validate(need(ft, ""))
         validate(need(tt, ""))
         tns <- sort(c(ft, tt))
         m <- isolate(model$x)
         cmin <- if(tns[1]==ft) isolate(foreignKey$fmin)
         else isolate(foreignKey$tmin)
         cmax <- if(tns[1]==ft) isolate(foreignKey$fmax)
         else isolate(foreignKey$tmax)
         cmin <- as.character(cmin)
         cmax <- ifelse(cmax==-1, "n", as.character(cmax))
         toRet <- list(
            column(
               6,
               selectInput(
                  "leftcardmin",
                  "Min. card.",
                  choices=c("0", "1"),
                  selected=ifelse(
                     length(cmin)==0,
                     ifelse(tns[1]==ft, "0", "1"),
                     cmin
                  ),
                  selectize=FALSE
               )
            ),
            column(
               6,
               selectInput(
                  "leftcardmax",
                  "Max. card.",
                  choices=c("1", "n"),
                  selected=ifelse(
                     length(cmax)==0,
                     ifelse(tns[1]==ft, "n", "1"),
                     cmax
                  ),
                  selectize=FALSE
               )
            )

         )
         return(toRet)
      })
      output$ircard <- renderUI({
         ft <- foreignKey$fromTable
         tt <- foreignKey$toTable
         validate(need(ft, ""))
         validate(need(tt, ""))
         tns <- sort(c(ft, tt))
         m <- isolate(model$x)
         cmin <- if(tns[1]!=ft) isolate(foreignKey$fmin)
         else isolate(foreignKey$tmin)
         cmax <- if(tns[1]!=ft) isolate(foreignKey$fmax)
         else isolate(foreignKey$tmax)
         cmin <- as.character(cmin)
         cmax <- ifelse(cmax==-1, "n", as.character(cmax))
         toRet <- list(
            column(
               6,
               selectInput(
                  "rightcardmin",
                  "Min. card.",
                  choices=c("0", "1"),
                  selected=ifelse(
                     length(cmin)==0,
                     ifelse(tns[1]!=ft, "0", "1"),
                     cmin
                  ),
                  selectize=FALSE
               )
            ),
            column(
               6,
               selectInput(
                  "rightcardmax",
                  "Max. card.",
                  choices=c("1", "n"),
                  selected=ifelse(
                     length(cmax)==0,
                     ifelse(tns[1]!=ft, "n", "1"),
                     cmax
                  ),
                  selectize=FALSE
               )
            )

         )
         return(toRet)
      })

      observe({
         cval <- c("0"=0L, "1"=1L, "n"=-1L)
         ft <- foreignKey$fromTable
         tt <- foreignKey$toTable
         validate(need(ft, ""))
         validate(need(tt, ""))
         tns <- sort(c(ft, tt))
         lftmin <- input$leftcardmin
         lftmax <- input$leftcardmax
         rgtmin <- input$rightcardmin
         rgtmax <- input$rightcardmax
         validate(need(lftmin,""))
         validate(need(lftmax,""))
         validate(need(rgtmin,""))
         validate(need(rgtmax,""))
         foreignKey$fmin <- as.integer(cval[ifelse(
            tns[1]==ft, lftmin, rgtmin
         )])
         foreignKey$fmax <- as.integer(cval[ifelse(
            tns[1]==ft, lftmax, rgtmax
         )])
         foreignKey$tmin <- as.integer(cval[ifelse(
            tns[1]!=ft, lftmin, rgtmin
         )])
         foreignKey$tmax <- as.integer(cval[ifelse(
            tns[1]!=ft, lftmax, rgtmax
         )])
      })


      #########################################################################@
      ## Update foreign keys ----
      #########################################################################@

      observe({
         validate(need(input$editFK > 0, ""))
         selFK <- isolate(selection$fk)
         validate(need(length(selFK)==1, ""))
         m <- isolate(model$x)
         mne <- isolate(model$vn)$edges
         validate(need(selFK %in% mne$id, ""))
         i <- which(mne$id==selFK)
         foreignKey$fromTable <- mne$from[i]
         foreignKey$toTable <- mne$to[i]
         foreignKey$fromFields <- mne$ff[[i]]
         foreignKey$toFields <- mne$tf[[i]]
         foreignKey$triggered=isolate(foreignKey$triggered)+1
         showModal(modalDialog(
            title="Edit foreign key",
            uiOutput("editForeignKey"),
            size="m",
            easyClose=TRUE
         ))
      })

      output$editForeignKey <- renderUI({
         validate(need(foreignKey$triggered>0, ""))
         ft <- isolate(foreignKey$fromTable)
         tt <- isolate(foreignKey$toTable)
         ff <- isolate(foreignKey$fromFields)
         tf <- isolate(foreignKey$toFields)
         validate(need(ft, ""))
         validate(need(tt, ""))
         validate(need(ff, ""))
         validate(need(tf, ""))
         tns <- sort(c(ft, tt))
         div(
            fluidRow(
               column(
                  5,
                  fluidRow(h4(tns[1]), class="centerBox"),
                  fluidRow(uiOutput("ilcard"))
               ),
               if(tns[1]==ft){
                  column(
                     2,
                     actionButton("confirmUpdateFK", "Update"),
                     tags$br(),
                     icon("long-arrow-alt-right", "fa-2x"),
                     class="centerBox"
                  )
               }else{
                  column(
                     2,
                     actionButton("confirmUpdateFK", "Update"),
                     tags$br(),
                     icon("long-arrow-alt-left", "fa-2x"),
                     class="centerBox"
                  )
               },
               column(
                  5,
                  fluidRow(h4(tns[length(tns)]), class="centerBox"),
                  fluidRow(uiOutput("ircard"))
               )
            )
         )
      })

      observe({
         validate(need(input$confirmUpdateFK > 0, ""))
         model$new <- isolate(model$x) %>%
            update_foreign_key(
               fromTable=isolate(foreignKey$fromTable),
               toTable=isolate(foreignKey$toTable),
               fromFields=isolate(foreignKey$fromFields),
               toFields=isolate(foreignKey$toFields),
               fmin=isolate(foreignKey$fmin),
               fmax=isolate(foreignKey$fmax),
               tmin=isolate(foreignKey$tmin),
               tmax=isolate(foreignKey$tmax)
            )
         removeModal()
         foreignKey$fromTable <- NULL
         foreignKey$toTable <- NULL
         foreignKey$fromFields <- NULL
         foreignKey$toFields <- NULL
         foreignKey$fmin <- NULL
         foreignKey$fmax <- NULL
         foreignKey$tmin <- NULL
         foreignKey$tmax <- NULL
      })


      #########################################################################@
      ## Remove foreign keys ----
      #########################################################################@

      observe({
         takeAction <- (
            (!is.null(input$removeFK) && input$removeFK > 0)
         )
         validate(need(takeAction, ""))
         fks <- isolate(selection$fk)
         m <- isolate(model$x)
         mne <- isolate(model$vn)$edges
         if(length(fks)>0 && all(fks!="") && all(fks %in% mne$id)){
            for(fk in fks){
               i <- which(mne$id==fk)
               m <- m %>% remove_foreign_key(
                  fromTable=mne$from[i],
                  fromFields=mne$ff[[i]],
                  toTable=mne$to[i],
                  toFields=mne$tf[[i]]
               )
            }
            model$new <- m
         }
      })

      #########################################################################@
      ## Node positions ----
      #########################################################################@

      observe({
         dispNodes <- input$modelNet_nodes
         validate(need(dispNodes, ""))
         m <- isolate(model$x)
         cp <- do.call(
            rbind,
            lapply(m, function(n)tibble(x=n$display$x, y=n$display$y))
         )
         np <- do.call(
            rbind,
            lapply(dispNodes[names(m)], function(n)tibble(x=n$x, y=n$y))
         )
         if(!all(np$x==cp$x & np$y==cp$y)){
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
            attr(m, "updateVis") <- FALSE
            model$new <- m
         }
      })

      #########################################################################@
      ## Update model ----
      #########################################################################@

      observe({
         nm <- model$new
         validate(need(nm, ""))
         updateVis <- attr(nm, "updateVis")
         if(is.null(updateVis)){
            updateVis <- TRUE
         }
         attr(nm, "updateVis") <- NULL
         dm <- isolate(model$x)
         validate(need(!identical(nm, dm), ""))

         ##
         tdm <- nm
         if(length(dm)==0){
            toReplot <- TRUE
         }else{
            toReplot <- FALSE
            if(updateVis){
               ndm <- isolate(model$vn)
               ntdm <- modelToVn(tdm, color=isolate(settings$defaultColor))
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
         selTables <- selection$tables
         validate(need(!isolate(selection$fromVN), ""))
         visNetworkProxy("modelNet") %>%
            visSelectNodes(selTables)
      })

      observe({
         selFK <- selection$fk
         selTables <- intersect(names(model$x), selection$tables)
         validate(need(!isolate(selection$fromVN), ""))
         if(length(selTables)==0){
            visNetworkProxy("modelNet") %>%
               visSelectEdges(selFK)
         }
      })

      #########################################################################@
      ## Manage history ----
      #########################################################################@

      observe({
         m <- model$x
         mn <- model$vn
         selection$tables <- sort(intersect(
            isolate(selection$tables), names(m)
         ))
         selection$fk <- sort(intersect(
            isolate(selection$fk), mn$edges$id
         ))
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
         ndm <- isolate(model$vn)
         ntdm <- modelToVn(tdm, color=isolate(settings$defaultColor))
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
         ndm <- isolate(model$vn)
         ntdm <- modelToVn(tdm, color=isolate(settings$defaultColor))
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
            shinyjs::disable("undo")
         }
         if(model$current > 1){
            shinyjs::enable("undo")
         }
         if(model$current >= length(model$history)){
            shinyjs::disable("redo")
         }
         if(model$current < length(model$history)){
            shinyjs::enable("redo")
         }
      })

      #########################################################################@
      ## Export model ----
      #########################################################################@

      observeEvent(input$export, {
         showModal(modalDialog(
            title="Export",
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
               class="exportButtons"
            )),
            column(6, downloadButton(
               "exportHtml",
               list(icon("map", "fa-2x"), "HTML"),
               class="exportButtons"
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
            plot(m, color=isolate(settings$defaultColor)) %>% visSave(file)
         }
      )

      #########################################################################@
      ## From R ----
      #########################################################################@

      if(fromR){
         ## _+ Autosaved object ----
         observe({
            assign(bcko, model$x, envir=modelEnv)
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
#' @param defaultColor a single color indicating the default table color
#' @param availableColors a character of possible colors for tables
#' @param example a file path to an sql or json model
#' @param forceIntro if TRUE the help tour start when the application
#' is launched (default: FALSE)
#'
#' @return The [RelDataModel] designed with the GUI.
#'
#' @import shiny
#' @importFrom stats rbeta runif
#'
#' @export
#'
model_relational_data <- function(
   modelInput=RelDataModel(list()), fromR=interactive(),
   defaultColor="#D9D9D9",
   # availableColors=c(
   #    "#7CE65F", "#D9B661", "#75E4AE", "#8FE6E0",
   #    "#DFFB86", "#FEFE8F", "#A9ECC9", "#F8DEC3",
   #    "#E0B8A0", "#FAC6DC", "#CC8490", "#F67FC4",
   #    "#C6BDF1", "#D8B8E3", "#9BC8FE", "#C6D1DC",
   #    "#9638E5", "#7D76D9", "#6E9DCE"
   # ),
   availableColors=c(
      "#9BC8FE", "#F67FC4", #"#ACF1BC",
      "#C6BDF1", "#DFFB86",
      "#F8DEC3", "#8FE6E0", "#FEFE8F", "#FAC6DC", "#A9ECC9"
   ),
   # availableColors=c(
   #    "#D9B661", "#E14D7D", "#75E4AE", "#9638E5", "#C6D1DC", "#D8B8E3",
   #    "#C663CB", "#7CE65F", "#DF7442", "#E0B8A0", "#D4E355", "#7D76D9",
   #    "#6E9DCE", "#E747D7", "#77DFDE", "#CC8490", "#D991D6", "#D0E9CF",
   #    "#CAE095", "#6E8D65"
   # ),
   example=system.file(
      "examples/HPO-model.json",
      package = utils::packageName()
   ),
   forceIntro=FALSE
){

   bcko <- NA
   if(fromR){
      bcko <- paste0(
         "Model_", format(Sys.time(), format="%Y_%m_%d_%H_%M_%S")
      )
      while(exists(bcko, where=modelEnv)){
         Sys.sleep(1)
         bcko <- paste0(
            "Model_", format(Sys.time(), format="%Y_%m_%d_%H_%M_%S")
         )
      }
   }

   modelInput <- auto_layout(modelInput, lengthMultiplier=300)

   ui <- buildUi(fromR=fromR)
   server <- buildServer(
      modelInput=modelInput, fromR=fromR, bcko=bcko,
      defaultColor=defaultColor, availableColors=availableColors,
      example=example, forceIntro=forceIntro
   )

   if(fromR){
      ## From R ----
      on.exit(
         cat(
            sprintf(
               "The returned model has also been autosaved as %s",
               crayon::green(bcko)
            ),
            sprintf(
               "\n  - Use %s to get it back.",
               crayon::yellow(
                  sprintf('recover_RelDataModel("%s")', bcko)
               )
            ),
            sprintf(
               "\n  - Use %s",
               crayon::yellow("list_autosaved_RelDataModel()")
            ),
            sprintf(
               " and %s",
               crayon::yellow("clean_autosaved_RelDataModels()")
            ),
            " to respectively list and remove all autosaved models.",
            "\n"
         )
      )
      runApp(shinyApp(ui, server))
   }else{
      ## Remote app ----
      shinyApp(ui, server)
   }

}

###############################################################################@
#' Recover an autosaved [RelDataModel]
#'
#' @param name The name of the autosaved [RelDataModel] to bring back.
#' Available autosaved [RelDataModel] can be listed using
#' the [list_autosaved_RelDataModel()]. If NA (default) the latest model is
#' returned.
#'
#' @export
#'
recover_RelDataModel <- function(name=NA){
   if(length(name)>1){
      warning("Several names provided: taking only the first one into account.")
   }
   if(length(name)==0){
      stop("No name provided")
   }
   if(is.na(name)){
      name <- list_autosaved_RelDataModel()
      name <- name[length(name)]
      if(length(name)==0){
         stop("There is not any model to recover")
      }
   }
   get(name, envir=modelEnv)
}

###############################################################################@
#' List autosaved [RelDataModel]
#'
#' @seealso [clean_autosaved_RelDataModels()] to clean this list.
#'
#' @export
#'
list_autosaved_RelDataModel <- function(){
   ls(envir=modelEnv, all.names=TRUE)
}

###############################################################################@
#' Remove all autosaved [RelDataModel]
#'
#' @export
#'
clean_autosaved_RelDataModels <- function(){
   rm(list=list_autosaved_RelDataModel(), envir=modelEnv)
}

###############################################################################@
modelEnv <- new.env(hash=TRUE, parent=emptyenv())
