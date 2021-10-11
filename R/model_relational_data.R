###############################################################################@
buildUi <- function(fromR){

   shiny::addResourcePath(
      "www",
      system.file("www", package=utils::packageName())
   )
   shiny::addResourcePath(
      "doc",
      system.file("doc", package=utils::packageName())
   )

   shiny::shinyUI(shiny::fluidPage(
      title="ReDaMoR",
      windowTitle="ReDaMoR",
      id="MainApp",

      ## Settings ----
      shinyjs::useShinyjs(),
      rintrojs::introjsUI(),

      ## HEAD ----
      shiny::tags$head(
         shiny::tags$link(
            rel="icon",
            href='www/ReDaMoR.png'
         ),
         shiny::tags$link(
            rel="stylesheet", type="text/css", href="www/cerulean.css"
         ),
         shiny::tags$link(
            rel="stylesheet", type="text/css", href="www/defChanges.css"
         ),
         shiny::tags$link(
            rel="stylesheet", type="text/css", href="www/appElements.css"
         ),
         shiny::tags$script(src='www/interactions.js'),
         if(fromR) NULL else shiny::tags$script(src='www/fromWeb.js')
      ),

      ## Main menu ----
      shiny::fluidRow(
         id="mainMenu",
         shiny::column(
            11,
            id="mainLColumn",
            shiny::div(
               id="mainDone",
               if(fromR){
                  shiny::actionButton(
                     "done",
                     list(shiny::icon("check", "fa-2x"), "Done")
                  ) %>%
                     shiny::div(title="Return the model in R session")
               }else{
                  shiny::img(src='www/ReDaMoR.png', id="mainLogo")
               }
            ),
            shiny::div(
               class="mainButton",
               shiny::actionButton(
                  "import",
                  list(shiny::icon("file-import", "fa-2x"), "Import")
               ),
               shiny::actionButton(
                  "export",
                  list("Export", shiny::icon("file-export", "fa-2x"))
               )
            ),
            shiny::div(
               class="mainButton",
               shiny::actionButton(
                  "undo",
                  list("Undo", shiny::icon("undo", "fa-2x"))
               ),
               shiny::actionButton(
                  "redo",
                  list(shiny::icon("redo", "fa-2x"), "Redo")
               ),
               title="Undo (Ctrl+Z) / Redo (Ctrl+Shift+Z)"
            ),
            shiny::div(
               class="mainButton",
               shiny::actionButton(
                  "addTable", "Add table",
                  icon=shiny::icon("plus-square", "fa-2x")
               )
            ),
            shiny::div(
               class="mainButton",
               shiny::uiOutput("modelSummary")
            ),
            shiny::div(
               class="mainButton",
               shiny::actionButton(
                  "doc", "", icon=shiny::icon("question-circle", "fa-2x")
               ) %>% shiny::div(title="Help tour")
            )
         ),
         shiny::column(
            1,
            id="mainRColumn",
            shiny::tags$a(
               "About ReDaMoR",
               href="doc/ReDaMoR.html",
               target="_blank",
               id="about"
            )
         )
      ),

      shiny::fluidRow(

         ## Model view ----
         shiny::column(
            7,
            shiny::fluidRow(
               id="viewMenu",
               shiny::column(
                  8,
                  id="findTableDiv",
                  shiny::selectInput(
                     "findTable",
                     label=NULL,
                     choices=NULL,
                     multiple=TRUE,
                     selected=NULL,
                     width="100%"
                  )
               ),
               shiny::column(
                  4,
                  id="viewButtons",
                  shiny::div(
                     class="viewButton",
                     shiny::actionButton(
                        "selectAll",
                        label=NULL,
                        icon=shiny::icon("object-group", "fa-2x"),
                        class="shrunkenButton"
                     ) %>% shiny::div(title="Select all tables")
                  ),
                  shiny::div(
                     class="viewButton",
                     shiny::actionButton(
                        "autoLayout",
                        label=NULL,
                        icon=shiny::icon("pencil-ruler", "fa-2x"),
                        class="shrunkenButton"
                     ) %>% shiny::div(title="Auto layout the model")
                  ),
                  shiny::div(
                     class="viewButton",
                     shiny::actionButton(
                        "fitNet",
                        label=NULL,
                        icon=shiny::icon("vector-square", "fa-2x"),
                        class="shrunkenButton"
                     ) %>% shiny::div(title="Fit model")
                  )
               )
            ),
            shiny::fluidRow(
               id="modelFrame",
               visNetwork::visNetworkOutput(
                  "modelNet",
                  height="75vh", width="100%"
               )
            )
         ),

         shiny::column(
            5,

            ## Edit menu ----
            shiny::div(
               id="editMenu",
               shiny::uiOutput(
                  "setTableColor",
                  class="editMenuSection"
               ),
               shiny::uiOutput(
                  "dupTablesInput",
                  class="editMenuSection"
               ),
               shiny::uiOutput(
                  "rmTablesInput",
                  class="editMenuSection"
               ),
               shiny::uiOutput(
                  "addFKInput",
                  class="editMenuSection"
               ),
               shiny::uiOutput(
                  "editFKInput",
                  class="editMenuSection"
               ),
               shiny::uiOutput(
                  "rmFKInput",
                  class="editMenuSection"
               )
            ),

            ## Edit table ----
            shiny::uiOutput("editTable")

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
         toRet <- dplyr::as_tibble(x) %>%
            dplyr::select("element", "intro")
         if(!fromR){
            toRet <- toRet %>%
               dplyr::filter(is.na(.data$element) | .data$element!="#done")
         }
         return(toRet)
      }
   )


   function(input, output, session) {

      #########################################################################@
      ## Help tour ----
      #########################################################################@

      context <- shiny::reactiveValues(
         x="main"
      )
      shiny::observe(
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
      shiny::observeEvent(input$doc, {
         docx <- do.call(rbind, rintrosteps[context$x])
         rintrojs::introjs(session, options = list(steps=docx))
      })
      shiny::observeEvent(input$docImp, {
         docx <- do.call(rbind, rintrosteps[c("Import")])
         rintrojs::introjs(session, options = list(steps=docx))
      })

      if(forceIntro){
         rintrojs::introjs(session, options = list(steps=rintrosteps[["main"]]))
      }

      #########################################################################@
      ## Settings ----
      #########################################################################@

      settings <- shiny::reactiveValues()
      settings$defaultColor <- defaultColor
      settings$availableColors <- availableColors
      shiny::observe({
         settings$availableColors <- unique(c(
            settings$defaultColor,
            lapply(model$x, function(x) x$display$color) %>%
               unlist() %>% setdiff(NA),
            shiny::isolate(settings$availableColors)
         ))
      })

      #########################################################################@
      ## The model ----
      #########################################################################@

      model <- shiny::reactiveValues(
         x=modelInput,              # The current model
         vn=modelToVn(modelInput),  # VisNet representation
         new=NULL,                  # A new model to add in history
         history=list(modelInput),  # The model history
         current=1,                 # The position of current model in history
         toImport=NULL,             # Model to import from file
         merged=NULL,               # merge: c(x, toImport)
         table=NULL,                # The table to edit
         indexTable=dplyr::tibble(  # Indexes of the table to edit
            fields=character(),
            unique=logical()
         ),
         fieldTable=dplyr::tibble(  # Fields of the table to edit
            name=character(),
            type=character(),
            nullable=logical(),
            unique=logical(),
            comment=character()
         )
      )
      shiny::observe(
         model$vn <- modelToVn(
            model$x,
            color=shiny::isolate(settings$defaultColor)
         )
      )

      replot <- shiny::reactiveValues(
         x=1                        # Used for triggering model re-plot
      )
      selection <- shiny::reactiveValues(
         release=0,                 # Used for refreshing the visNetwork
         tables=NULL,               # Selected tables
         fk=NULL,                   # Selected foreign keys
         fromVN=FALSE               # Used for refreshing the visNetwork
      )

      #########################################################################@
      ## Notifications ----
      #########################################################################@

      warningMessage <- shiny::reactiveValues(
         n=0,
         message=NULL
      )
      sendWarning <- function(message){
         warningMessage$n <- shiny::isolate(warningMessage$n) + 1
         warningMessage$message <- message
      }
      shiny::observe({
         shiny::req(warningMessage$n>0)
         shiny::showNotification(
            shiny::isolate(warningMessage$message),
            duration=5,
            type="warning"
         )
      })

      errorMessage <- shiny::reactiveValues(
         n=0,
         message=NULL
      )
      sendError <- function(message){
         errorMessage$n <- shiny::isolate(errorMessage$n) + 1
         errorMessage$message <- message
      }
      shiny::observe({
         shiny::req(errorMessage$n>0)
         shiny::showNotification(
            shiny::isolate(errorMessage$message),
            duration=5,
            type="error"
         )
      })

      #########################################################################@
      ## Model view ----
      #########################################################################@

      shiny::observe({
         m <- model$x
         shiny::updateSelectInput(
            session,
            "findTable",
            choices=sort(names(m)),
            selected=intersect(shiny::isolate(selection$tables), names(m))
         )
      })
      shiny::observe({
         selTables <- selection$tables
         shiny::updateSelectInput(
            session,
            "findTable",
            selected=as.character(selTables)
         )
      })
      shiny::observe({
         selTables <- sort(input$findTable)
         shiny::req(!identical(selTables, shiny::isolate(selection$tables)))
         mn <- shiny::isolate(model$vn)
         selFK <- mn$edges %>%
            dplyr::filter(
               .data$from %in% selTables | .data$to %in% selTables
            ) %>%
            dplyr::pull("id")
         selection$fromVN <- FALSE
         if(length(selTables)==0){
            selection$tables <- NULL
         }else{
            selection$tables <- selTables
         }
         selection$release <- shiny::isolate(selection$release)+1
      })
      shiny::observeEvent(input$selectAll, {
         m <- shiny::isolate(model$x)
         shiny::updateSelectInput(
            session,
            "findTable",
            selected=sort(names(m))
         )
      })
      shiny::observeEvent(input$autoLayout, {
         m <- auto_layout(shiny::isolate(model$x), force=TRUE)
         model$new <- m
      })

      output$modelSummary <- shiny::renderUI({
         m <- model$x
         mn <- model$vn
         nt <- length(m)
         nfk <- nrow(mn$edges)
         np <- lapply(m, function(x) nrow(x$fields)) %>% unlist() %>% sum()
         shiny::tagList(
            shiny::tags$strong("Tables:"), nt, "-",
            shiny::tags$strong("Foreign keys:"), nfk, "-",
            shiny::tags$strong("Fields:"), np
         )
      })

      #########################################################################@
      ## Model network ----
      #########################################################################@

      output$modelNet <- visNetwork::renderVisNetwork({
         replot$x
         selection$fromVN <- FALSE
         selection$tables <- NULL
         selection$fk <- NULL
         plot(
            shiny::isolate(model$x),
            color=shiny::isolate(settings$defaultColor)
         ) %>%
            visNetwork::visEvents(release="releaseVn")
      })

      shiny::observe({
         input$modelNet_release
         selection$release <- shiny::isolate(selection$release)+1
      })
      shiny::observe({
         selection$release
         visNetwork::visNetworkProxy("modelNet") %>%
            visNetwork::visGetSelectedNodes()
         visNetwork::visNetworkProxy("modelNet") %>%
            visNetwork::visGetSelectedEdges()
         visNetwork::visNetworkProxy("modelNet") %>%
            visNetwork::visGetNodes()
      })


      modelNet_selectedNodes <- shiny::reactive({
         input$modelNet_selectedNodes
      })
      shiny::observe({
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

      modelNet_selectedEdges <- shiny::reactive({input$modelNet_selectedEdges})
      shiny::observe({
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

      shiny::observeEvent(input$fitNet, {
         visNetwork::visNetworkProxy("modelNet") %>% visNetwork::visFit()
      })

      #########################################################################@
      ## Import model ----
      #########################################################################@

      shiny::observeEvent(input$import, {
         shiny::showModal(shiny::modalDialog(
            title="Import",
            shiny::uiOutput("import"),
            size="l",
            easyClose=TRUE
         ))
      })

      output$import <- shiny::renderUI({
         list(
            shiny::fluidRow(
               shiny::column(
                  6,
                  class="leftBox",
                  shiny::fileInput(
                     "impModel", "Choose an sql or a json file",
                     multiple=FALSE,
                     accept=c(".sql", ".json", ".sql.gz", ".json.gz"),
                     width="100%"
                  )
               ),
               shiny::column(
                  2,
                  class="leftBox",
                  shiny::uiOutput("exampleModel")
               ),
               shiny::column(
                  4,
                  class="rightBox",
                  shiny::div(
                     class="mainButton",
                     shiny::actionButton(
                        "docImp", "",
                        icon=shiny::icon("question-circle", "fa-2x")
                     ),
                     title="Help tour"
                  )
               )
            ),
            shiny::fluidRow(shiny::uiOutput("impModel"))
         )
      })

      ## _+ Import preview ----
      output$impModel <- shiny::renderUI({
         mi <- model$toImport
         shiny::req(!is.null(mi))
         if(!is.RelDataModel(mi)){
            list(shiny::p(mi, class="errorMessage"))
         }else{
            list(
               shiny::div(
                  visNetwork::visNetworkOutput(
                     "impModelNet",
                     height="65vh", width="100%"
                  ),
                  id="impModelFrame"
               ),
               shiny::uiOutput("impMessage")
            )
         }
      })

      output$impModelNet <- visNetwork::renderVisNetwork({
         mi <- model$toImport
         shiny::req(mi)
         plot(mi, color=shiny::isolate(settings$defaultColor))
      })

      shiny::observe({
         mi <- model$toImport
         shiny::req(mi)
         m <- shiny::isolate(model$x)
         mm <- try(c(m, mi), silent=TRUE)
         model$merged <- mm
      })

      output$impMessage <- shiny::renderUI({
         mm <- model$merged
         if(is.RelDataModel(mm)){
            shiny::actionButton(
               "importValidate",
               list(
                  shiny::icon("file-import", "fa-2x"),
                  "Merge with current model"
               )
            )
         }else{
            list(shiny::p(mm, class="errorMessage"))
         }
      })

      ## _+ From model ----
      shiny::observe({
         fi <- input$impModel
         shiny::req(fi)
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
      output$exampleModel <- shiny::renderUI({
         shiny::req(file.exists(example))
         m <- try(read_json_data_model(example), silent=TRUE)
         if(!is.RelDataModel(m)){
            m <- try(read_SQL_data_model(example), silent=TRUE)
         }
         shiny::req(m)
         shiny::actionLink("exampleLink", label="Try an example")
      })
      shiny::observeEvent(input$exampleLink, {
         shiny::req(file.exists(example))
         mi <- try(read_json_data_model(example), silent=TRUE)
         if(!is.RelDataModel(mi)){
            mi <- try(read_SQL_data_model(example), silent=TRUE)
         }
         shiny::req(mi)
         model$toImport <-  auto_layout(mi, lengthMultiplier=45*length(mi))
      })

      ## _+ Validate import ----
      shiny::observe({
         shiny::req(input$importValidate)
         mm <- shiny::isolate(model$merged)
         shiny::req(mm)

         cmn <- shiny::isolate(model$vn)
         if(
            !is.null(cmn$nodes)>0 && nrow(cmn$nodes)>0 &&
            all(!is.na(cmn$nodes$x)) && all(!is.na(cmn$nodes$y))
         ){
            toReplot <- FALSE
            cmxrange <- c(min(cmn$nodes$x), max(cmn$nodes$x))
            cmyrange <- c(min(cmn$nodes$x), max(cmn$nodes$y))
            shiny::req(shiny::isolate(model$toImport))
            tin <- modelToVn(
               shiny::isolate(model$toImport),
               color=shiny::isolate(settings$defaultColor)
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
         shiny::removeModal()
      })

      #########################################################################@
      ## Add table ----
      #########################################################################@

      shiny::observeEvent(input$addTable, {
         shiny::showModal(shiny::modalDialog(
            title="Add table",
            shiny::div(
               shiny::fluidRow(
                  shiny::column(
                     10,
                     shiny::textInput(
                        "newTableName", label=NULL, width="100%",
                        placeholder="Table name"
                     )
                  ),
                  shiny::column(
                     2,
                     shiny::actionButton("confirmAddTable", "Add")
                  )
               ),
               shiny::fluidRow(shiny::uiOutput("newTableExists"))
            ),
            size="s",
            easyClose=TRUE
         ))
      })

      output$newTableExists <- shiny::renderUI({
         ntn <- input$newTableName
         m <- shiny::isolate(model$x)
         if(ntn %in% names(m)){
            shiny::p("Table name already used", class="errorMessage")
         }else{
            list()
         }
      })

      shiny::observe({
         ntn <- input$newTableName
         m <- shiny::isolate(model$x)
         if(is.null(ntn) || ntn=="" || ntn %in% names(m)){
            shinyjs::disable("confirmAddTable")
         }else{
            shinyjs::enable("confirmAddTable")
         }
      })

      shiny::observe({
         shiny::req(input$confirmAddTable)
         tn <- shiny::isolate(input$newTableName)
         m <- shiny::isolate(model$x)
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
            shiny::removeModal()
         }
      })

      #########################################################################@
      ## Edit table ----
      #########################################################################@

      shiny::observe({
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

      output$editTable <- shiny::renderUI({
         mt <- model$table
         shiny::req(mt)
         selTable <- mt$tableName
         shiny::div(
            shiny::fluidRow(
               shiny::column(8, shiny::h3(selTable)),
               shiny::column(
                  4,
                  class="rightBox",
                  shiny::actionButton("renameTable", "Rename") %>%
                     shiny::div(title="Rename the table")
               )
            ),
            shiny::uiOutput("tableCommentUI"),
            shiny::tags$hr(class="editSeparator"),
            shiny::uiOutput("fields"),
            shiny::tags$hr(class="editSeparator"),
            shiny::uiOutput("primaryKey"),
            shiny::tags$hr(class="editSeparator"),
            shiny::uiOutput("indexes")
         )
      })

      ## _+ Rename table ----
      shiny::observeEvent(input$renameTable, {
         shiny::showModal(shiny::modalDialog(
            title="Rename table",
            shiny::div(
               shiny::fluidRow(
                  shiny::column(
                     10,
                     shiny::textInput(
                        "tableNewName", label=NULL, width="100%",
                        placeholder="Table new name"
                     )
                  ),
                  shiny::column(
                     2,
                     shiny::actionButton("confirmRenameTable", "Rename")
                  )
               ),
               shiny::fluidRow(shiny::uiOutput("newNameExists"))
            ),
            size="s",
            easyClose=TRUE
         ))
      })
      output$newNameExists <- shiny::renderUI({
         ntn <- input$tableNewName
         m <- shiny::isolate(model$x)
         if(ntn %in% names(m)){
            shiny::p("Table name already used", class="errorMessage")
         }else{
            list()
         }
      })
      shiny::observe({
         ntn <- input$tableNewName
         m <- shiny::isolate(model$x)
         if(is.null(ntn) || ntn=="" || ntn %in% names(m)){
            shinyjs::disable("confirmRenameTable")
         }else{
            shinyjs::enable("confirmRenameTable")
         }
      })
      shiny::observe({
         shiny::req(input$confirmRenameTable)
         tn <- shiny::isolate(input$tableNewName)
         mt <- shiny::isolate(model$table)
         shiny::req(mt)
         selTable <- mt$tableName
         m <- shiny::isolate(model$x)
         if(!is.null(tn) && tn!="" && !tn %in% names(m)){
            m <- rename_table(m, old=selTable, new=tn)
            model$new <- m
            shiny::removeModal()
         }
      })

      ## _+ Table commment ----
      output$tableCommentUI <- shiny::renderUI({
         mt <- model$table
         shiny::req(mt)
         selTable <- mt$tableName
         shiny::fluidRow(
            shiny::column(
               8,
               shiny::textAreaInput(
                  "tableComment",
                  label=NULL,
                  value=ifelse(
                     is.na(mt$display$comment), "", mt$display$comment
                  ),
                  width="100%",
                  placeholder="Table description"
               )
            ),
            shiny::column(
               4,
               shiny::uiOutput("refreshCommentH", class="updateHighlight"),
               shiny::actionButton(
                  "refreshComment",
                  label=NULL,
                  icon=shiny::icon("check", "fa-1x"),
                  class="disabled"
               ) %>% shiny::div(title="Update table comment", class="iblock"),
               class="rightBox"
            )
         )
      })
      shiny::observe({
         input$refreshComment
         ntn <- input$tableComment
         shiny::req(length(ntn)>0)
         mt <- model$table
         shiny::req(mt)
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
      output$refreshCommentH <- shiny::renderUI({
         input$refreshComment
         ntn <- input$tableComment
         shiny::req(length(ntn)>0)
         mt <- model$table
         shiny::req(mt)
         selTable <- mt$tableName
         cc <- mt$display$comment
         ntn <- ifelse(is.na(ntn), "", ntn)
         cc <- ifelse(is.na(cc), "", cc)
         shiny::req(ntn!=cc)
         shiny::icon("arrow-right", "fa-1x")
      })
      shiny::observe({
         input$refreshComment
         nc <- shiny::isolate(input$tableComment)
         shiny::req(!is.na(nc))
         if(nc==""){
            nc <- NA
         }
         mt <- shiny::isolate(model$table)
         shiny::req(mt)
         selTable <- mt$tableName
         cc <- mt$display$comment
         m <- shiny::isolate(model$x)
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
      output$fields <- shiny::renderUI({
         mt <- model$table
         shiny::req(mt)
         selTable <- mt$tableName
         list(
            shiny::fluidRow(
               shiny::column(6, shiny::h4("Fields")),
               shiny::column(
                  6,
                  shiny::uiOutput("updateFieldDiv", inline=TRUE),
                  if(is.MatrixModel(mt)){
                     NULL
                  }else{
                     shiny::actionButton(
                        "addField", label="",
                        icon=shiny::icon("plus-square", "fa-1x"),
                        class="shrunkenButton"
                     ) %>%
                        shiny::div(
                           title="Add a new field",
                           class="iblock"
                        )
                  },
                  class="rightBox"
               )
            ),
            shiny::fluidRow(
               shiny::column(12, DT::DTOutput("fieldTable"))
            ),
            shiny::uiOutput("fieldCommentDisplay")
         )
      })
      output$fieldTable <- DT::renderDT({
         mt <- model$table
         # shiny::req(mt)
         # selTable <- mt$tableName
         shiny::isolate(model$fieldTable) %>%
            dplyr::select(-"comment") %>%
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
      shiny::observe({
         mt <- model$table
         shiny::req(mt)
         selTable <- mt$tableName
         model$fieldTable <- mt$fields
      })
      shiny::observe({
         DT::replaceData(
            proxyFieldTable,
            data=model$fieldTable %>% dplyr::select(-"comment"),
            clearSelection="all"
         )
      })
      # ## __- Display field comment
      output$fieldCommentDisplay <- shiny::renderUI({
         seli <- input$fieldTable_rows_selected
         shiny::req(length(seli)==1)
         shiny::req(nrow(model$fieldTable)>0)
         mt <- shiny::isolate(model$table)
         shiny::req(mt)
         selTable <- mt$tableName
         shiny::req(nrow(mt$fields)>0)
         shiny::req(seli>=1 & seli <= nrow(mt$fields))
         shiny::p(mt$fields$comment[seli])
      })
      # ## __- Modify fields ----
      output$updateFieldDiv <- shiny::renderUI({
         seli <- input$fieldTable_rows_selected
         shiny::req(length(seli)==1)
         shiny::req(nrow(model$fieldTable)>0)
         mt <- shiny::isolate(model$table)
         shiny::req(mt)
         selTable <- mt$tableName
         shiny::req(nrow(mt$fields)>0)
         shiny::req(seli>=1 & seli <= nrow(mt$fields))
         shiny::div(
            shiny::actionButton(
               "moveFieldUp",
               label="",
               icon=shiny::icon("arrow-alt-circle-up", "fa-1x"),
               class="shrunkenButton"
            ) %>%
               shiny::div(
                  title="Move up",
                  class="iblock"
               ),
            shiny::actionButton(
               "moveFieldDown",
               label="",
               icon=shiny::icon("arrow-alt-circle-down", "fa-1x"),
               class="shrunkenButton"
            ) %>%
               shiny::div(
                  title="Move up",
                  class="iblock"
               ),
            shiny::actionButton(
               "updateField",
               label="",
               icon=shiny::icon("edit", "fa-1x"),
               class="shrunkenButton"
            ) %>%
               shiny::div(
                  title="Edit field properties",
                  class="iblock"
               ),
            if(is.MatrixModel(mt)){
               NULL
            }else{
               shiny::actionButton(
                  "removeField",
                  label="",
                  icon=shiny::icon("minus-square", "fa-1x"),
                  class="shrunkenButton"
               ) %>% shiny::div(title="Remove field", class="iblock")
            },
            class="iblock"
         )
      })
      # ## __- Remove field ----
      shiny::observe({
         shiny::req(input$removeField>0)
         seli <- shiny::isolate(input$fieldTable_rows_selected)
         shiny::req(length(seli)==1)
         mt <- shiny::isolate(model$table)
         shiny::req(mt)
         selTable <- mt$tableName
         m <- shiny::isolate(model$x)
         shiny::req(nrow(mt$fields)>0)
         fn <- mt$fields$name[seli]
         m <- try(m %>% remove_field(
            tableName=selTable,
            fieldName=fn
         ), silent=TRUE)
         if(is.RelDataModel(m)){
            model$new <- m
         }else{
            shiny::showModal(shiny::modalDialog(
               title="Unable to remove field",
               shiny::p(
                  shiny::HTML(paste(
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
      shiny::observe({
         shiny::req(input$moveFieldUp>0)
         seli <- shiny::isolate(input$fieldTable_rows_selected)
         shiny::req(length(seli)==1)
         mt <- shiny::isolate(model$table)
         shiny::req(mt)
         selTable <- mt$tableName
         m <- shiny::isolate(model$x)
         shiny::req(nrow(mt$fields)>0)
         fn <- mt$fields$name[seli]
         alli <- 1:nrow(m[[selTable]]$fields)
         fi <- which(m[[selTable]]$fields$name==fn)
         shiny::req(fi>1)
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
      shiny::observe({
         shiny::req(input$moveFieldDown>0)
         seli <- shiny::isolate(input$fieldTable_rows_selected)
         shiny::req(length(seli)==1)
         mt <- shiny::isolate(model$table)
         shiny::req(mt)
         selTable <- mt$tableName
         m <- shiny::isolate(model$x)
         shiny::req(nrow(mt$fields)>0)
         fn <- mt$fields$name[seli]
         alli <- 1:nrow(m[[selTable]]$fields)
         fi <- which(m[[selTable]]$fields$name==fn)
         shiny::req(fi<length(alli))
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
      shiny::observeEvent(input$addField, {
         mt <- shiny::isolate(model$table)
         shiny::req(mt)
         selTable <- mt$tableName
         fields <- mt$fields
         shiny::showModal(shiny::modalDialog(
            title="Add field",
            shiny::fluidRow(
               shiny::column(
                  10,
                  shiny::textInput(
                     "newFieldName", label="Field",
                     placeholder="Field name",
                     width="100%"
                  ),
                  shiny::uiOutput("existingNewField"),
                  shiny::selectInput(
                     "newFieldType", label="Type",
                     choices=if(nrow(fields)==0){
                        c(SUPPTYPES, "row", "column")
                     }else{
                        SUPPTYPES
                     },
                     selected=NULL, multiple=FALSE
                  ),
                  shiny::checkboxInput(
                     "newFieldNullable", label="Nullable?",
                     value=FALSE,
                  ),
                  shiny::checkboxInput(
                     "newFieldUnique", label="Unique?",
                     value=FALSE,
                  ),
                  shiny::textAreaInput(
                     "newFieldComment", label="Comment",
                     placeholder=paste(
                        'Field description.',
                        'If base64, you should start with the extension of the',
                        'file between {} (e.g. "{png}", "{html}", "{zip}"...)'
                     ),
                     width="100%"
                  )
               ),
               shiny::column(
                  2,
                  shiny::actionButton("confirmAddField", "Add")
               )
            ),
            size="s",
            easyClose=TRUE
         ))
      })
      shiny::observe({
         nfn <- input$newFieldName
         mt <- shiny::isolate(model$table)
         shiny::req(mt)
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
      output$existingNewField <- shiny::renderUI({
         mt <- shiny::isolate(model$table)
         shiny::req(mt)
         selTable <- mt$tableName
         fields <- mt$fields
         shiny::req(input$newFieldName %in% fields$name)
         shiny::p("Field name already used", class="errorMessage")
      })
      shiny::observeEvent(input$confirmAddField, {
         mt <- shiny::isolate(model$table)
         shiny::req(mt)
         selTable <- mt$tableName
         fields <- mt$fields
         nfn <- shiny::isolate(input$newFieldName)
         shiny::req(
            !is.null(nfn) &&
            nfn!="" &&
            !nfn %in% fields$name
         )
         nm <- shiny::isolate(model$x) %>%
            add_field(
               tableName=selTable,
               name=nfn,
               type=shiny::isolate(input$newFieldType),
               nullable=shiny::isolate(input$newFieldNullable),
               unique=shiny::isolate(input$newFieldUnique),
               comment=as.character(shiny::isolate(input$newFieldComment))
            )
         if(!identical(nm, shiny::isolate(model$x))){
            model$new <- nm
         }
         shiny::removeModal()
      })

      ## __- Update field ----
      shiny::observeEvent(input$updateField, {
         seli <- shiny::isolate(input$fieldTable_rows_selected)
         shiny::req(length(seli)==1)
         mt <- shiny::isolate(model$table)
         shiny::req(mt)
         selTable <- mt$tableName
         fields <- mt$fields
         shiny::req(nrow(fields)>0)
         shiny::showModal(shiny::modalDialog(
            title="Update field",
            shiny::fluidRow(
               shiny::column(
                  10,
                  shiny::textInput(
                     "fieldName", label="Field",
                     value=fields$name[seli],
                     placeholder="Field name",
                     width="100%"
                  ),
                  shiny::uiOutput("existingField"),
                  shiny::selectInput(
                     "fieldType", label="Type",
                     choices=if(fields$type[seli] %in% c("row", "column")){
                        c("row", "column")
                     }else{
                        if(is.MatrixModel(mt)){
                           setdiff(SUPPTYPES, "base64")
                        }else{
                           SUPPTYPES
                        }
                     },
                     selected=fields$type[seli], multiple=FALSE
                  ),
                  if(fields$type[seli] %in% c("row", "column")){
                     NULL
                  }else{
                     shiny::checkboxInput(
                        "fieldNullable", label="Nullable?",
                        value=fields$nullable[seli],
                     )
                  },
                  if(fields$type[seli] %in% c("row", "column")){
                     NULL
                  }else{
                     shiny::checkboxInput(
                        "fieldUnique", label="Unique?",
                        value=fields$unique[seli],
                     )
                  },
                  shiny::textAreaInput(
                     "fieldComment", label="Comment",
                     value=fields$comment[seli],
                     placeholder=paste(
                        'Field description.',
                        'If base64, you should start with the extension of the',
                        'file between {} (e.g. "{png}", "{html}", "{zip}"...)'
                     ),
                     width="100%"
                  ),
                  shiny::uiOutput("updateFieldError")
               ),
               shiny::column(
                  2,
                  shiny::actionButton("confirmUpdateField", "Update")
               )
            ),
            size="s",
            easyClose=TRUE
         ))
      })
      shiny::observe({
         nfn <- input$fieldName
         seli <- shiny::isolate(input$fieldTable_rows_selected)
         shiny::req(length(seli)==1)
         mt <- shiny::isolate(model$table)
         shiny::req(mt)
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
      output$existingField <- shiny::renderUI({
         nfn <- input$fieldName
         seli <- shiny::isolate(input$fieldTable_rows_selected)
         shiny::req(length(seli)==1)
         mt <- shiny::isolate(model$table)
         shiny::req(mt)
         selTable <- mt$tableName
         fields <- mt$fields
         shiny::req(nfn %in% fields$name[-seli])
         shiny::p("Field name already used", class="errorMessage")
      })
      updateField <- shiny::reactiveValues(error=NULL)
      shiny::observeEvent(input$confirmUpdateField, {
         seli <- shiny::isolate(input$fieldTable_rows_selected)
         shiny::req(length(seli)==1)
         mt <- shiny::isolate(model$table)
         shiny::req(mt)
         selTable <- mt$tableName
         fields <- mt$fields
         nfn <- shiny::isolate(input$fieldName)
         shiny::req(
            !is.null(nfn) &&
               nfn!="" &&
               !nfn %in% fields$name[-seli]
         )
         nm <- shiny::isolate(model$x)
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
               type=shiny::isolate(input$fieldType),
               nullable=shiny::isolate(input$fieldNullable),
               unique=shiny::isolate(input$fieldUnique),
               comment=as.character(shiny::isolate(input$fieldComment))
            ), silent=TRUE)
         if(is.RelDataModel(nm)){
            updateField$error <- NULL
            if(!identical(nm, shiny::isolate(model$x))){
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
            shiny::removeModal()
         }else{
            updateField$error <- nm
         }
      })
      output$updateFieldError <- shiny::renderUI({
         e <- updateField$error
         shiny::req(!is.null(e))
         shiny::p(e, class="errorMessage")
      })


      ## _+ Table primary key ----
      output$primaryKey <- shiny::renderUI({
         mt <- model$table
         shiny::req(mt)
         selTable <- mt$tableName
         fnames <- mt$fields$name
         shiny::req(fnames)
         shiny::fluidRow(
            shiny::column(3, shiny::h4("Primary key")),
            shiny::column(
               5,
               shiny::selectInput(
                  "primaryKey", label=NULL,
                  choices=fnames,
                  selected=mt$primaryKey,
                  multiple=TRUE,
                  width="100%"
               )
            ),
            shiny::column(
               4,
               shiny::uiOutput("refreshPKH", class="updateHighlight"),
               shiny::actionButton(
                  "refreshPrimaryKey",
                  label=NULL,
                  icon=shiny::icon("check", "fa-1x"),
                  class="disabled"
               ) %>% shiny::div(
                  title="Update table primary key",
                  class="iblock"
               ),
               class="rightBox"
            )
         )
      })
      shiny::observe({
         input$refreshPrimaryKey
         npk <- input$primaryKey
         mt <- shiny::isolate(model$table)
         shiny::req(mt)
         selTable <- mt$tableName
         cpk <- mt$primaryKey
         if(length(cpk)!=length(npk) || any(sort(cpk)!=sort(npk))){
            shinyjs::enable("refreshPrimaryKey")
         }else{
            shinyjs::disable("refreshPrimaryKey")
         }
         if(is.MatrixModel(mt)){
            shinyjs::disable("primaryKey")
         }else{
            shinyjs::enable("primaryKey")
         }
      })
      output$refreshPKH <- shiny::renderUI({
         input$refreshPrimaryKey
         npk <- input$primaryKey
         mt <- shiny::isolate(model$table)
         shiny::req(mt)
         selTable <- mt$tableName
         cpk <- mt$primaryKey
         shiny::req(
            length(cpk)!=length(npk) || any(sort(cpk)!=sort(npk)),
         )
         shiny::icon("arrow-right", "fa-1x")
      })
      shiny::observe({
         shiny::req(input$refreshPrimaryKey>0)
         npk <- shiny::isolate(input$primaryKey)
         # npk <- input$primaryKey
         mt <- shiny::isolate(model$table)
         shiny::req(mt)
         selTable <- mt$tableName
         m <- shiny::isolate(model$x)
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
      output$indexes <- shiny::renderUI({
         mt <- model$table
         shiny::req(mt)
         selTable <- mt$tableName
         fnames <- mt$fields$name
         shiny::req(fnames)
         list(
            shiny::fluidRow(
               shiny::column(6, shiny::h4("Indexes")),
               shiny::column(
                  6,
                  shiny::uiOutput("updateIndexDiv", inline=TRUE),
                  shiny::actionButton(
                     "addIndex", label="",
                     icon=shiny::icon("plus-square", "fa-1x"),
                     class="shrunkenButton"
                  ) %>%
                     shiny::div(title="Add an index", class="iblock"),
                  class="rightBox"
               )
            ),
            shiny::fluidRow(
               shiny::column(12, DT::DTOutput("indexTable"))
            )
         )
      })
      output$indexTable <- DT::renderDT({
         mt <- model$table
         # shiny::req(mt)
         # selTable <- mt$tableName
         shiny::isolate(model$indexTable) %>%
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
      shiny::observe({
         mt <- model$table
         shiny::req(mt)
         selTable <- mt$tableName
         if(length(mt$indexes)>0){
            indexTable <- mt$indexes %>%
               lapply(function(x){
                  dplyr::tibble(
                     Fields=sprintf("[%s]", paste(x$fields, collapse="], [")),
                     Unique=x$unique
                  )
               })
            model$indexTable <- do.call(rbind, indexTable)
         }else{
            model$indexTable <- dplyr::tibble(
               Fields=character(),
               Unique=logical()
            )
         }
      })
      shiny::observe({
         DT::replaceData(
            proxyIndexTable,
            data=model$indexTable,
            clearSelection="all"
         )
      })
      ## __- Update index ----
      output$updateIndexDiv <- shiny::renderUI({
         seli <- input$indexTable_rows_selected
         shiny::req(length(seli)==1)
         shiny::req(nrow(model$indexTable)>0)
         mt <- shiny::isolate(model$table)
         shiny::req(mt)
         selTable <- mt$tableName
         shiny::req(length(mt$indexes)>0)
         shiny::req(seli>=1 & seli <= length(mt$indexes))
         ui <- mt$indexes[[seli]]$unique
         list(
            shiny::actionButton(
               "updateIndex",
               label="",
               icon=shiny::icon("edit", "fa-1x"),
               class="shrunkenButton"
            ) %>%
               shiny::div(
                  title="Update index properties",
                  class="iblock"
               ),
            shiny::actionButton(
               "removeIndex",
               label="",
               icon=shiny::icon("minus-square", "fa-1x"),
               class="shrunkenButton"
            ) %>%
               shiny::div(
                  title="Remove index",
                  class="iblock"
               )
         )
      })
      shiny::observeEvent(input$updateIndex,{
         seli <- shiny::isolate(input$indexTable_rows_selected)
         shiny::req(length(seli)==1)
         shiny::req(nrow(model$indexTable)>0)
         mt <- shiny::isolate(model$table)
         shiny::req(mt)
         selTable <- mt$tableName
         shiny::req(length(mt$indexes)>0)
         shiny::req(seli>=1 & seli <= length(mt$indexes))
         ui <- mt$indexes[[seli]]$unique
         shiny::showModal(shiny::modalDialog(
            title="Update field",
            shiny::checkboxInput("setUniqueIndex", "Unique?", value=ui),
            size="s",
            easyClose=TRUE
         ))
      })
      shiny::observe({
         ui <- input$setUniqueIndex
         shiny::req(!is.null(ui) && !is.na(ui))
         seli <- shiny::isolate(input$indexTable_rows_selected)
         shiny::req(length(seli)==1)
         mt <- shiny::isolate(model$table)
         shiny::req(mt)
         selTable <- mt$tableName
         shiny::req(length(mt$indexes)>0)
         m <- shiny::isolate(model$x)
         if(mt$indexes[[seli]]$unique!=ui){
            m <- m %>% set_unique_index(
               tableName=selTable,
               fieldNames=mt$indexes[[seli]]$fields,
               unique=ui
            )
            if(!identical(m, shiny::isolate(model$x))){
               model$new <- m
            }else{
               sendError(paste(
                  "The index could not be modified because it is required",
                  "for primary or foreign keys."
               ))
            }
            shiny::removeModal()
         }
      })
      ## __- Remove index ----
      shiny::observe({
         shiny::req(input$removeIndex>0)
         seli <- shiny::isolate(input$indexTable_rows_selected)
         shiny::req(length(seli)==1)
         mt <- shiny::isolate(model$table)
         shiny::req(mt)
         selTable <- mt$tableName
         shiny::req(length(mt$indexes)>0)
         m <- shiny::isolate(model$x)
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
      shiny::observeEvent(input$addIndex, {
         mt <- shiny::isolate(model$table)
         shiny::req(mt)
         selTable <- mt$tableName
         fnames <- mt$fields$name
         shiny::showModal(shiny::modalDialog(
            title="Add index",
            shiny::div(
               shiny::fluidRow(
                  shiny::column(
                     8,
                     shiny::selectInput(
                        "newIndexFields", label=NULL,
                        choices=fnames,
                        multiple=TRUE
                     )
                  ),
                  shiny::column(
                     2,
                     shiny::checkboxInput(
                        "uniqueNewIndex", "Unique?",
                        value=FALSE
                     )
                  ),
                  shiny::column(
                     2,
                     shiny::actionButton("confirmAddIndex", "Add")
                  )
               )
            ),
            size="s",
            easyClose=TRUE
         ))
      })
      shiny::observe({
         if(length(input$newIndexFields)==0){
            shinyjs::disable("confirmAddIndex")
         }else{
            shinyjs::enable("confirmAddIndex")
         }
      })
      shiny::observeEvent(input$confirmAddIndex, {
         mt <- shiny::isolate(model$table)
         shiny::req(mt)
         selTable <- mt$tableName
         shiny::req(length(input$newIndexFields)>0)
         nm <- shiny::isolate(model$x) %>%
            add_index(
               tableName=selTable,
               fieldNames=input$newIndexFields,
               unique=input$uniqueNewIndex
            )
         if(!identical(nm, shiny::isolate(model$x))){
            model$new <- nm
         }else{
            sendError(paste(
               "The index could not be added:",
               "it may already exist or it may not fit other constraints."
            ))
         }
         shiny::removeModal()
      })

      #########################################################################@
      ## Edit menu ----
      #########################################################################@

      output$addFKInput <- shiny::renderUI({
         selTable <- selection$tables
         shiny::req(length(selTable)>0 & length(selTable)<=2)
         shiny::actionButton(
            "addForeignKey", "Key",
            icon=shiny::icon("plus", "fa-2x"),
            # icon=shiny::icon("external-link-alt", "fa-2x"),
            class="shrunkenButton"
         ) %>% shiny::div(
            title="Add a foreign key"
         )
      })
      output$editFKInput <- shiny::renderUI({
         selFK <- selection$fk
         shiny::req(length(selFK)==1)
         shiny::actionButton(
            "editFK",
            label=shiny::HTML(paste(
               '<i class="far fa-edit fa-2x"></i>',
               'keys'
            )),
            class="shrunkenButton"
         ) %>% shiny::div(
            title="Edit cardinalities of the selected foreign key (F2)"
         )
      })
      output$rmFKInput <- shiny::renderUI({
         selFK <- selection$fk
         shiny::req(length(selFK)>0)
         shiny::actionButton(
            "removeFK",
            label=shiny::HTML(paste(
               '<i class="far fa-trash-alt fa-2x"></i>',
               'keys'
            )),
            class="shrunkenButton"
         ) %>% shiny::div(
            title="Remove selected foreign keys (del)"
         )
      })
      output$rmTablesInput <- shiny::renderUI({
         selTable <- selection$tables
         shiny::req(length(selTable)>0)
         shiny::actionButton(
            "removeTables",
            label=shiny::HTML(paste(
               '<i class="fas fa-trash fa-2x"></i>',
               'tables'
            )),
            class="shrunkenButton"
         ) %>% shiny::div(
            title="Remove selected tables (del)"
         )
      })
      output$dupTablesInput <- shiny::renderUI({
         selTable <- selection$tables
         shiny::req(length(selTable)>0)
         shiny::actionButton(
            "duplicateTables",
            label=shiny::HTML(paste(
               '<i class="fas fa-copy fa-2x"></i>',
               'tables'
            )),
            class="shrunkenButton"
         ) %>% shiny::div(
            title="Duplicate selected tables"
         )
      })

      #########################################################################@
      ## Table color ----
      #########################################################################@

      output$setTableColor <- shiny::renderUI({
         selTables <- selection$tables
         shiny::req(length(selTables)>0)
         tval <- lapply(
            shiny::isolate(model$x),
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
               "", shiny::isolate(settings$availableColors)
            ),
            allowTransparent=TRUE
         ) %>% shiny::div(
            id="tabColPick",
            title="Select table color"
         )
      })
      shiny::observe({
         newCol <- input$tableColor
         shiny::req(newCol!="")
         selTables <- shiny::isolate(selection$tables)
         shiny::req(length(selTables)>0)
         m <- shiny::isolate(model$x)
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

      shiny::observe({
         takeAction <- (
            (!is.null(input$removeTables) && input$removeTables > 0)
         )
         shiny::req(takeAction)
         tns <- shiny::isolate(selection$tables)
         m <- shiny::isolate(model$x)
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
               shiny::showModal(shiny::modalDialog(
                  title="Unable to remove table",
                  shiny::p(
                     shiny::HTML(paste(
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

      shiny::observe({
         takeAction <- (
            (!is.null(input$duplicateTables) && input$duplicateTables > 0)
         )
         shiny::req(takeAction)
         tns <- shiny::isolate(selection$tables)
         m <- shiny::isolate(model$x)
         vn <- shiny::isolate(model$vn)
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

      foreignKey <- shiny::reactiveValues(
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

      shiny::observe({
         shiny::req(input$addForeignKey > 0)
         tns <- shiny::isolate(selection$tables)
         m <- shiny::isolate(model$x)
         foreignKey$fromTable <- foreignKey$toTable <-
            foreignKey$fromFields <- foreignKey$toFields <- NULL
         foreignKey$triggered=shiny::isolate(foreignKey$triggered)+1
         if(length(tns)>0 && all(tns!="") && all(tns %in% names(m))){
            shiny::showModal(shiny::modalDialog(
               title="Add foreign key",
               shiny::uiOutput("addForeignKey"),
               size="m",
               easyClose=TRUE
            ))
         }
      })

      output$addForeignKey <- shiny::renderUI({
         tns <- selection$tables
         shiny::req(length(tns)>0)
         shiny::req(foreignKey$triggered>0)
         foreignKey$fromTable <- tns[1]
         foreignKey$toTable <- tns[length(tns)]
         shiny::div(
            shiny::fluidRow(
               shiny::column(
                  5,
                  shiny::fluidRow(shiny::h4(tns[1]), class="centerBox"),
                  shiny::fluidRow(shiny::uiOutput("ilcard"))
               ),
               if(length(tns)==1){
                  shiny::column(
                     2,
                     shiny::actionButton("confirmAddFK", "Add", disabled=TRUE),
                     shiny::tags$br(),
                     shiny::icon("long-arrow-alt-right", "fa-2x"),
                     class="centerBox"
                  )
               }else{
                  shiny::column(
                     2,
                     shiny::actionButton("confirmAddFK", "Add", disabled=TRUE),
                     shiny::tags$br(),
                     shiny::actionButton(
                        "fkDirection", "",
                        icon=shiny::icon("long-arrow-alt-right", "fa-2x")
                     ) %>% shiny::div(title="Change foreign key direction"),
                     class="centerBox"
                  )
               },
               shiny::column(
                  5,
                  shiny::fluidRow(
                     shiny::h4(tns[length(tns)]),
                     class="centerBox"
                  ),
                  shiny::fluidRow(shiny::uiOutput("ircard"))
               )
            ),
            shiny::tags$hr(class="editSeparator"),
            ##
            shiny::fluidRow(shiny::uiOutput("fkFields")),
            shiny::tags$hr(class="editSeparator"),
            ##
            shiny::fluidRow(shiny::uiOutput("possibleFkFields"))
         )
      })

      shiny::observe({
         if(
            length(foreignKey$fromTable)==0 || length(foreignKey$toTable)==0 ||
            length(foreignKey$fromFields)==0 || length(foreignKey$toFields)==0
         ){
            shinyjs::disable("confirmAddFK")
         }else{
            shinyjs::enable("confirmAddFK")
         }
      })

      shiny::observe({
         shiny::req(input$fkDirection>0)
         ft <- shiny::isolate(foreignKey$toTable)
         tt <- shiny::isolate(foreignKey$fromTable)
         shiny::req(ft)
         shiny::req(tt)
         shiny::req(ft!=tt)
         tns <- sort(c(ft, tt))
         foreignKey$fromTable <- ft
         foreignKey$toTable <- tt
         foreignKey$fromFields <- foreignKey$toFields <- NULL
         if(ft==tns[1]){
            shiny::updateActionButton(
               session, "fkDirection",
               icon=shiny::icon("long-arrow-alt-right", "fa-2x")
            )
         }else{
            shiny::updateActionButton(
               session, "fkDirection",
               icon=shiny::icon("long-arrow-alt-left", "fa-2x")
            )
         }
      })

      output$possibleFkFields <- shiny::renderUI({
         ft <- foreignKey$fromTable
         tt <- foreignKey$toTable
         shiny::req(ft)
         shiny::req(tt)
         tns <- sort(c(ft, tt))
         m <- shiny::isolate(model$x)
         if(is.MatrixModel(m[[ft]])){
            ftfields <- m[[ft]]$fields$name[which(
               m[[ft]]$fields$type %in% c("row", "column")
            )]
         }else{
            ftfields <- m[[ft]]$fields$name
         }
         if(is.MatrixModel(m[[tt]])){
            ttfields <- m[[tt]]$fields$name[which(
               m[[tt]]$fields$type %in% c("row", "column")
            )]
         }else{
            ttfields <- m[[tt]]$fields$name
         }
         toRet <- list(
            shiny::column(
               5,
               shiny::div(
                  shiny::selectInput(
                     "fkFromField", "", ftfields, multiple=FALSE, width="100%",
                     selectize=FALSE
                  ),
                  class="fkFieldSel"
               )
            ),
            shiny::column(2, shiny::uiOutput("addFkFields"), class="centerBox"),
            shiny::column(
               5,
               shiny::div(
                  shiny::selectInput(
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

      output$addFkFields <- shiny::renderUI({
         ft <- foreignKey$fromTable
         tt <- foreignKey$toTable
         shiny::req(ft)
         shiny::req(tt)
         m <- shiny::isolate(model$x)
         ftfields <- m[[ft]]$fields
         ttfields <- m[[tt]]$fields
         from <- input$fkFromField
         to <- input$fkToField
         shiny::req(from %in% ftfields$name)
         shiny::req(to %in% ttfields$name)
         fft <- ftfields[which(ftfields$name==from),]$type
         fft <- ifelse(fft %in% c("row", "column"), "character", fft)
         tft <- ttfields[which(ttfields$name==to),]$type
         tft <- ifelse(tft %in% c("row", "column"), "character", tft)
         if(fft != tft){
            return(shiny::tagList(
               shiny::tags$br(),
               shiny::p("Incompatible types", class="errorMessage")
            ))
         }else{
            if(fft=="base64"){
               return(shiny::tagList(
                  shiny::tags$br(),
                  shiny::p(
                     "base64 fields cannot be used in foreign keys",
                     class="errorMessage"
                  )
               ))
            }else{
               selFrom <- foreignKey$fromFields
               selTo <- foreignKey$toFields
               alreadyIn <- length(which(selFrom==from & selTo==to))>0
               if(alreadyIn){
                  return(shiny::tagList(
                     shiny::tags$br(),
                     shiny::p("Already in key", class="errorMessage")
                  ))
               }else{
                  return(shiny::tagList(
                     shiny::tags$br(),
                     shiny::actionButton(
                        "addFkField", label="",
                        icon=shiny::icon("plus-square", "fa-1x")
                     ) %>% shiny::div(title="Add key field")))
               }
            }
         }
      })

      shiny::observe({
         shiny::req(input$addFkField>0)
         foreignKey$fromFields <- c(
            shiny::isolate(foreignKey$fromFields),
            shiny::isolate(input$fkFromField)
         )
         foreignKey$toFields <- c(
            shiny::isolate(foreignKey$toFields),
            shiny::isolate(input$fkToField)
         )
      })

      output$fkFields <- shiny::renderUI({
         from <- foreignKey$fromFields
         to <- foreignKey$toFields
         shiny::req(from)
         shiny::req(to)
         shiny::fluidRow(
            shiny::column(1, ""),
            shiny::column(10, DT::DTOutput("fkFieldTable")),
            shiny::column(1,
               shiny::uiOutput("rmFkField"),
               class="rightBox"
            )
         )
      })
      output$fkFieldTable <- DT::renderDT({
         from <- foreignKey$fromFields
         to <- foreignKey$toFields
         shiny::req(from)
         shiny::req(to)
         ft <- shiny::isolate(foreignKey$fromTable)
         tt <- shiny::isolate(foreignKey$toTable)
         tns <- sort(c(ft, tt))
         if(tns[1]==ft){
            left <- from
            right <- to
         }else{
            left <- to
            right <- from
         }
         DT::datatable(
            dplyr::tibble(l=left, r=right),
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
      output$rmFkField <- shiny::renderUI({
         sel <- input$fkFieldTable_rows_selected
         shiny::req(sel)
         return(
            shiny::actionButton(
               "confirmRmFkField",
               label="",
               icon=shiny::icon("minus-square", "fa-1x")
            ) %>% shiny::div(title="Remove key field")
         )
      })
      shiny::observe({
         shiny::req(input$confirmRmFkField)
         sel <- shiny::isolate(input$fkFieldTable_rows_selected)
         shiny::req(length(sel)>0)
         foreignKey$fromFields <- shiny::isolate(foreignKey$fromFields)[-sel]
         foreignKey$toFields <- shiny::isolate(foreignKey$toFields)[-sel]
      })

      shiny::observe({
         shiny::req(input$confirmAddFK > 0)
         m <- shiny::isolate(model$x)
         suppressWarnings(
               nm <- try({
                  m %>%
                     add_foreign_key(
                        fromTable=shiny::isolate(foreignKey$fromTable),
                        toTable=shiny::isolate(foreignKey$toTable),
                        fromFields=shiny::isolate(foreignKey$fromFields),
                        toFields=shiny::isolate(foreignKey$toFields),
                        fmin=shiny::isolate(foreignKey$fmin),
                        fmax=shiny::isolate(foreignKey$fmax),
                        tmin=shiny::isolate(foreignKey$tmin),
                        tmax=shiny::isolate(foreignKey$tmax)
                     )
               }, silent=TRUE)
         )
         if(inherits(nm, "try-error")){
            sendError(as.character(nm))
         }else{
            shiny::removeModal()
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
         }
      })

      ## _+ Cardinality ----
      output$ilcard <- shiny::renderUI({
         ft <- foreignKey$fromTable
         tt <- foreignKey$toTable
         shiny::req(ft)
         shiny::req(tt)
         tns <- sort(c(ft, tt))
         m <- shiny::isolate(model$x)
         cmin <- if(tns[1]==ft) shiny::isolate(foreignKey$fmin)
         else shiny::isolate(foreignKey$tmin)
         cmax <- if(tns[1]==ft) shiny::isolate(foreignKey$fmax)
         else shiny::isolate(foreignKey$tmax)
         cmin <- as.character(cmin)
         cmax <- ifelse(cmax==-1, "n", as.character(cmax))
         toRet <- list(
            shiny::column(
               6,
               shiny::selectInput(
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
            shiny::column(
               6,
               shiny::selectInput(
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
      output$ircard <- shiny::renderUI({
         ft <- foreignKey$fromTable
         tt <- foreignKey$toTable
         shiny::req(ft)
         shiny::req(tt)
         tns <- sort(c(ft, tt))
         m <- shiny::isolate(model$x)
         cmin <- if(tns[1]!=ft) shiny::isolate(foreignKey$fmin)
         else shiny::isolate(foreignKey$tmin)
         cmax <- if(tns[1]!=ft) shiny::isolate(foreignKey$fmax)
         else shiny::isolate(foreignKey$tmax)
         cmin <- as.character(cmin)
         cmax <- ifelse(cmax==-1, "n", as.character(cmax))
         toRet <- list(
            shiny::column(
               6,
               shiny::selectInput(
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
            shiny::column(
               6,
               shiny::selectInput(
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

      shiny::observe({
         cval <- c("0"=0L, "1"=1L, "n"=-1L)
         ft <- foreignKey$fromTable
         tt <- foreignKey$toTable
         shiny::req(ft)
         shiny::req(tt)
         tns <- sort(c(ft, tt))
         lftmin <- input$leftcardmin
         lftmax <- input$leftcardmax
         rgtmin <- input$rightcardmin
         rgtmax <- input$rightcardmax
         shiny::req(lftmin)
         shiny::req(lftmax)
         shiny::req(rgtmin)
         shiny::req(rgtmax)
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

      shiny::observe({
         shiny::req(input$editFK > 0)
         selFK <- shiny::isolate(selection$fk)
         shiny::req(length(selFK)==1)
         m <- shiny::isolate(model$x)
         mne <- shiny::isolate(model$vn)$edges
         shiny::req(selFK %in% mne$id)
         i <- which(mne$id==selFK)
         foreignKey$fromTable <- mne$from[i]
         foreignKey$toTable <- mne$to[i]
         foreignKey$fromFields <- mne$ff[[i]]
         foreignKey$toFields <- mne$tf[[i]]
         foreignKey$triggered=shiny::isolate(foreignKey$triggered)+1
         shiny::showModal(shiny::modalDialog(
            title="Edit foreign key",
            shiny::uiOutput("editForeignKey"),
            size="m",
            easyClose=TRUE
         ))
      })

      output$editForeignKey <- shiny::renderUI({
         shiny::req(foreignKey$triggered>0)
         ft <- shiny::isolate(foreignKey$fromTable)
         tt <- shiny::isolate(foreignKey$toTable)
         ff <- shiny::isolate(foreignKey$fromFields)
         tf <- shiny::isolate(foreignKey$toFields)
         shiny::req(ft)
         shiny::req(tt)
         shiny::req(ff)
         shiny::req(tf)
         tns <- sort(c(ft, tt))
         shiny::div(
            shiny::fluidRow(
               shiny::column(
                  5,
                  shiny::fluidRow(shiny::h4(tns[1]), class="centerBox"),
                  shiny::fluidRow(shiny::uiOutput("ilcard"))
               ),
               if(tns[1]==ft){
                  shiny::column(
                     2,
                     shiny::actionButton("confirmUpdateFK", "Update"),
                     shiny::tags$br(),
                     shiny::icon("long-arrow-alt-right", "fa-2x"),
                     class="centerBox"
                  )
               }else{
                  shiny::column(
                     2,
                     shiny::actionButton("confirmUpdateFK", "Update"),
                     shiny::fluidPage$br(),
                     shiny::icon("long-arrow-alt-left", "fa-2x"),
                     class="centerBox"
                  )
               },
               shiny::column(
                  5,
                  shiny::fluidRow(
                     shiny::h4(tns[length(tns)]),
                     class="centerBox"
                  ),
                  shiny::fluidRow(shiny::uiOutput("ircard"))
               )
            )
         )
      })

      shiny::observe({
         shiny::req(input$confirmUpdateFK > 0)
         model$new <- shiny::isolate(model$x) %>%
            update_foreign_key(
               fromTable=shiny::isolate(foreignKey$fromTable),
               toTable=shiny::isolate(foreignKey$toTable),
               fromFields=shiny::isolate(foreignKey$fromFields),
               toFields=shiny::isolate(foreignKey$toFields),
               fmin=shiny::isolate(foreignKey$fmin),
               fmax=shiny::isolate(foreignKey$fmax),
               tmin=shiny::isolate(foreignKey$tmin),
               tmax=shiny::isolate(foreignKey$tmax)
            )
         shiny::removeModal()
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

      shiny::observe({
         takeAction <- (
            (!is.null(input$removeFK) && input$removeFK > 0)
         )
         shiny::req(takeAction)
         fks <- shiny::isolate(selection$fk)
         m <- shiny::isolate(model$x)
         mne <- shiny::isolate(model$vn)$edges
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

      shiny::observe({
         dispNodes <- input$modelNet_nodes
         shiny::req(dispNodes)
         m <- shiny::isolate(model$x)
         cp <- do.call(
            rbind,
            lapply(m, function(n)dplyr::tibble(x=n$display$x, y=n$display$y))
         )
         np <- do.call(
            rbind,
            lapply(dispNodes[names(m)], function(n)dplyr::tibble(x=n$x, y=n$y))
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

      shiny::observe({
         nm <- model$new
         shiny::req(nm)
         updateVis <- attr(nm, "updateVis")
         if(is.null(updateVis)){
            updateVis <- TRUE
         }
         attr(nm, "updateVis") <- NULL
         dm <- shiny::isolate(model$x)
         shiny::req(!identical(nm, dm))

         ##
         tdm <- nm
         if(length(dm)==0){
            toReplot <- TRUE
         }else{
            toReplot <- FALSE
            if(updateVis){
               ndm <- shiny::isolate(model$vn)
               ntdm <- modelToVn(
                  tdm,
                  color=shiny::isolate(settings$defaultColor)
               )
               edgeToDel <- setdiff(ndm$edges$id, ntdm$edges$id)
               if(length(edgeToDel)>0){
                  visNetwork::visNetworkProxy("modelNet") %>%
                     visNetwork::visRemoveEdges(edgeToDel)
               }
               nodeToDel <- setdiff(names(dm), names(tdm))
               if(length(nodeToDel)>0){
                  visNetwork::visNetworkProxy("modelNet") %>%
                     visNetwork::visRemoveNodes(nodeToDel)
               }
               visNetwork::visNetworkProxy("modelNet") %>%
                  visNetwork::visUpdateNodes(ntdm$nodes) %>%
                  visNetwork::visUpdateEdges(ntdm$edges)
            }
         }
         ##

         ch <- shiny::isolate(model$history)
         cm <- shiny::isolate(model$current)
         ch <- ch[-((cm:length(ch))+1)]
         ch <- c(ch, list(nm))
         cm <- cm+1
         model$history <- ch
         model$current <- cm
         model$x <- nm
         model$new <- NULL
         if(toReplot){
            replot$x <- shiny::isolate(replot$x)+1
         }
      })

      shiny::observe({
         selTables <- selection$tables
         shiny::req(!shiny::isolate(selection$fromVN))
         visNetwork::visNetworkProxy("modelNet") %>%
            visNetwork::visSelectNodes(selTables)
      })

      shiny::observe({
         selFK <- selection$fk
         selTables <- intersect(names(model$x), selection$tables)
         shiny::req(!shiny::isolate(selection$fromVN))
         if(length(selTables)==0){
            visNetwork::visNetworkProxy("modelNet") %>%
               visNetwork::visSelectEdges(selFK)
         }
      })

      #########################################################################@
      ## Manage history ----
      #########################################################################@

      shiny::observe({
         m <- model$x
         mn <- model$vn
         selection$tables <- sort(intersect(
            shiny::isolate(selection$tables), names(m)
         ))
         selection$fk <- sort(intersect(
            shiny::isolate(selection$fk), mn$edges$id
         ))
      })

      shiny::observe({
         shiny::req(input$undo)
         ch <- shiny::isolate(model$history)
         cm <- shiny::isolate(model$current)
         cm <- cm -1
         shiny::req(cm>0)

         ###########################@
         ## The commented code below was first used to
         ## replot the network from scratch
         # model$x <- ch[[cm]]
         # model$current <- cm
         # replot$x <- shiny::isolate(replot$x)+1
         ###########################@

         dm <- shiny::isolate(model$x)
         tdm <- ch[[cm]]
         ndm <- shiny::isolate(model$vn)
         ntdm <- modelToVn(tdm, color=shiny::isolate(settings$defaultColor))
         edgeToDel <- setdiff(ndm$edges$id, ntdm$edges$id)
         if(length(edgeToDel)>0){
            visNetwork::visNetworkProxy("modelNet") %>%
               visNetwork::visRemoveEdges(edgeToDel)
         }
         nodeToDel <- setdiff(names(dm), names(tdm))
         if(length(nodeToDel)>0){
            visNetwork::visNetworkProxy("modelNet") %>%
               visNetwork::visRemoveNodes(nodeToDel)
         }
         visNetwork::visNetworkProxy("modelNet") %>%
            visNetwork::visUpdateNodes(ntdm$nodes) %>%
            visNetwork::visUpdateEdges(ntdm$edges)
         model$x <- tdm
         model$current <- cm
      })

      shiny::observe({
         shiny::req(input$redo)
         ch <- shiny::isolate(model$history)
         cm <- shiny::isolate(model$current)
         cm <- cm +1
         shiny::req(cm<=length(ch))

         ###########################@
         ## The commented code below was first used to
         ## replot the network from scratch
         # model$x <- ch[[cm]]
         # model$current <- cm
         # replot$x <- shiny::isolate(replot$x)+1
         ###########################@

         dm <- shiny::isolate(model$x)
         tdm <- ch[[cm]]
         ndm <- shiny::isolate(model$vn)
         ntdm <- modelToVn(tdm, color=shiny::isolate(settings$defaultColor))
         edgeToDel <- setdiff(ndm$edges$id, ntdm$edges$id)
         if(length(edgeToDel)>0){
            visNetwork::visNetworkProxy("modelNet") %>%
               visNetwork::visRemoveEdges(edgeToDel)
         }
         nodeToDel <- setdiff(names(dm), names(tdm))
         if(length(nodeToDel)>0){
            visNetwork::visNetworkProxy("modelNet") %>%
               visNetwork::visRemoveNodes(nodeToDel)
         }
         visNetwork::visNetworkProxy("modelNet") %>%
            visNetwork::visUpdateNodes(ntdm$nodes) %>%
            visNetwork::visUpdateEdges(ntdm$edges)
         model$x <- tdm
         model$current <- cm
      })

      shiny::observe({
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

      shiny::observeEvent(input$export, {
         shiny::showModal(shiny::modalDialog(
            title="Export",
            shiny::uiOutput("export"),
            size="s",
            easyClose=TRUE
         ))
      })

      output$export <- shiny::renderUI({
         shiny::fluidRow(
            shiny::column(6, shiny::downloadButton(
               "exportJson",
               list(shiny::icon("file-code", "fa-2x"), "JSON"),
               class="exportButtons"
            )),
            shiny::column(6, shiny::downloadButton(
               "exportHtml",
               list(shiny::icon("map", "fa-2x"), "HTML"),
               class="exportButtons"
            ))
         )
      })

      ## _+ JSON ----
      output$exportJson <- shiny::downloadHandler(
         filename = function() {
            paste0("Data-model", ".json")
         },
         content = function(file) {
            m <- shiny::isolate(model$x)
            shiny::req(m)
            write_json_data_model(m, file)
         }
      )

      ## _+ HTML ----
      output$exportHtml <- shiny::downloadHandler(
         filename = function() {
            paste0("Data-model", ".html")
         },
         content = function(file) {
            m <- shiny::isolate(model$x)
            shiny::req(m)
            plot(m, color=shiny::isolate(settings$defaultColor)) %>%
               visNetwork::visSave(file)
         }
      )

      #########################################################################@
      ## From R ----
      #########################################################################@

      if(fromR){
         ## _+ Autosaved object ----
         shiny::observe({
            assign(bcko, model$x, envir=modelEnv)
         })
         ## _+ Done button ----
         shiny::observeEvent(input$done, {
            shiny::stopApp(invisible(model$x))
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
      shiny::runApp(shiny::shinyApp(ui, server))
   }else{
      ## Remote app ----
      shiny::shinyApp(ui, server)
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
