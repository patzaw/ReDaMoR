###############################################################################@
buildUi <- function(fromR){
   shinyUI(fluidPage(
      title="ReDaMoR",

      ## Settings ----
      useShinyjs(),
      rintrojs::introjsUI(),

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
                if ($("#tableNewName").is(":focus") * (event.key == "Enter")) {
                    $("#confirmRenameTable").click();
                }
            });
            // $(document).keyup(function(event) {
            //     if ($("#tableComment").is(":focus") * (event.key == "Enter")) {
            //     console.log("here");
            //         $("#refreshComment").click();
            //     }
            // });
            '
         ),
         if(fromR) NULL else tags$script('
            window.onbeforeunload = function() {
               return "Your changes will be lost!";
            };
         '
         )
      ),

      ## Main menu ----
      fluidRow(
         div(
            style="display:inline-block; margin-left:0px; margin-right:10px",
            if(fromR){
               actionButton(
                  "done",
                  list(icon("check", "fa-2x"), "Done")
               ) %>%
                  div(title="Return the model in R session")
            }else{
               img(
                  src=paste(
                     "data:image/png;base64,",
                     base64enc::base64encode(system.file(
                        "www/ReDaMoR.png",
                        package = packageName()
                     ))
                  ),
                  style="height:75px;"
               )
            }
         ),
         div(
            style="display:inline-block; margin-left:5px; margin-right:10px",
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
            style="display:inline-block; margin-left:5px; margin-right:10px",
            actionButton(
               "undo",
               list("Undo", icon("undo", "fa-2x"))
            ),
            actionButton(
               "redo",
               list(icon("redo", "fa-2x"), "Redo")
            )
         ),
         div(
            style="display:inline-block; margin-left:5px; margin-right:10px",
            actionButton(
               "addTable", "Add table",
               icon=icon("plus-square", "fa-2x")
            )
         ),
         div(
            style="display:inline-block; margin-left:5px; margin-right:10px",
            uiOutput("modelSummary")
         ),
         div(
            style="display:inline-block; margin-left:5px; margin-right:5px;",
            actionButton(
               "doc", "", icon=icon("question-circle", "fa-2x")
            ) %>% div(title="Documentation")
         ),
         style="margin-top:25px;margin-bottom:20px;"
      ),

      fluidRow(

         ## Model view ----
         column(
            7,
            fluidRow(
               column(
                  9,
                  style="padding-left:0;",
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
                  1,
                  style="padding-left:0; text-align:left;",
                  div(actionButton(
                     "selectAll",
                     label=NULL,
                     icon=icon("object-group", "fa-2x")
                  ), title="Select all tables"),
                  tags$style(HTML(
                     "#selectAll{padding-top:2px;padding-bottom:2px;}"
                  ))
               ),
               column(
                  1,
                  style="padding-left:0; padding-right:0; text-align:center;",
                  div(actionButton(
                     "autoLayout",
                     label=NULL,
                     icon=icon("pencil-ruler", "fa-2x")
                  ), title="Auto layout the model"),
                  tags$style(HTML(
                     "#autoLayout{padding-top:2px;padding-bottom:2px;}"
                  ))
               ),
               column(
                  1,
                  style="text-align:right; padding-left:0; padding-right:0;",
                  div(actionButton(
                     "fitNet",
                     label=NULL,
                     icon=icon("vector-square", "fa-2x")
                  ), title="Fit model"),
                  tags$style(HTML(
                     "#fitNet{padding-top:2px;padding-bottom:2px;}"
                  ))
               ),
               style="margin-bottom:0;"
            ),
            fluidRow(
               visNetworkOutput("modelNet", height="75vh", width="100%"),
               style="border:solid; min-height:75vh;"
            )
         ),

         column(
            5,

            ## Multiple tables ----
            div(
               uiOutput(
                  "setTableColor",
                  style="display: inline-block; margin-right:15px; width:75px;"
               ),
               uiOutput(
                  "addFKInput",
                  style="display: inline-block; margin-right:15px;"
               ),
               uiOutput(
                  "rmFKInput",
                  style="display: inline-block; margin-right:15px;"),
               uiOutput(
                  "rmTablesInput",
                  style="display: inline-block;"
               ),
               style="padding-bottom:5px; text-align:center;"
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
   example
){

   rintrosteps <- jsonlite::fromJSON(system.file(
      "Documentation/rintrojs-steps.json",
      package = packageName()
   )) %>% lapply(
      function(x){
         toRet <- as_tibble(x) %>%
            select(element, intro)
         if(!fromR){
            toRet <- toRet %>%
               filter(is.na(element) | element!="#done")
         }
         return(toRet)
      }
   )


   function(input, output, session) {

      #########################################################################@
      ## Documentation ----
      #########################################################################@

      context <- reactiveValues(
         x="main"
      )
      observeEvent(input$doc, {
         docx <- rintrosteps[context$x] %>%
            do.call(rbind, .)
         rintrojs::introjs(session, options = list(steps=docx))
      })
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
            }
            context$x <- ctxt
         }
      )
      observeEvent(input$docImp, {
         docx <- rintrosteps[c("Import")] %>%
            do.call(rbind, .)
         rintrojs::introjs(session, options = list(steps=docx))
      })

      rintrojs::introjs(session, options = list(steps=rintrosteps[["main"]]))

      #########################################################################@
      ## The model ----
      #########################################################################@

      model <- reactiveValues(
         x=modelInput,              # The current model
         new=NULL,                  # A new model to add in history
         history=list(modelInput),  # The model history
         current=1,                 # The position of current model in history
         toImport=NULL,             # Model to import from file
         merged=NULL,               # merge: c(x, toImport)
         table=NULL,
         indexTable=tibble(
            fields=character(),
            unique=logical()
         ),
         fieldTable=tibble(
            name=character(),
            type=character(),
            nullable=logical(),
            unique=logical(),
            comment=character()
         )
      )
      replot <- reactiveValues(
         x=1                        # Replot the model
      )
      selection <- reactiveValues(
         release=0,
         tables=NULL,               # Selected tables
         fk=NULL,                   # Selected foreign keys
         fromVN=FALSE
      )

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
      ## Model overview ----
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
         mn <- isolate(model$x) %>% modelToVn()
         selFK <- mn$edges %>%
            filter(from %in% selTables | to %in% selTables) %>%
            pull(id)
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
         mn <- modelToVn(m)
         nt <- length(m)
         nfk <- nrow(mn$edges)
         np <- lapply(m, function(x) nrow(x$fields)) %>% unlist() %>% sum()
         p(
            tags$strong("Tables:"), nt, "-",
            tags$strong("Foreign keys:"), nfk, "-",
            tags$strong("Fields:"), np,
            style=paste(
               "padding:5px;",
               "border:solid; border-radius:8px;",
               "background:#E3E3E3;"
            )
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
            visEvents(
               release="function(nodes) {
                  Shiny.onInputChange('modelNet_release', Math.random());
               }"
            )
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
      })# %>% debounce(500)
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

      modelNet_selectedEdges <- reactive({input$modelNet_selectedEdges}) %>%
         debounce(500)
      observe({
         selFK <- intersect(
            modelNet_selectedEdges(),
            modelToVn(model$x, color=isolate(settings$defaultColor))$edges$id
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
                  div(fileInput(
                     "impModel", "Choose an sql or a json file",
                     multiple=FALSE,
                     accept=c(".sql", ".json", ".sql.gz", ".json.gz"),
                     width="100%"
                  ), id="impModelCol"),
                  style="text-align:left;"
               ),
               column(
                  2,
                  uiOutput("exampleModel"),
                  style="text-align:left;"
               ),
               # column(
               #    4,
               #    div(fileInput(
               #       "infModel",
               #       paste(
               #          "Choose data files from which",
               #          "the data model should be inferred."
               #       ),
               #       multiple=TRUE,
               #       accept=c(".txt", ".csv", ".tsv"),
               #       width="100%"
               #    ), id="infModelCol"),
               #    style="text-align:left;"
               # ),
               column(
                  4,
                  div(
                     style="display:inline-block; margin-right:5px;",
                     actionButton(
                        "docImp", "", icon=icon("question-circle", "fa-2x")
                     ) %>% div(title="Documentation")
                  ),
                  style="text-align:right;"
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
               list(icon("file-import", "fa-2x"), "Merge with current model"),
               style="margin-top:25px;margin-bottom:25px;"
            )
         }else{
            list(p(mm, style="color:red;font-weight: bold;"))
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

      # ## _+ From data files ----
      # observe({
      #    fi <- input$infModel
      #    validate(need(fi, ""))
      #    fiext <- regexpr(
      #       "(\\.[[:alnum:]]+)(\\.gz)?$", fi$name, ignore.case=TRUE
      #    )
      #    fiext <- substr(
      #       fi$name, fiext, fiext+attr(fiext, "match.length")-1
      #    ) %>% tolower() %>% unique()
      #    if(length(fiext)==0){
      #       model$toImport <- paste(
      #          "Selected files have several extensions.",
      #          "You should select files with the same extension:",
      #          ".csv (comma separated values),",
      #          "tsv or .txt (tab separated values)"
      #       )
      #    }
      #    validate(need(length(fiext)==1, ""))
      #    supportedExt <- c(".csv", ".tsv", ".txt")
      #    if(!fiext %in% supportedExt){
      #       model$toImport <- paste(
      #          "File extension should be one of the following:",
      #          ".csv (comma separated values),",
      #          "tsv or .txt (tab separated values)"
      #       )
      #    }
      #    validate(need(fiext %in% supportedExt, ""))
      #    if(fiext==".csv"){
      #       delim=","
      #    }
      #    if(fiext %in% c(".tsv", ".txt")){
      #       delim="\t"
      #    }
      #    dfEnvir <- new.env()
      #    for(i in 1:nrow(fi)){
      #       tv <- try(
      #          suppressMessages(
      #             readr::read_delim(file=fi$datapath[i], delim=delim)
      #          ),
      #          silent=TRUE
      #       )
      #       if(!is.data.frame(tv)){
      #          dfEnvir <- tv
      #          break()
      #       }else{
      #          assign(
      #             sub("(\\.[[:alnum:]]+)(\\.gz)?$", "", fi$name[i]),
      #             tv,
      #             envir=dfEnvir
      #          )
      #       }
      #    }
      #    if(!is.environment(dfEnvir)){
      #       model$toImport <- dfEnvir
      #    }else{
      #       model$toImport <- df_to_model(
      #          list=ls(envir=dfEnvir, all.names=TRUE),
      #          envir=dfEnvir
      #       ) %>%
      #          auto_layout(lengthMultiplier=45*length(.))
      #    }
      # })

      ## _+ Validate import ----
      observe({
         validate(need(input$importValidate, ""))
         mm <- isolate(model$merged)
         validate(need(mm, ""))

         cmn <- modelToVn(
            isolate(model$x), color=isolate(settings$defaultColor)
         )
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
               column(8, h3(selTable, style="margin-top:6px;")),
               column(
                  4,
                  actionButton("renameTable", "Rename") %>%
                     div(title="Rename the table"),
                  style="text-align:right;"
               )
            ),
            uiOutput("tableCommentUI"),
            tags$hr(style="border-color:#317EAC; border-width:3px;"),
            uiOutput("fields"),
            tags$hr(style="border-color:#317EAC; border-width:3px;"),
            uiOutput("primaryKey"),
            tags$hr(style="border-color:#317EAC; border-width:3px;"),
            uiOutput("indexes"),
            style=paste(
               "border:solid;", #"border-radius:15px;",
               "max-height:75vh;",
               "overflow:scroll;",
               "padding:15px;"
            )
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
            p("Table name already used", style="color:red;font-weight: bold;")
         }else{
            list()
         }
      })
      observe({
         ntn <- input$tableNewName
         m <- isolate(model$x)
         if(is.null(ntn) || ntn=="" || ntn %in% names(m)){
            disable("confirmRenameTable")
         }else{
            enable("confirmRenameTable")
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
               10,
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
               2,
               actionButton(
                  "refreshComment",
                  label=NULL,
                  icon=icon("check", "fa-1x"),
                  class="btn btn-default action-button shiny-bound-input disabled"
               ) %>% div(title="Update table comment"),
               style="text-align:right;"
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
            disable("refreshComment")
         }else{
            enable("refreshComment")
         }
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
                     icon=icon("plus-square", "fa-1x")
                  ) %>%
                     div(
                        title="Add a new field",
                        style="display:inline-block;"
                     ),
                  style="text-align:right;"
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
            select(-comment) %>%
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
            data=model$fieldTable %>% select(-comment),
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
         p(
            mt$fields$comment[seli],
            style="font-weight: bold; padding-top:5px; padding-bottom:5px;"
         )
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
               "updateField",
               label="",
               icon=icon("edit", "fa-1x")
            ) %>%
               div(
                  title="Edit field properties",
                  style="display:inline-block;"
               ),
            actionButton(
               "removeField",
               label=HTML(paste(
                  '<i class="fa fa-minus-square fa-1x" style="color:red;">',
                  '</i>'
               ))
            ) %>% div(title="Remove field", style="display:inline-block;"),
            style="display:inline-block;"
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
                     sprintf(
                        "%s is used in a foreign key.",
                        sprintf("<strong>%s</strong>", fn)
                     ),
                     "<br>Remove foreign keys before removing this fields."
                  )),
                  style="color:red;"#font-weight: bold;"
               ),
               size="s",
               easyClose=TRUE
            ))
         }
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
            disable("confirmAddField")
         }else{
            enable("confirmAddField")
         }
      })
      output$existingNewField <- renderUI({
         mt <- isolate(model$table)
         validate(need(mt, ""))
         selTable <- mt$tableName
         fields <- mt$fields
         validate(need(input$newFieldName %in% fields$name, ""))
         p("Field name already used", style="color:red;font-weight: bold;")
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
            disable("confirmUpdateField")
         }else{
            enable("confirmUpdateField")
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
         p("Field name already used", style="color:red;font-weight: bold;")
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
            }
            removeModal()
         }else{
            updateField$error <- nm
         }
      })
      output$updateFieldError <- renderUI({
         e <- updateField$error
         validate(need(!is.null(e), ""))
         p(e, style="color:red;")
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
               7,
               selectInput(
                  "primaryKey", label=NULL,
                  choices=fnames,
                  selected=mt$primaryKey,
                  multiple=TRUE,
                  width="100%"
               )
            ),
            column(
               2,
               actionButton(
                  "refreshPrimaryKey",
                  label=NULL,
                  icon=icon("check", "fa-1x"),
                  class="btn btn-default action-button shiny-bound-input disabled"
               ) %>% div(title="Update table primary key"),
               style="text-align:right;"
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
            enable("refreshPrimaryKey")
         }else{
            disable("refreshPrimaryKey")
         }
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
                     icon=icon("plus-square", "fa-1x")
                  ) %>%
                     div(title="Add an index", style="display:inline-block;"),
                  style="text-align:right;"
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
            model$indexTable <- mt$indexes %>%
               lapply(function(x){
                  tibble(
                     Fields=sprintf("[%s]", paste(x$fields, collapse="], [")),
                     Unique=x$unique
                  )
               }) %>%
               do.call(rbind, .)
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
               icon=icon("edit", "fa-1x")
            ) %>%
               div(
                  title="Update index properties",
                  style="display:inline-block;"
               ),
            actionButton(
               "removeIndex",
               label=HTML(paste(
                  '<i class="fa fa-minus-square fa-1x" style="color:red;">',
                  '</i>'
               ))
            ) %>% div(title="Remove index", style="display:inline-block;")
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
            model$new <- m
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
         m <- m %>% remove_index(
            tableName=selTable,
            fieldNames=mt$indexes[[seli]]$fields
         )
         model$new <- m
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
            disable("confirmAddIndex")
         }else{
            enable("confirmAddIndex")
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
         }
         removeModal()
      })

      #########################################################################@
      ## Multiple tables ----
      #########################################################################@

      output$addFKInput <- renderUI({
         selTable <- selection$tables
         validate(need(length(selTable)>0 & length(selTable)<=2, ""))
         actionButton(
            "addForeignKey", "Add key",
            icon=icon("external-link-alt", "fa-2x")
         ) %>% div(title="Add a foreign key")
      })
      output$rmFKInput <- renderUI({
         selFK <- selection$fk
         validate(need(length(selFK)>0, ""))
         actionButton(
            "removeFK",
            label=HTML(paste(
               '<i class="far fa-trash-alt fa-2x"></i>',
               'Remove keys'
            ))
         ) %>% div(title="Remove selected foreign keys")
      })
      output$rmTablesInput <- renderUI({
         selTable <- selection$tables
         validate(need(length(selTable)>0, ""))
         actionButton(
            "removeTables",
            label=HTML(paste(
               '<i class="fas fa-trash fa-2x"></i>',
               'Remove tables'
            ))
         ) %>% div(title="Remove selected tables")
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
         ) %>% div(title="Select table color")
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
                        sprintf(
                           "%s is referenced by other tables.",
                           sprintf("<strong>%s</strong>", tn)
                        ),
                        "<br>Remove foreign keys before removing this table."
                     )),
                     style="color:red;"#font-weight: bold;"
                  ),
                  size="s",
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
                  fluidRow(h4(tns[1]), style="text-align:center;"),
                  fluidRow(uiOutput("ilcard"))
               ),
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
                        "fkDirection", "",
                        icon=icon("long-arrow-alt-right", "fa-2x")
                     ) %>% div(title="Change foreign key direction"),
                     style="text-align:center;"
                  )
               },
               column(
                  5,
                  fluidRow(h4(tns[length(tns)]), style="text-align:center;"),
                  fluidRow(uiOutput("ircard"))
               ),
               style="border-bottom:solid; border-color:#317EAC;"
            ),
            fluidRow(
              uiOutput("fkFields"),
              style=paste(
                 "border-bottom:solid ;border-color:#317EAC;",
                 "margin:15px; padding:15px;"
              )
            ),
            fluidRow(
               uiOutput("possibleFkFields"),
               style=paste(
                  "border-bottom:solid; border-color:#317EAC;",
                  "margin:15px; padding:15px;"
               )
            )
         )
      })

      observe({
         if(
            length(foreignKey$fromTable)==0 || length(foreignKey$toTable)==0 ||
            length(foreignKey$fromFields)==0 || length(foreignKey$toFields)==0
         ){
            disable("confirmAddFK")
         }else{
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
                  style="width:100%; margin:auto;"
               )
            ),
            column(2, uiOutput("addFkFields"), style="text-align:center;"),
            column(
               5,
               div(
                  selectInput(
                     "fkToField", "", ttfields, multiple=FALSE, width="100%"
                  ),
                  style="width:100%; margin:auto;"
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
               style="text-align:right;"
            )
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
            tibble(l=left, r=right),
            rownames=FALSE,
            # colnames=c(tns[1], tns[length(tns)]),
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
               label=HTML(paste(
                  '<i class="fa fa-minus-square fa-1x" style="color:red;">',
                  '</i>'
               ))
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
         model$new <- isolate(model$x) %>%
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
         removeModal()
      })

      ## _+ Cardinality ----
      output$ilcard <- renderUI({
         tns <- isolate(selection$tables)
         ft <- foreignKey$fromTable
         tt <- foreignKey$toTable
         validate(need(ft, ""))
         validate(need(tt, ""))
         m <- isolate(model$x)
         toRet <- list(
            column(
               6,
               selectInput(
                  ifelse(tns[1]==ft, "fcardmin", "tcardmin"),
                  "Min. card.",
                  choices=c("0", "1"),
                  selected=ifelse(tns[1]==ft, "0", "1")
               )
            ),
            column(
               6,
               selectInput(
                  ifelse(tns[1]==ft, "fcardmax", "tcardmax"),
                  "Max. card.",
                  choices=c("1", "n"),
                  selected=ifelse(tns[1]==ft, "n", "1")
               )
            )

         )
         return(toRet)
      })
      output$ircard <- renderUI({
         tns <- isolate(selection$tables)
         ft <- foreignKey$fromTable
         tt <- foreignKey$toTable
         validate(need(ft, ""))
         validate(need(tt, ""))
         m <- isolate(model$x)
         toRet <- list(
            column(
               6,
               selectInput(
                  ifelse(tns[1]!=ft, "fcardmin", "tcardmin"),
                  "Min. card.",
                  choices=c("0", "1"),
                  selected=ifelse(tns[1]!=ft, "0", "1")
               )
            ),
            column(
               6,
               selectInput(
                  ifelse(tns[1]!=ft, "fcardmax", "tcardmax"),
                  "Max. card.",
                  choices=c("1", "n"),
                  selected=ifelse(tns[1]!=ft, "n", "1")
               )
            )

         )
         return(toRet)
      })

      observe({
         cval <- c("0"=0L, "1"=1L, "n"=-1L)
         foreignKey$fmin <- as.integer(cval[input$fcardmin])
         foreignKey$fmax <- as.integer(cval[input$fcardmax])
         foreignKey$tmin <- as.integer(cval[input$tcardmin])
         foreignKey$tmax <- as.integer(cval[input$tcardmax])
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
         mne <- modelToVn(m, color=isolate(settings$defaultColor))$edges
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
         cp <- lapply(m, function(n)tibble(x=n$display$x, y=n$display$y)) %>%
            do.call(rbind, .)
         np <- lapply(dispNodes[names(m)], function(n)tibble(x=n$x, y=n$y)) %>%
            do.call(rbind, .)
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
               ndm <- modelToVn(dm, color=isolate(settings$defaultColor))
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
         mn <- modelToVn(m)
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
         ndm <- modelToVn(dm, color=isolate(settings$defaultColor))
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
         ndm <- modelToVn(dm, color=isolate(settings$defaultColor))
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
            plot(m, color=isolate(settings$defaultColor)) %>% visSave(file)
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
#' @param defaultColor a single color indicating the default table color
#' @param availableColors a character of possible colors for tables
#' @param example a file path to an sql or json model
#'
#' @export
#'
model_relational_data <- function(
   modelInput=RelDataModel(list()), fromR=interactive(),
   defaultColor="#D9D9D9",
   availableColors=c(
      "#9BC8FE", "#F67FC4", "#ACF1BC", "#C6BDF1", "#DFFB86",
      "#F8DEC3", "#8FE6E0", "#FEFE8F", "#FAC6DC", "#A9ECC9"
   ),
   # availableColors=c(
   #    "#D9B661", "#E14D7D", "#75E4AE", "#9638E5", "#C6D1DC", "#D8B8E3",
   #    "#C663CB", "#7CE65F", "#DF7442", "#E0B8A0", "#D4E355", "#7D76D9",
   #    "#6E9DCE", "#E747D7", "#77DFDE", "#CC8490", "#D991D6", "#D0E9CF",
   #    "#CAE095", "#6E8D65"
   # ),
   example=system.file(
      "examples/HPO-model.sql",
      package = packageName()
   )
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
   server <- buildServer(
      modelInput=modelInput, fromR=fromR, bcko=bcko,
      defaultColor=defaultColor, availableColors=availableColors,
      example=example
   )

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
