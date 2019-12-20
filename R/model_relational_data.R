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
            fluidRow(
               column(
                  1,
                  actionButton(
                     "selectAll",
                     label=NULL,
                     icon=icon("object-group", "fa-2x")
                  )
               ),
               column(
                  7,
                  uiOutput("findTable")
               ),
               column(
                  4,
                  uiOutput("modelSummary")
               ),
               style="padding-top:10px; padding-bottom:5px;"
            ),
            fluidRow(
               visNetworkOutput("modelNet", height="80vh", width="100%"),
               style="border:solid; min-height:80vh;"
            )
         ),

         column(
            5,

            ## Multiple tables ----
            uiOutput("multiTables"),

            ## Edit table ----
            uiOutput("editTable")

         )
      )


   ))
}

###############################################################################@
buildServer <- function(
   modelInput, fromR, bcko,
   defaultColor, availableColors
){

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
         merged=NULL,               # merge: c(x, toImport)
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
         tables=NULL,               # Selected tables
         fk=NULL                    # Selected foreign keys
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
      ## Model network ----
      #########################################################################@

      output$modelNet <- renderVisNetwork({
         replot$x
         selection$tables <- NULL
         selection$fk <- NULL
         plot(isolate(model$x), color=isolate(settings$defaultColor)) %>%
            visEvents(
               release="function(nodes) {
                Shiny.onInputChange('modelNet_release', Math.random());
                ;}"
            ) # %>%
            # visOptions(nodesIdSelection=list(useLabels=FALSE))
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
            modelToVn(model$x, color=isolate(settings$defaultColor))$edges$id
         )
      })

      #########################################################################@
      ## Model overview ----
      #########################################################################@

      output$findTable <- renderUI({
         m <- model$x
         selTables <- selection$tables
         validate(need(length(names(m))>0, ""))
         selectInput(
            "findTable",
            label=NULL,
            choices=names(m),
            multiple=TRUE,
            selected=selection$tables,
            width="100%"
         )
      })
      observe({
         selection$tables <- input$findTable
      })
      observeEvent(input$selectAll, {
         m <- isolate(model$x)
         selTables <- isolate(selection$tables)
         if(length(selTables)==length(m)){
            selection$tables <- NULL
         }else{
            selection$tables <- names(m)
         }
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

      output$editTable <- renderUI({
         selTable <- selection$tables
         validate(need(length(selTable)==1, ""))
         mt <- isolate(model$x[[selTable]])
         div(
            fluidRow(
               column(8, h3(selTable, style="margin-top:6px;")),
               column(
                  4,
                  actionButton("renameTable", "Rename"),
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
               "border:solid; border-radius:15px;",
               # "min-height:85vh;",
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
         selTable <- isolate(selection$tables)
         validate(need(length(selTable)==1, ""))
         m <- isolate(model$x)
         if(!is.null(tn) && tn!="" && !tn %in% names(m)){
            m <- rename_table(m, old=selTable, new=tn)
            model$new <- m
            removeModal()
         }
      })

      ## _+ Table commment ----
      output$tableCommentUI <- renderUI({
         selTable <- selection$tables
         validate(need(length(selTable)==1, ""))
         mt <- model$x[[selTable]]
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
                  icon=icon("sync-alt", "fa-1x")
               ),
               style="text-align:right;"
            )
         )
      })
      observe({
         input$refreshComment
         ntn <- input$tableComment
         selTable <- isolate(selection$tables)
         validate(need(length(selTable)==1, ""))
         m <- isolate(model$x)
         cc <- m[[selTable]]$display$comment
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
         selTable <- isolate(selection$tables)
         validate(need(length(selTable)==1, ""))
         m <- isolate(model$x)
         cc <- m[[selTable]]$display$comment
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
         selTable <- selection$tables
         validate(need(length(selTable)==1, ""))
         mt <- isolate(model$x[[selTable]])
         list(
            fluidRow(
               column(6, h4("Fields")),
               column(
                  6,
                  uiOutput("updateField", inline=TRUE),
                  actionButton(
                     "addField", label="",
                     icon=icon("plus-square", "fa-1x")
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
         selTable <- selection$tables
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
         selTable <- selection$tables
         validate(need(length(selTable)==1, ""))
         mt <- model$x[[selTable]]
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
         selTable <- isolate(selection$tables)
         validate(need(length(selTable)==1, ""))
         mt <- isolate(model$x[[selTable]])
         validate(need(nrow(mt$fields)>0, ""))
         validate(need(seli>=1 & seli <= nrow(mt$fields), ""))
         p(
            mt$fields$comment[seli],
            style="font-weight: bold; padding-top:5px; padding-bottom:5px;"
         )
      })
      # ## __- Modify fields ----
      output$updateField <- renderUI({
         seli <- input$fieldTable_rows_selected
         validate(need(length(seli)==1, ""))
         validate(need(nrow(model$fieldTable)>0, ""))
         selTable <- isolate(selection$tables)
         validate(need(length(selTable)==1, ""))
         mt <- isolate(model$x[[selTable]])
         validate(need(nrow(mt$fields)>0, ""))
         validate(need(seli>=1 & seli <= nrow(mt$fields), ""))
         div(
            actionButton(
               "updateField",
               label="",
               icon=icon("edit", "fa-1x")
            ),
            actionButton(
               "removeField",
               label=HTML(paste(
                  '<i class="fa fa-minus-square fa-1x" style="color:red;">',
                  '</i>'
               ))
            ),
            style="display:inline;"
         )
      })
      # ## __- Remove field ----
      observe({
         validate(need(input$removeField>0, ""))
         seli <- isolate(input$fieldTable_rows_selected)
         validate(need(length(seli)==1, ""))
         selTable <- isolate(selection$tables)
         validate(need(length(selTable)==1, ""))
         m <- isolate(model$x)
         mt <- m[[selTable]]
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
         selTable <- isolate(selection$tables)
         validate(need(length(selTable)==1, ""))
         mt <- isolate(model$x[[selTable]])
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
         selTable <- isolate(selection$tables)
         validate(need(length(selTable)==1, ""))
         mt <- isolate(model$x[[selTable]])
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
         selTable <- isolate(selection$tables)
         validate(need(length(selTable)==1, ""))
         mt <- isolate(model$x[[selTable]])
         fields <- mt$fields
         validate(need(input$newFieldName %in% fields$name, ""))
         p("Field name already used", style="color:red;font-weight: bold;")
      })
      observeEvent(input$confirmAddField, {
         selTable <- isolate(selection$tables)
         validate(need(length(selTable)==1, ""))
         mt <- isolate(model$x[[selTable]])
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
         selTable <- isolate(selection$tables)
         validate(need(length(selTable)==1, ""))
         mt <- isolate(model$x[[selTable]])
         fields <- mt$fields
         validate(need(nrow(fields)>0, ""))
         showModal(modalDialog(
            title="Add field",
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
                  )
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
         selTable <- isolate(selection$tables)
         validate(need(length(selTable)==1, ""))
         mt <- isolate(model$x[[selTable]])
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
         selTable <- isolate(selection$tables)
         validate(need(length(selTable)==1, ""))
         mt <- isolate(model$x[[selTable]])
         fields <- mt$fields
         validate(need(nfn %in% fields$name[-seli], ""))
         p("Field name already used", style="color:red;font-weight: bold;")
      })
      observeEvent(input$confirmUpdateField, {
         seli <- isolate(input$fieldTable_rows_selected)
         validate(need(length(seli)==1, ""))
         selTable <- isolate(selection$tables)
         validate(need(length(selTable)==1, ""))
         mt <- isolate(model$x[[selTable]])
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
         nm <- nm %>%
            update_field(
               tableName=selTable,
               fieldName=nfn,
               type=isolate(input$fieldType),
               nullable=isolate(input$fieldNullable),
               unique=isolate(input$fieldUnique),
               comment=as.character(isolate(input$fieldComment))
            )
         if(!identical(nm, isolate(model$x))){
            model$new <- nm
         }
         removeModal()
      })

      ## _+ Table primary key ----
      output$primaryKey <- renderUI({
         selTable <- selection$tables
         validate(need(length(selTable)==1, ""))
         mt <- model$x[[selTable]]
         fnames <- mt$fields$name
         validate(need(fnames, ""))
         fluidRow(
            column(4, h4("Primary key")),
            column(
               8,
               selectInput(
                  "primaryKey", label=NULL,
                  choices=fnames,
                  selected=mt$primaryKey,
                  multiple=TRUE
               )
            )
         )
      })
      observe({
         npk <- input$primaryKey
         selTable <- isolate(selection$tables)
         validate(need(length(selTable)==1, ""))
         m <- isolate(model$x)
         cpk <- m[[selTable]]$primaryKey
         if(length(cpk)!=length(npk) || any(sort(cpk)!=sort(npk))){
            model$new <- m %>%
               set_primary_key(tableName=selTable, fieldNames=npk)
         }
      })

      ## _+ Table indexes ----
      output$indexes <- renderUI({
         selTable <- selection$tables
         validate(need(length(selTable)==1, ""))
         mt <- isolate(model$x[[selTable]])
         fnames <- mt$fields$name
         validate(need(fnames, ""))
         list(
            fluidRow(
               column(6, h4("Indexes")),
               column(
                  6,
                  uiOutput("updateIndex", inline=TRUE),
                  actionButton(
                     "addIndex", label="",
                     icon=icon("plus-square", "fa-1x")
                  ),
                  style="text-align:right;"
               )
            ),
            fluidRow(
               column(12, DT::DTOutput("indexTable"))
               # column(4, uiOutput("updateIndex"))
            )
         )
      })
      output$indexTable <- DT::renderDT({
         selTable <- selection$tables
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
         selTable <- selection$tables
         validate(need(length(selTable)==1, ""))
         mt <- model$x[[selTable]]
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
      output$updateIndex <- renderUI({
         seli <- input$indexTable_rows_selected
         validate(need(length(seli)==1, ""))
         validate(need(nrow(model$indexTable)>0, ""))
         selTable <- isolate(selection$tables)
         validate(need(length(selTable)==1, ""))
         mt <- isolate(model$x[[selTable]])
         validate(need(length(mt$indexes)>0, ""))
         validate(need(seli>=1 & seli <= length(mt$indexes), ""))
         ui <- mt$indexes[[seli]]$unique
         list(
            actionButton(
               "updateIndex",
               label="",
               icon=icon("edit", "fa-1x")
            ),
            actionButton(
               "removeIndex",
               label=HTML(paste(
                  '<i class="fa fa-minus-square fa-1x" style="color:red;">',
                  '</i>'
               ))
            )
         )
      })
      observeEvent(input$updateIndex,{
         seli <- isolate(input$indexTable_rows_selected)
         validate(need(length(seli)==1, ""))
         validate(need(nrow(model$indexTable)>0, ""))
         selTable <- isolate(selection$tables)
         validate(need(length(selTable)==1, ""))
         mt <- isolate(model$x[[selTable]])
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
         selTable <- isolate(selection$tables)
         validate(need(length(selTable)==1, ""))
         m <- isolate(model$x)
         mt <- m[[selTable]]
         validate(need(length(mt$indexes)>0, ""))
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
         selTable <- isolate(selection$tables)
         validate(need(length(selTable)==1, ""))
         m <- isolate(model$x)
         mt <- m[[selTable]]
         validate(need(length(mt$indexes)>0, ""))
         m <- m %>% remove_index(
            tableName=selTable,
            fieldNames=mt$indexes[[seli]]$fields
         )
         model$new <- m
      })
      ## __- Add index ----
      observeEvent(input$addIndex, {
         selTable <- isolate(selection$tables)
         validate(need(length(selTable)==1, ""))
         mt <- isolate(model$x[[selTable]])
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
         selTable <- isolate(selection$tables)
         validate(need(length(selTable)==1, ""))
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

      output$multiTables <- renderUI({
         selTable <- selection$tables
         selFK <- selection$fk
         validate(need(length(selTable)>0 || length(selFK)>0, ""))
         return(div(
            uiOutput(
               "setTableColor",
               style="display: inline-block; margin-right:15px; width:100px;"
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
            style="padding:15px; text-align:center;"
         ))
      })
      output$addFKInput <- renderUI({
         selTable <- selection$tables
         validate(need(length(selTable)>0 & length(selTable)<=2, ""))
         actionButton(
            "addForeignKey", "Add foreign key",
            icon=icon("external-link-alt", "fa-2x")
         )
      })
      output$rmFKInput <- renderUI({
         selFK <- selection$fk
         validate(need(length(selFK)>0, ""))
         actionButton(
            "removeFK",
            label=HTML(paste(
               '<i class="far fa-trash-alt fa-2x"></i>',
               'Remove foreign keys'
            ))
         )
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
         toFields=NULL
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
               column(5, h4(tns[1]), style="text-align:center;"),
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
                     ),
                     style="text-align:center;"
                  )
               },
               column(5, h4(tns[length(tns)]), style="text-align:center;"),
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
            model$new <- m
         }
      })

      #########################################################################@
      ## Update model ----
      #########################################################################@

      observe({
         nm <- model$new
         validate(need(nm, ""))
         dm <- isolate(model$x)
         validate(need(!identical(nm, dm), ""))

         ##
         tdm <- nm
         if(length(dm)==0){
            toReplot <- TRUE
         }else{
            toReplot <- FALSE
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
            visSelectNodes(selTables) # %>%
            # visOptions(nodesIdSelection=list(useLabels=FALSE))
      })

      observe({
         selFK <- intersect(
            modelToVn(model$x, color=isolate(settings$defaultColor))$edges$id,
            selection$fk
         )
         selection$fk <- selFK
         selTables <- intersect(names(model$x), selection$tables)
         if(length(selTables)==0){
            visNetworkProxy("modelNet") %>%
               visSelectEdges(selFK) # %>%
               # visOptions(nodesIdSelection=list(useLabels=FALSE))
         }
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
#'
#' @export
#'
model_relational_data <- function(
   modelInput=RelDataModel(list()), fromR=interactive(),
   defaultColor="#D9D9D9",
   availableColors=c(
      "#D9B661", "#E14D7D", "#75E4AE", "#9638E5", "#C6D1DC", "#D8B8E3",
      "#C663CB", "#7CE65F", "#DF7442", "#E0B8A0", "#D4E355", "#7D76D9",
      "#6E9DCE", "#E747D7", "#77DFDE", "#CC8490", "#D991D6", "#D0E9CF",
      "#CAE095", "#6E8D65"
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
      defaultColor=defaultColor, availableColors=availableColors
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
