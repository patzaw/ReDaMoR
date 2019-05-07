###############################################################################@
#' @importFrom base64enc base64encode
buildUi <- function(fromR){
   shinyUI(fluidPage(
      title="ReDaMoR",

      ## HEAD ----
      tags$head(
         tags$link(
            rel="icon",
            href=paste(
               "data:image/png;base64,",
               base64encode(system.file(
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
                     base64encode(system.file(
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
               "load",
               list(icon("file-import", "fa-2x"), "Import"),
               style="margin-top:25px;margin-bottom:25px;"
            )
                  # fileInput(
                  #    "import", "",
                  #    buttonLabel="Import...",
                  #    accept=c("text/json", "text/sql", ".json", ".sql")
                  # )
         ),
         column(
            2,
            actionButton(
               "save",
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
               h1("Model view"),
               style="border:solid; min-height:85vh;"
            )
         ),

         ## Edit table ----
         column(
            5,
            h1("Table edit")
         )
      )


   ))
}

###############################################################################@
buildServer <- function(fromR){

   function(input, output, session) {

      ## Done button ----
      observeEvent(input$done, {
         stopApp("HELLO!")
      })

   }

}

###############################################################################@
#' Relational data modeler GUI
#'
#' @export
#'
modeler <- function(fromR=TRUE){

   ui <- buildUi(fromR=fromR)
   server <- buildServer(fromR=fromR)

   if(fromR){
      ## From R ----
      runApp(shinyApp(ui, server))
   }else{
      ## Remote app ----
      shinyApp(ui, server)
   }

}
