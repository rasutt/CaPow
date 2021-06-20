UploadUI <- function(id = "UploadUI", label = "UploadUI") {
  ns <- NS(id)
  
  tabPanel(
    title = "Upload Data",
    tags$head(tags$style("#UploadUI-errormessage{color: red;}")),
    div(
      style = "width:1070px;",
      fluidPage(
        title = "Upload Data",
        sidebarLayout(
          sidebarPanel(
            textInput(ns("dataname"), "Dataset name:", ""),
            textInput(ns("datadescrip"), "Dataset description:", ""),
            fileInput(
              ns("file"), "Choose CSV File (one column/survey)",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")
            ),
            checkboxInput(ns("header"), "Header", TRUE),
            actionButton(ns("uploadbutton"), "Upload Dataset")
          ),
          
          mainPanel(
            h4(textOutput(ns("errormessage"))),
            h4("Head of File"),
            tableOutput(ns("head")),
            FitSummaryUI(id = ns("FitSummaryUI"))
          )
        )
      )
    )
  )
}
