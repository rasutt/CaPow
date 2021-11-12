SaveUI <- function(id = "SaveUI", label = "SaveUI") {
  ns <- NS(id)
  
  tabPanel(
    title = "Save Session",
    tags$head(tags$style("#SaveUI-errormessage{color: red;}")),
    div(
      style = "width:1070px;",
      fluidPage(
        title = "Save Session",
        sidebarLayout(
          sidebarPanel(
            textInput(ns("filenameinput"), "Enter filename"),
            downloadButton(ns("savesession"), "Save Session")
          ),
          
          mainPanel(
            h4(textOutput(ns("errormessage"))),
            SummaryUI(id = ns("SummaryUI"))
          )
        )
      )
    )
  )
}