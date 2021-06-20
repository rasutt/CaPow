LoadUI <- function(id = "LoadUI", label = "LoadUI") {
  ns <- NS(id)
  
  tabPanel(
    title = "Load Session",
    tags$head(tags$style("#LoadUI-errormessage{color: red;}")),
    div(
      style = "width:1070px;",
      fluidPage(
        title = "Load Session",
        sidebarLayout(
          sidebarPanel(
            fileInput(ns("file"), "Choose dat File", multiple = FALSE),
            actionButton(ns("loadbutton"), "Load Session")
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