# UI for Project Builder module
ProjectBuilderUI <- function(id = "ProjectBuilderUI") {
  ns <- NS(id)
  
  tabPanel(
    title = "Project Builder",
    tags$head(tags$style("#ProjectBuilderUI-errormessage{color: red;}")),
    div(
      style = "width:1070px;",
      fluidPage(
        title = "Create Project",
        sidebarLayout(
          sidebarPanel(
            textInput(ns("projname"), "Project name:", ""),
            textInput(ns("projdescrip"), "Project description:", ""),
            uiOutput(ns("simselection")),
            uiOutput(ns("modelselection")),
            actionButton(ns("savebutton"), "Save Project")
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