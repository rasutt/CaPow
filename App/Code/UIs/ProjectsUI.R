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

# UI for Run module
RunUI <- function(id = "RunUI") {
  ns <- NS(id)
  
  tabPanel(
    title = "Run Projects",
    div(
      style = "width:1070px;",
      fluidPage(
        sidebarLayout(
          sidebarPanel(
            numericInput(
              ns("Nsim"), 
              "Number of simulations:", 
              value = 1000,
              min = 100,
              max = 1000,
              step = 100
            ),
            uiOutput(ns("projselect")),
            h4(htmlOutput(ns("running"))),
            h4(htmlOutput(ns("noprojects"))),
            actionButton(ns("runbutton"), "Run Projects"),
            br(),
            helpText("Click Run Projects to submit projects for running."),
            helpText("Do not close the browser until all projects have finished.  You can monitor progress in the R console.")
          ),
          ## Show the projname, the simset description, and the model description
          mainPanel(
            SummaryUI(id = ns("SummaryUI"))
          )
        )
      )
    )
  )
}

# UI for Projects tab
ProjectsUI <- function(id = "ProjectsUI") {
  ns <- NS(id)
  
  tabPanel(
    title = "Projects",
    tabsetPanel(
      ProjectBuilderUI(),
      RunUI()
    )
  )
}