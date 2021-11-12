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