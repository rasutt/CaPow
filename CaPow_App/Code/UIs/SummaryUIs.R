# UI for FitSummary module
FitSummaryUI <- function(id = "FitSummaryUI") {
  ns <- NS(id)
  
  list(
    h4("Fit List"),
    tableOutput(ns("fitlist")),
    h4("Dataset List"),
    tableOutput(ns("datasetlist")),
    h4("Model List"),
    tableOutput(ns("modellist"))
  )  
}

# UI for Summary module
SummaryUI <- function(id = "SummaryUI") {
  ns <- NS(id)
  
  list(
    h4("Project List"),
    uiOutput(ns("projlist")),
    h4("Simulation List"),
    tableOutput(ns("simlist")),
    FitSummaryUI(id = ns("FitSummaryUI"))
  )  
}