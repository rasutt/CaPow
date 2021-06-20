# UI for Summary module
SummaryUI <- function(id = "SummaryUI") {
  ns <- NS(id)
  
  list(
    h4("Project List"),
    tableOutput(ns("projlist")),
    h4("Simulation List"),
    tableOutput(ns("simlist")),
    FitSummaryUI(id = ns("FitSummaryUI"))
  )  
}