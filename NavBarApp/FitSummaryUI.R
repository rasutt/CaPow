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