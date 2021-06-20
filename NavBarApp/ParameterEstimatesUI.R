ParameterEstimatesUI <- function(id = "ParameterEstimatesUI", label = "ParameterEstimatesUI") {
  ns <- NS(id)
  
  tabPanel(
    title = "Parameter Estimates",
    uiOutput(ns("fit_results")),
    FitSummaryUI(id = ns("FitSummaryUI"))
  )   
}