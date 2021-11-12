PopulationEstimatesUI <- function(id = "PopulationEstimatesUI", label = "PopulationEstimatesUI") {
  ns <- NS(id)
  
  tabPanel(
    title = "Population Estimates",
    uiOutput(ns("pop_tables")),
    FitSummaryUI(id = ns("FitSummaryUI"))
  )   
}