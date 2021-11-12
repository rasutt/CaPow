ViewPopPlotsUI <- function(id = "ViewPopPlotsUI", label = "ViewPopPlotsUI") {
  ns <- NS(id)
  
  tabPanel(
    title = "Population Plots",
    uiOutput(ns("Nt_plots")),
    FitSummaryUI(id = ns("FitSummaryUI"))
  )   
}