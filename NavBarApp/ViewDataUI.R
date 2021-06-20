ViewDataUI <- function(id = "ViewDataUI", label = "ViewDataUI") {
  ns <- NS(id)
  
  tabPanel(
    title = "View Data",
    uiOutput(ns("data_heads")),
    FitSummaryUI(id = ns("FitSummaryUI"))
  )   
}