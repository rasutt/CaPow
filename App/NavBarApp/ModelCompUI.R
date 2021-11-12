ModelCompUI <- function(id = "ModelCompUI", label = "ModelCompUI") {
  ns <- NS(id)
  
  tabPanel(
    title = "Model Comparison",
    uiOutput(ns("model_comp")),
    FitSummaryUI(id = ns("FitSummaryUI"))
  )   
}