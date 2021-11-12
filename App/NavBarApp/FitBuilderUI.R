FitBuilderUI <- function(id = "FitBuilderUI") {
  tabPanel(
    title = "Fit Builder",
    tabsetPanel(
      UploadUI(),
      ViewDataUI(),
      FitModelUI(),
      ViewPopPlotsUI(),
      ParameterEstimatesUI(),
      PopulationEstimatesUI(),
      ModelCompUI()
    )
  )
}