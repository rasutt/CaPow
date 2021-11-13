# Server logic for combined CaPow app.

# Overall server logic
shinyServer(function(input, output, session) {
  # I can maybe source most of this outside server? I just want to load
  # capow.dat inside so that each session gets its own CPenv...
  source("capow_tmb.R", local = T)
  # source("capow.R", local = T)
  set.seed(1)
  
  # Source server logic for modules
  # What does the local = T do again?
  source("NavBarApp/ModelServer.R", local = T)
  source("NavBarApp/FitSummaryServer.R", local = T)
  source("NavBarApp/FitBuilder/UploadServer.R", local = T)
  source("NavBarApp/FitBuilder/FitModelServer.R", local = T)
  source("NavBarApp/FitBuilder/ViewDataServer.R", local = T)
  source("NavBarApp/FitBuilder/ParameterEstimatesServer.R", local = T)
  source("NavBarApp/FitBuilder/ModelCompServer.R", local = T)
  source("NavBarApp/FitBuilder/PopulationEstimatesServer.R", local = T)
  source("NavBarApp/FitBuilder/ViewPopPlotsServer.R", local = T)
  source("NavBarApp/SimServer.R", local = T)
  source("NavBarApp/SummaryServer.R", local = T)
  source("NavBarApp/ProjectBuilderServer.R", local = T)
  source("NavBarApp/RunServer.R", local = T)
  source("NavBarApp/PowerServer.R", local = T)
  source("NavBarApp/PlotServer.R", local = T)
  source("NavBarApp/RemoveServer.R", local = T)
  source("NavBarApp/SaveLoadServer.R", local = T)
  
  # Create reactive list of reactive-value lists for objects to pass between modules
  capow_list <- reactive(
    list(
      sim_list = reactiveVal(get0("SimList", envir = CPenv)),
      model_list = reactiveVal(get0("ModelList", envir = CPenv)),
      project_list = reactiveVal(get0("ProjectList", envir = CPenv)),
      dataset_list = reactiveVal(get0("DatasetList", envir = CPenv)),
      fit_list = reactiveVal(get0("FitList", envir = CPenv))
    )
  )
  
  # Call modules passing reactive values explicitly
  capow_list <- callModule(ModelServer, "ModelUI", capow_list)
  capow_list <- callModule(UploadServer, "UploadUI", capow_list)
  capow_list <- callModule(FitModelServer, "FitModelUI", capow_list)
  callModule(ViewDataServer, "ViewDataUI", capow_list)
  callModule(ParameterEstimatesServer, "ParameterEstimatesUI", capow_list)
  callModule(ModelCompServer, "ModelCompUI", capow_list)
  callModule(PopulationEstimatesServer, "PopulationEstimatesUI", capow_list)
  callModule(ViewPopPlotsServer, "ViewPopPlotsUI", capow_list)
  capow_list <- callModule(SimServer, "SimUI", capow_list)
  capow_list <- callModule(ProjectBuilderServer, "ProjectBuilderUI", capow_list)
  capow_list <- callModule(RunServer, "RunUI", capow_list)
  callModule(PowerServer, "PowerUI", capow_list)
  callModule(PlotServer, "PlotUI", capow_list)
  capow_list <- callModule(RemoveServer, "RemoveUI", capow_list)
  capow_list <- callModule(LoadServer, "LoadUI", capow_list)
  callModule(SaveServer, "SaveUI", capow_list)
})