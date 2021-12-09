# Server logic for CaPow web app
shinyServer(function(input, output, session) {
  # Set random number seed while testing
  set.seed(1)
  
  # Load initial objects for app 
  load("Data/capow.dat")
  
  # Compile and load POPAN negative log-likelihood function with TMB
  compile("popan_nll.cpp")
  dyn.load(dynlib("popan_nll"))
  
  # Load functions used by servers into local environment
  source("Code/capow_server_funcs.R", local = T)
  
  # Load server logic for UIs
  files.sources = list.files("Code/Servers", recursive = T)
  for (i in seq_along(files.sources)) {
    source(paste0("Code/Servers/", files.sources[i]), local = T)
  }
  
  # Create reactive list of reactive-value lists for objects to pass between
  # modules.  Reactive values notify things when they change, like shiny inputs.
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