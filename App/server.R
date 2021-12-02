# Server logic for CaPow web app
shinyServer(function(input, output, session) {
  # Load main functions for app.  local = T evaluates the expressions within
  # this environment, rather than the global one, which this one can't seem to
  # see
  source("Code/capow_tmb.R", local = T)
  # source("capow.R", local = T)
  set.seed(1)
  
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