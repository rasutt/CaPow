# Server logic for FitSummary module
FitSummaryServer <- function(input, output, session, capow_list) {
  # Display list summaries
  # Fits
  output$fitlist <- renderTable({
    allfits <- capow_list()$fit_list()
    if(length(allfits) > 0){
      # Get fit info
      allfits <- allfits[naturalorder(names(allfits))]
      allfitsnames <- unlist(lapply(allfits, function(x) x["name"]))
      allfitsdescrip <- unlist(lapply(allfits, function(x) x["description"]))
      allfitsdataset <- unlist(lapply(allfits, function(x) x["dataset"]))
      allfitsmodel <- unlist(lapply(allfits, function(x) x["model"]))
      
      # Create dataframe
      data.frame(
        row.names = NULL,
        "Name" = allfitsnames,
        "Description" = allfitsdescrip,
        "Dataset" = allfitsdataset,
        "Model"= allfitsmodel
      )
    }
    else {
      data.frame("Name" = "No Fits created")
    }
  })
  
  # Datasets
  output$datasetlist <- renderTable({
    alldatasets <- capow_list()$dataset_list()
    if(length(alldatasets) > 0){
      alldatasets <- alldatasets[naturalorder(names(alldatasets))]
      data.frame(
        row.names = NULL,
        "Name" = unlist(lapply(alldatasets, function(x) x$datasetname)),
        "Description" = unlist(lapply(alldatasets, function(x) x$description))
      )
    }
    else {
      data.frame("Name" = "No Datasets created")
    }
  })
  
  # Models
  output$modellist <- renderTable({
    if(length(capow_list()$model_list()) > 0){
      allmodels <- capow_list()$model_list()
      allmodels <- allmodels[naturalorder(names(allmodels))]
      data.frame(
        row.names = NULL,
        "Name" = unlist(lapply(allmodels, function(x) x$modelname)),
        "Description" = unlist(lapply(allmodels, function(x) x$description))
      )
    }
    else {
      data.frame("Name" = "No Models created")
    }
  })
}

# Server logic for Summary UI
SummaryServer <- function(input, output, session, capow_list) {
  # Display list summaries
  
  # Projects
  output$projlist <- renderTable({
    ## If projects already exist, grab them from CPenv, otherwise return a table with nothing in it:
    allproj <- capow_list()$project_list()
    if(length(allproj) > 0){
      ## Create the list of all projects for display:
      allproj <- allproj[naturalorder(names(allproj))]
      allprojnames <- unlist(lapply(allproj, function(x) x["name"]))
      allprojdescrip <- unlist(lapply(allproj, function(x) x["description"]))
      allprojsim <- unlist(lapply(allproj, function(x) x["simset"]))
      allprojmodel <- unlist(lapply(allproj, function(x) x["model"]))
      allprojrun <- unlist(lapply(allproj, function(x) x["run"]))
      
      data.frame(
        row.names = NULL,
        "Name" = allprojnames,
        "Description" = allprojdescrip,
        "Run" = allprojrun,
        "Simulation" = allprojsim,
        "Model"= allprojmodel
      )
    }
    else {
      data.frame("Name" = "No projects created")
    }
  })
  
  # Simulations - Should this also include whether and which fits are included? 
  output$simlist <- renderTable({
    if(length(capow_list()$sim_list()) > 0){
      allsims <- capow_list()$sim_list()
      allsims <- allsims[naturalorder(names(allsims))]
      data.frame(
        row.names = NULL,
        "Name" = unlist(lapply(allsims, function(x) x$simname)),
        "Description" = unlist(lapply(allsims, function(x) x$description))
      )
    }
    else {
      data.frame("Name" = "No Sims created")
    }
  })
  
  # Fits, Datasets, and Models
  callModule(FitSummaryServer, "FitSummaryUI", capow_list)
}