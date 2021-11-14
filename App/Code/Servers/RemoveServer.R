# Server logic for Remove Items module
RemoveServer <- function(input, output, session, capow_list) {
  # Reactive component selection
  ns <- NS("RemoveUI")
  output$projselect <- renderUI(
    selectInput(ns("projremove"), "Choose a Project to remove:",
                choices = c(" ", naturalsort(names(capow_list()$project_list())), "Remove All Projects"))
  )
  output$simselect <- renderUI(
    selectInput(ns("simremove"), "Choose a Sim to remove:",
                choices = c(" ", naturalsort(names(capow_list()$sim_list())), "Remove All Sims"))
  )
  output$modelselect <- renderUI(
    selectInput(ns("modelremove"), "Choose a Model to remove:",
                choices = c(" ", naturalsort(names(capow_list()$model_list())), "Remove All Models"))
  )
  output$fitselect <- renderUI(
    selectInput(ns("fitremove"), "Choose a Fit to remove:",
                choices = c(" ", naturalsort(names(capow_list()$fit_list())), "Remove All Fits"))
  )
  output$datasetselect <- renderUI(
    selectInput(ns("datasetremove"), "Choose a Dataset to remove:",
                choices = c(" ", naturalsort(names(capow_list()$dataset_list())), "Remove All Datasets"))
  )
  
  # Error message
  stopMessage <- reactiveVal("")
  output$errormessage <- renderText(stopMessage())
  
  # Remove objects when button clicked
  # Projects
  observeEvent(
    input$projremovebutton,
    {
      ## If projects already exist, grab them from CPenv, otherwise return a table with nothing in it:
      if(length(get("ProjectList", envir = CPenv))>0){
        ProjList.old <- get("ProjectList", envir = CPenv)
        ProjList.old <- ProjList.old[naturalorder(names(ProjList.old))]
        
        ProjList.new <- ProjList.old
        
        ## If Remove All Projects is entered in the input, remove all projects and assign an empty list to
        ## CPenv:
        if(input$projremove=="Remove All Projects"){
          ## We remove all results no matter what input$resultsonly says:
          for(pr in names(ProjList.old)) {
            ProjList.new[[pr]]["run"] <- 0
            resultname <- paste("result.", pr, sep="")
            if(exists(resultname, envir=CPenv)) rm(list=resultname, envir=CPenv)
          }
          ## If also removing all projects entirely, replace ProjList.new with a blank list:
          if(input$resultsonly=="removeproj")
            ProjList.new <- list()
        }
        
        ## If a project name is entered in the input, remove the project and / or its results:
        else if(input$projremove!=" "){
          ## If the project has already been removed, do nothing, otherwise continue:
          if(!is.na(match(input$projremove, names(ProjList.old)))){
            
            ## Remove the results no matter what input$resultsonly says, and set the "run" status to 0:
            resultname <- paste("result.", input$projremove, sep="")
            if(exists(resultname, envir=CPenv)) {
              rm(list=resultname, envir=CPenv)
              ProjList.new[[input$projremove]]["run"] <- 0
              stopMessage("")
            }
            ## If only removal of results was requested, and there are no results to remove, dump:
            else if(input$resultsonly=="resonly")
              stopMessage("Project had no results to remove!")
            
            ## If also removing the project entirely, remove the project from the project list:
            if(input$resultsonly=="removeproj") {
              ProjList.new[[input$projremove]] <- NULL
              stopMessage("")
            }
          }  ## End of case where the project has not previously been removed.
        }
        
        ## If the project list has been updated, write the updated list to CPenv, even if empty:
        if(input$projremove!=" ") {
          assign("ProjectList", ProjList.new, envir=CPenv)
          capow_list()$project_list(ProjList.new)
        }
      }  ## End of if(project-type selector chosen)
    }
  )
  
  # Simulations
  observeEvent(
    input$simremovebutton,
    {
      ## If SimSets already exist, grab them from CPenv, otherwise return a table with nothing in it:
      if(length(get("SimList", envir = CPenv))>0){
        SimList.old <- get("SimList", envir = CPenv)
        SimList.old <- SimList.old[naturalorder(names(SimList.old))]
        SimList.new <- SimList.old
        
        ## If Remove All SimSets is entered in the input, remove all simsets and assign an empty list to
        ## CPenv:
        if(input$simremove=="Remove All Sims"){
          ## If there are any projects at all, it is not allowed to remove Sims:
          if(length(get("ProjectList", envir=CPenv))>0)
            stopMessage("Sims cannot be removed if they are part of Projects. 
                        You need to remove the projects first.")
          ## If continuing, set the new SimList to blank:
          else {
            SimList.new <- list()
            stopMessage("")
          }
        }
        
        ## If a Simset name is entered in the input, remove the Simset:
        else if(input$simremove!=" "){
          ## If the Simset has already been removed, do nothing, otherwise continue:
          if(!is.na(match(input$simremove, names(SimList.old)))){
            
            ## If continuing, check that the Simset does not belong to any project.  If it does, dump.
            allproj <- get("ProjectList", envir=CPenv)
            simsInProjects <- unlist(lapply(allproj, function(x)x["simset"]))
            projContainingSim <- names(allproj)[simsInProjects==input$simremove]
            if(length(projContainingSim)>0)
              stopMessage(paste("Sim", input$simremove, "is included in projects", paste0(projContainingSim, collapse=", "), 
                                ". Sims cannot be removed if they are part of Projects. You need to remove the projects first."))
            
            ## If continuing, remove the Simset from the Sim list:
            else {
              SimList.new[[input$simremove]] <- NULL
              stopMessage("")
            }
          }   ## End of case where the Simset has not previously been removed.
        }
        
        ## If the SimList has been updated, write the updated list to CPenv, even if empty:
        if(input$simremove!=" ") {
          assign("SimList", SimList.new, envir=CPenv)
          capow_list()$sim_list(SimList.new)
        }
      }  ## End of if(sim-type selector is chosen)
    }
  )
  
  # Models
  observeEvent(
    input$modelremovebutton,
    {
      ## If Models already exist, grab them from CPenv, otherwise return a table with nothing in it:
      if(length(get("ModelList", envir = CPenv))>0){
        ModelList.old <- get("ModelList", envir = CPenv)
        ModelList.old <- ModelList.old[naturalorder(names(ModelList.old))]
        ModelList.new <- ModelList.old
        
        ## If Remove All Models is entered in the input, remove all models and assign an empty list to
        ## CPenv:
        if(input$modelremove=="Remove All Models"){
          ## If there are any projects at all, it is not allowed to remove Models:
          if(length(get("ProjectList", envir=CPenv))>0)
            stopMessage("Models cannot be removed if they are part of Projects. 
                          You need to remove the projects first.")
          ## If continuing, set the new ModelList to blank:
          else {
            ModelList.new <- list()
            stopMessage("")
          }
        }
        
        ## If a Model name is entered in the input, remove the Model:
        else if(input$modelremove!=" "){
          ## If the Model has already been removed, do nothing, otherwise continue:
          if(!is.na(match(input$modelremove, names(ModelList.old)))){
            
            ## If continuing, check that the Model does not belong to any project.  If it does, dump.
            allproj <- get("ProjectList", envir=CPenv)
            modelsInProjects <- unlist(lapply(allproj, function(x)x["model"]))
            projContainingModel <- names(allproj)[modelsInProjects==input$modelremove]
            if(length(projContainingModel)>0)
              stopMessage(paste("Model", input$modelremove, "is included in projects", 
                                paste0(projContainingModel, collapse=", "), 
                                ". Models cannot be removed if they are part of Projects. 
                                  You need to remove the projects first."))
            
            ## If continuing, remove the Model from the Model list:
            else {
              ModelList.new[[input$modelremove]] <- NULL
              stopMessage("")
            }
            
          }   ## End of case where the model has not previously been removed.
        }
        ## If the ModelList has been updated, write the updated list to CPenv, even if empty:
        if(input$modelremove!=" ") {
          assign("ModelList", ModelList.new, envir=CPenv)
          capow_list()$model_list(ModelList.new)
        }
      }  ## End of if(model-type selector is chosen)
    }
  )
  
  # Fits - Should update to require that not included in any simulations
  observeEvent(
    input$fitremovebutton,
    {
      ## If fits already exist, grab them from CPenv, otherwise return a table with nothing in it:
      if(length(get("FitList", envir = CPenv))>0){
        FitList.old <- get("FitList", envir = CPenv)
        FitList.old <- FitList.old[naturalorder(names(FitList.old))]
        
        FitList.new <- FitList.old
        
        ## If Remove All Fits is entered in the input, remove all fits and assign an empty list to
        ## CPenv:
        if(input$fitremove=="Remove All Fits"){
            FitList.new <- list()
            stopMessage("")
        }
        
        ## If a fit name is entered in the input, remove the fit:
        else if(input$fitremove!=" "){
          ## If the fit has already been removed, do nothing, otherwise continue:
          if(!is.na(match(input$fitremove, names(FitList.old)))){
            FitList.new[[input$fitremove]] <- NULL
            stopMessage("")
          }
        }  ## End of case where the fit has not previously been removed.
        
        ## If the fit list has been updated, write the updated list to CPenv, even if empty:
        if(input$fitremove!=" ") {
          assign("FitList", FitList.new, envir=CPenv)
          
          # Remove the same number of renderUI outputs so don't get errors in RStudio
          for(i in length(capow_list()$fit_list()):(length(FitList.new) + 1)) {
            removeUI(paste0("[id='ParameterEstimatesUI-fit", i, "']"))
            removeUI(paste0("[id='MetadataUI-metadata", i, "']"))
            removeUI(paste0("[id='PopulationEstimatesUI-pop_table", i, "']"))
            removeUI(paste0("[id='ViewPopPlotsUI-plot", i, "']"))
          }
          
          # Update reactive list
          capow_list()$fit_list(FitList.new)
        }
      }  ## End of if(fit-type selector chosen)
    }
  )
  
  # Datasets
  observeEvent(
    input$datasetremovebutton,
    {
      ## If Datasets already exist, grab them from CPenv, otherwise return a table with nothing in it:
      if(length(get("DatasetList", envir = CPenv))>0){
        DatasetList.old <- get("DatasetList", envir = CPenv)
        DatasetList.old <- DatasetList.old[naturalorder(names(DatasetList.old))]
        DatasetList.new <- DatasetList.old
        
        ## If Remove All Datasets is entered in the input, remove all datasets and assign an empty list to
        ## CPenv:
        if(input$datasetremove=="Remove All Datasets"){
          ## If there are any fits at all, it is not allowed to remove Datasets:
          if(length(get("FitList", envir=CPenv))>0)
            stopMessage("Datasets cannot be removed if they are part of Fits. 
                          You need to remove the fits first.")
          ## If continuing, set the new DatasetList to blank:
          else {
            DatasetList.new <- list()
            stopMessage("")
          }
        }
        
        ## If a Dataset name is entered in the input, remove the Dataset:
        else if(input$datasetremove!=" "){
          ## If the Dataset has already been removed, do nothing, otherwise continue:
          if(!is.na(match(input$datasetremove, names(DatasetList.old)))){
            
            ## If continuing, check that the Dataset does not belong to any fit.  If it does, dump.
            allfit <- get("FitList", envir=CPenv)
            datasetsInFits <- unlist(lapply(allfit, function(x)x["dataset"]))
            fitContainingDataset <- names(allfit)[datasetsInFits==input$datasetremove]
            if(length(fitContainingDataset)>0)
              stopMessage(paste("Dataset", input$datasetremove, "is included in fits", 
                                paste0(fitContainingDataset, collapse=", "), 
                                ". Datasets cannot be removed if they are part of Fits. 
                                  You need to remove the fits first."))
            
            ## If continuing, remove the Dataset from the Dataset list:
            else {
              DatasetList.new[[input$datasetremove]] <- NULL
              stopMessage("")
            }
            
          }   ## End of case where the dataset has not previously been removed.
        }
        ## If the DatasetList has been updated, write the updated list to CPenv, even if empty:
        if(input$datasetremove!=" ") {
          assign("DatasetList", DatasetList.new, envir=CPenv)
          
          # Remove the same number of renderUI outputs so don't get errors in RStudio
          for(i in length(capow_list()$dataset_list()):(length(DatasetList.new) + 1)) {
            removeUI(paste0("[id='ViewDataUI-dataset", i, "']"))
          }
          
          # Update reactive list
          capow_list()$dataset_list(DatasetList.new)
        }
      }  ## End of if(dataset-type selector is chosen)
    }
  )
  
  # Display lists
  callModule(SummaryServer, "SummaryUI", capow_list)
  
  # Return reactive list of objects
  capow_list
}