# Server logic for Project Builder module
ProjectBuilderServer <- function(input, output, session, capow_list) {
  # Reactive component selection
  ns <- NS("ProjectBuilderUI")
  output$simselection <- renderUI(
    selectInput(ns("simset"), "Choose a Simulation Set:", 
                choices = naturalsort(names(capow_list()$sim_list())))
  )
  output$modelselection <- renderUI(
    selectInput(ns("model"), "Choose a Model:", 
                choices = naturalsort(names(capow_list()$model_list())))
  )
  
  # Error message
  stopMessage <- reactiveVal("")
  output$errormessage <- renderText(stopMessage())
  
  # Update project list when save button pressed
  create_project_func <- function() {
    ## Get the existing project list, even if empty:
    ProjList.old <- capow_list()$project_list()
    ProjList.old <- ProjList.old[naturalorder(names(ProjList.old))]
    
    ## Check that the simset and model both have identical configurations of survey years:
    ## they need to have identical time labels AND identical selection of survey years within
    ## those time labels.  For example, it wouldn't work if the simset has labels 2001, 2003, 2005
    ## but the model has labels 2001, 2002, 2003, 2004, 2005 with surveys at 2001, 2003, 2005,
    ## because we need to simulate what is happening during the survey gaps.
    chosenSim <- get("SimList", envir=CPenv)[[ input$simset ]]
    chosenModel <- get("ModelList", envir=CPenv)[[ input$model ]]
    simTimeLabels <- as.character(chosenSim$paramdf$timelabels)
    modelTimeLabels <- as.character(chosenModel$paramdf$timelabels)
    simTimeOpt <- chosenSim$paramdf$timeopt
    modelTimeOpt <- chosenModel$paramdf$timeopt
    if (length(simTimeLabels) != length(modelTimeLabels) || 
        any(simTimeLabels != modelTimeLabels) ||
        any(simTimeOpt != modelTimeOpt)) {
      stopMessage("This Sim and Model cover different arrangements of time periods and/or survey years.  
                  You can only combine sims and models into projects if they have the same survey configuration.")
    } else {
      ## Extract all project components from the various inputs,
      ## Project name, Description, Simset name, Model name,
      ## and add to the end "run=0" to indicate that the project is not yet run.
      projcomps <- c(
        name = input$projname, 
        description = input$projdescrip,
        simset = input$simset, 
        model = input$model, 
        run=0
      )
      
      ## Run checks if there are existing projects: the new project must have a different name
      ## and a new combination of simset and model:
      if(length(ProjList.old) > 0){
        ## Make sure the project name hasn't already been used:
        existing.projnames <- unlist(lapply(ProjList.old, function(x)x["name"]))
        if(any(existing.projnames==input$projname)) stopMessage("Project name already taken")
        else if(input$projname == "") stopMessage("Please enter a project name")
        else {
          ## Make sure the combination of simset and model hasn't already been used:
          existing.simnames <- unlist(lapply(ProjList.old, function(x)x["simset"]))
          existing.modelnames <- unlist(lapply(ProjList.old, function(x)x["model"]))
          nproj <- length(ProjList.old)
          simmatch <- as.numeric(existing.simnames==input$simset)
          modelmatch <- as.numeric(existing.modelnames==input$model)
          if(any(simmatch * modelmatch == 1)){
            which.match <- which(simmatch * modelmatch==1)
            stopMessage(
              paste0("Project ", existing.projnames[which.match], 
                     " already has this combination of Simulation and Model."))
          } else {
            ## Append the newly created project to the project list:
            ProjList.new <- ProjList.old
            ProjList.new[[input$projname]] <- projcomps
            ## Write the updated project list to CPenv:
            assign("ProjectList", ProjList.new, envir=CPenv)
            capow_list()$project_list(ProjList.new)
            stopMessage("")
          }
        }
      } else {
        ## Append the newly created project to the project list:
        ProjList.new <- ProjList.old
        ProjList.new[[input$projname]] <- projcomps
        ## Write the updated project list to CPenv:
        assign("ProjectList", ProjList.new, envir=CPenv)
        capow_list()$project_list(ProjList.new)
        stopMessage("")
      }
    }
  }
  observeEvent(input$savebutton, create_project_func())
  
  # Display summary lists
  callModule(SummaryServer, "SummaryUI", capow_list)
  
  # Return reactive objects list
  capow_list
}