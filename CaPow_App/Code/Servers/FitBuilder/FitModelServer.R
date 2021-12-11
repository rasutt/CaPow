FitModelServer <- function(input, output, session, capow_list) {
  # Create namespace
  ns <- NS("FitModelUI")
  
  # Create input to choose dataset
  output$datasetselection <- renderUI(
    selectInput(ns("dataset"), "Choose a Dataset:", 
                choices = naturalsort(names(capow_list()$dataset_list()))))
  
  # Create input to choose model
  output$modelselection <- renderUI(
    selectInput(ns("model"), "Choose a Model:", 
                choices = naturalsort(names(capow_list()$model_list()))))
  
  # Create input to choose simset.  Leave commented out in case find starting
  # value problems later and need to let users choose them.
  # output$simselection <- renderUI(
  #   selectInput(ns("sim"), "Choose a Sim (for starting parameters):", 
  #               choices = naturalsort(names(capow_list()$sim_list()))))
  
  # Error message
  stopMessage <- reactiveVal("")
  output$errormessage <- renderText(stopMessage())
  
  # Function to fit model to dataset. Leave the select simset option commented
  # out in case find starting value problems later and need to let users choose
  # them.
  fit_func <- function() {
    chosenDataset <- get("DatasetList", envir=CPenv)[[ input$dataset ]]
    chosenModel <- get("ModelList", envir=CPenv)[[ input$model ]]
    # chosenSim <- get("SimList", envir=CPenv)[[ input$sim ]]
    
    # Make simset for popan.setup.func.  Easiest way to make popan.func work.
    # Either copies of model values and constants for optimisation starting
    # values.
    fitparamdf <- chosenModel$paramdf
    fitparamdf$survrate <- 0.9
    fitparamdf$capturepr <- fitparamdf$prentry <- 0.99 / nrow(fitparamdf)
    fitSimset <- list(
      simname = input$fitname,
      description = input$fitdescrip,
      chosentimes = chosenModel$chosentimes,
      gapvec = chosenModel$gapvec,
      superpopn = 2 * nrow(chosenDataset$dataset),
      simtype = chosenModel$modeltype,
      lambdaparam = ifelse(chosenModel$lambdaparam == "", "", 1),
      paramdf = fitparamdf
    )
    
    # Start progress bar
    withProgress({
      incProgress(1 / 2)
      
      # Fit model
      res.fit <- popan.func(
        det.dat = as.matrix(chosenDataset$dataset),
        setup.res = popan.setup.func(model = chosenModel, simset = fitSimset)
        # setup.res = popan.setup.func(model = chosenModel, simset = chosenSim)
      )
      
      # Parameter estimates and standard errors
      before_first_var_index <- 
        match(paste0("var.", names(res.fit)[1]), names(res.fit)) - 1
      par.ests <- data.frame(matrix(res.fit[1:(2 * before_first_var_index)], 
                                    nrow = 2, byrow = T))
      rownames(par.ests) <- c("Estimates", "Standard Errors")
      colnames(par.ests) <- names(res.fit)[1:before_first_var_index]
      par.ests[2, par.ests[2, ] < 0] <- NA
      par.ests[2, ] <- sqrt(par.ests[2, ])
      parestsval(par.ests)
      
      # Estimated expected population size over time and standard errors
      first_Nt_index <- match("exp_n_alive1", names(res.fit))
      pop.ests <- data.frame(matrix(res.fit[first_Nt_index:length(res.fit)], 
                                    nrow = 2, byrow = T))
      rownames(pop.ests) <- c("Estimates", "Standard Errors")
      colnames(pop.ests) <- 
        chosenModel$paramdf$timelabels[chosenModel$chosentimes]
      varhat.Nhat <- as.numeric(pop.ests[2, ]^2)
      Nhat <- as.numeric(pop.ests[1, ])
      ci.C <- exp(1.959964 * sqrt(log(1 + varhat.Nhat / Nhat^2)))
      N.ci.low <- Nhat / ci.C
      N.ci.hi <- Nhat * ci.C
      pop.ests <- rbind(pop.ests, N.ci.low, N.ci.hi)
      rownames(pop.ests) <- 
        c(rownames(pop.ests)[1:2], "Log normal CI low", "Log normal CI high") 
      popestsval(pop.ests)
      
      # Model Comparison
      start <- 2 * before_first_var_index + 1
      mod.comp <- data.frame(matrix(res.fit[start:length(res.fit)], nrow = 1))
      mod.comp[, 2] <- as.integer(round(mod.comp[, 2]))
      mod.comp[, 5:6] <- as.logical(mod.comp[, 5:6])
      names(mod.comp) <- c(
        "Minimum Negative Log Likelihood",
        "Number of Parameters",
        "AIC",
        "Corrected AIC", 
        "Optimiser Converged",
        "Any Estimate Out of Bounds"
      )
      modcompval(mod.comp[1:5])
      
      incProgress(1 / 2)
    }, message = "Fitting...")
    
    ## Append the newly created fit to the fit list:
    FitList <- capow_list()$fit_list()
    fitcomps <- list(
      name = input$fitname, 
      description = input$fitdescrip,
      dataset = input$dataset, 
      model = input$model,
      fit = res.fit
    )
    FitList[[input$fitname]] <- fitcomps
    
    ## Write the updated fit list to CPenv:
    assign("FitList", FitList, envir=CPenv)
    capow_list()$fit_list(FitList)
    stopMessage("")
  }
  
  # Try to fit model when button pressed.
  observeEvent(input$fitbutton, {
    # Check that the number of columns in the dataset equals the number of
    # surveys in the model.
    chosenDataset <- get("DatasetList", envir=CPenv)[[ input$dataset ]]
    chosenModel <- get("ModelList", envir=CPenv)[[ input$model ]]
    if (ncol(chosenDataset$dataset) != length(chosenModel$chosentimes))
      stopMessage(
        "This Dataset and Model cover different numbers of survey years. You can 
      only fit models to datasets if they have the same number of survey years."
      )
    
    # Check that fitname entered.
    else if(input$fitname == "") stopMessage("Please enter a fit name")
    
    else {
      ## Get the existing fit list, even if empty:
      FitList.old <- capow_list()$fit_list()
      
      ## Run checks if there are existing fits: the new fit must have a different name
      ## and a new combination of dataset and model:
      if(length(FitList.old) > 0) {
        ## Make sure the fit name hasn't already been used:
        existing.fitnames <- unlist(lapply(FitList.old, function(x)x["name"]))
        if(any(existing.fitnames == input$fitname)) stopMessage("Fit name already taken")
        
        else {
          ## Make sure the combination of dataset and model hasn't already been used:
          existing.datasetnames <- unlist(lapply(FitList.old, function(x) x["dataset"]))
          existing.modelnames <- unlist(lapply(FitList.old, function(x) x["model"]))
          datasetmatch <- as.numeric(existing.datasetnames == input$dataset)
          modelmatch <- as.numeric(existing.modelnames == input$model)
          if(any(datasetmatch * modelmatch == 1)){
            which.match <- which(datasetmatch * modelmatch == 1)
            stopMessage(paste0("Fit ", existing.fitnames[which.match], " already has this combination of Dataset and Model."))
            
          } else fit_func()
        }
        
      } else fit_func()
    }
  })
  
  # Display parameter estimates
  parestsval <- reactiveVal(data.frame("Results" = "No model fit yet"))
  output$parests <- renderTable(parestsval(), digits = 3, rownames = T)
  
  # Display estimated population size over time
  popestsval <- reactiveVal(data.frame("Results" = "No model fit yet"))
  output$popests <- renderTable(popestsval(), digits = 3, rownames = T)
  output$Nt_plot <- renderPlot({
    # If model has been fit
    if(is.null(popestsval()$Results)) {
      # Make plot
      plot(
        as.numeric(colnames(popestsval())),
        as.numeric(popestsval()[1, ]), 
        main = "Expected population size over time",
        sub = "Log normal 95% confidence intervals",
        ylab = "Population size",
        xlab = "Time",
        ylim = c(0, max(popestsval()[4, ], na.rm = T)),
        type = 'b'
      )
      lines(as.numeric(colnames(popestsval())), popestsval()[3, ], lty = 2)
      lines(as.numeric(colnames(popestsval())), popestsval()[4, ], lty = 2)
      
    } else { ## If no model has been fit
      plot(0, main = "No model fit yet", col = "dark grey")
    }
  })
  
  # Model comparison
  modcompval <- reactiveVal(data.frame("Results" = "No models fit yet"))
  output$modcomp <- renderTable(modcompval(), digits = 6)
  
  # Display Fit, Dataset, and Model list summaries
  callModule(FitSummaryServer, "FitSummaryUI", capow_list)
  
  # Return reactive list of lists
  capow_list
}