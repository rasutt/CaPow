FitModelServer <- function(input, output, session, capow_list) {
  # Reactive component selection
  ns <- NS("FitModelUI")
  output$datasetselection <- renderUI(
    selectInput(ns("dataset"), "Choose a Dataset:", 
                choices = naturalsort(names(capow_list()$dataset_list()))))
  output$modelselection <- renderUI(
    selectInput(ns("model"), "Choose a Model:", 
                choices = naturalsort(names(capow_list()$model_list()))))
  # Leave the select simset option commented in in case find starting value problems later
  # and need to let users choose them.
  # output$simselection <- renderUI(
  #   selectInput(ns("sim"), "Choose a Sim (for starting parameters):", 
  #               choices = naturalsort(names(capow_list()$sim_list()))))
  
  # Error message
  stopMessage <- reactiveVal("")
  output$errormessage <- renderText(stopMessage())
  
  # Function to fit model to dataset.
  # Leave the select simset option commented in in case find starting value problems later
  # and need to let users choose them!
  fit_func <- function() {
    chosenDataset <- get("DatasetList", envir=CPenv)[[ input$dataset ]]
    chosenModel <- get("ModelList", envir=CPenv)[[ input$model ]]
    # chosenSim <- get("SimList", envir=CPenv)[[ input$sim ]]
    
    # Make simset for popan.setup.func.  Easiest way to make popan.func work.
    # Either copies of model values and constants for optimisation starting values.
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
    
    withProgress(
      {
        incProgress(1 / 2)
        # print("chosenModel")
        # print(chosenModel)
        # print("fitSimset")
        # print(fitSimset)
        res.fit <- popan.func(
          det.dat = as.matrix(chosenDataset$dataset),
          setup.res = popan.setup.func(model = chosenModel, simset = fitSimset)
          # setup.res = popan.setup.func(model = chosenModel, simset = chosenSim)
        )
        
        # Update results display
        
        # Parameter estimates and standard errors
        before_first_var_index <- match(paste0("var.", names(res.fit)[1]), names(res.fit)) - 1
        res.df1 <- data.frame(matrix(res.fit[1:(2 * before_first_var_index)], nrow = 2, byrow = T))
        rownames(res.df1) <- c("Estimates", "Standard Errors")
        colnames(res.df1) <- names(res.fit)[1:before_first_var_index]
        res.df1[2, res.df1[2, ] < 0] <- NA
        res.df1[2, ] <- sqrt(res.df1[2, ])
        fitresultsval1(res.df1)

        # Estimated expected population size over time and standard errors
        first_Nt_index <- match("exp_n_alive1", names(res.fit))
        res.df2 <- data.frame(matrix(res.fit[first_Nt_index:length(res.fit)], nrow = 2, byrow = T))
        rownames(res.df2) <- c("Estimates", "Standard Errors")
        colnames(res.df2) <- chosenModel$paramdf$timelabels[chosenModel$chosentimes]
        
        # Find log normal confidence interval
        varhat.Nhat <- as.numeric(res.df2[2, ]^2)
        Nhat <- as.numeric(res.df2[1, ])
        
        ci.C <- exp(1.959964 * sqrt(log(1 + varhat.Nhat / Nhat^2)))
        
        N.ci.low <- Nhat / ci.C
        N.ci.hi <- Nhat * ci.C
        
        # Add confidence intervals to dataframe
        res.df2 <- rbind(
          res.df2, 
          N.ci.low,
          N.ci.hi
        )
        rownames(res.df2) <- c(rownames(res.df2)[1:2], "Log normal CI low", "Log normal CI high") 
        
        # Set results display value
        fitresultsval2(res.df2)
        
        # Model Comparison
        res.df3 <- data.frame(matrix(res.fit[(2 * before_first_var_index + 1):length(res.fit)], nrow = 1))
        res.df3[, 2] <- as.integer(round(res.df3[, 2]))
        res.df3[, 5:6] <- as.logical(res.df3[, 5:6])
        # names(res.df3) <- names(res.fit)[(2 * before_first_var_index + 1):length(res.fit)]
        names(res.df3) <- c(
          "Minimum Negative Log Likelihood",
          "Number of Parameters",
          "AIC",
          "Corrected AIC", 
          "Optimiser Converged",
          "Any Estimate Out of Bounds"
        )
        # fitresultsval3(res.df3[1:6])
        
        # Out of bounds flag not updated yet?
        fitresultsval3(res.df3[1:5])
        
        incProgress(1 / 2)
      },
      message = "Fitting..."
    )
    
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
    # Check that the number of columns in the dataset equals the number of surveys in the model.
    chosenDataset <- get("DatasetList", envir=CPenv)[[ input$dataset ]]
    chosenModel <- get("ModelList", envir=CPenv)[[ input$model ]]
    if (ncol(chosenDataset$dataset) != length(chosenModel$chosentimes))
      stopMessage("This Dataset and Model cover different numbers of survey years.
                  You can only fit models to datasets if they have the same number of survey years.")
    
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
  
  # Display fit results
  # Parameters
  fitresultsval1 <- reactiveVal(data.frame("Results" = "No model fit yet"))
  output$fitresults1 <- renderTable(fitresultsval1(), digits = 3, rownames = T)
  
  # Population size over time
  fitresultsval2 <- reactiveVal(data.frame("Results" = "No model fit yet"))
  output$fitresults2 <- renderTable(fitresultsval2(), digits = 3, rownames = T)
  output$Nt_plot <- renderPlot({
    # If model has been fit
    if(is.null(fitresultsval2()$Results)) {
      # Find log normal confidence interval
      varhat.Nhat <- as.numeric(fitresultsval2()[2, ]^2)
      Nhat <- as.numeric(fitresultsval2()[1, ])

      ci.C <- exp(1.959964 * sqrt(log(1 + varhat.Nhat / Nhat^2)))
      
      N.ci.low <- Nhat / ci.C
      N.ci.hi <- Nhat * ci.C
  
      N.ci.low[is.na(N.ci.low)] <- Nhat[is.na(N.ci.low)]
      N.ci.hi[is.na(N.ci.hi)] <- Nhat[is.na(N.ci.hi)]
      N.ci.low[is.nan(N.ci.low)] <- Nhat[is.nan(N.ci.low)]
      N.ci.hi[is.nan(N.ci.hi)] <- Nhat[is.nan(N.ci.hi)]

      # print(N.ci.low)
      # print(N.ci.hi)
      
      # Make plot
      {
        plot(
          as.numeric(colnames(fitresultsval2())),
          as.numeric(fitresultsval2()[1, ]), 
          main = "Expected population size over time",
          sub = "Log normal 95% confidence intervals",
          ylab = "Population size",
          xlab = "Time",
          ylim = c(0, max(N.ci.hi, na.rm = T)),
          type = 'b'
        )
        
        lines(
          as.numeric(colnames(fitresultsval2())),
          N.ci.low,
          lty = 2
        )
        lines(
          as.numeric(colnames(fitresultsval2())),
          N.ci.hi,
          lty = 2
        )
        
        # legend(
        #   "bottomleft",
        #   legend = c("estimates", "log normal 95% confidence intervals"),
        #   lty = c(1, 2)
        # )
      }      
    } else { ## If no model has been fit
      plot(
        0, 
        main = "No model fit yet",
        col = "dark grey"
      )
    }
  })
  
  # Metadata. Not showing for now?
  fitresultsval3 <- reactiveVal(data.frame("Results" = "No models fit yet"))
  output$fitresults3 <- renderTable(fitresultsval3(), digits = 6)
  
  # Display Fit, Dataset, and Model list summaries
  callModule(FitSummaryServer, "FitSummaryUI", capow_list)
  
  # Return reactive list of lists
  capow_list
}