# Server logic for ModelComp module
ModelCompServer <- function(input, output, session, capow_list) {
  # Create namespace
  ns <- NS("ModelCompUI")
  
  # Render reactive UI for model comparison results
  output$model_comp <- renderUI({
    # Make empty list
    tl <- list()
    
    # Loop through list of fits
    for(i in seq(along = capow_list()$fit_list())) {
      # Make title for fit
      tl <- c(tl, list(h4(paste("Model Comparison -", 
                                capow_list()$fit_list()[[i]]$name))))
      
      # Make table for fit results
      tl <- c(tl, list(tableOutput(ns(paste0("model_comp", i)))))
    }
    
    # Turn list into tag list and return it
    tagList(tl)
  })
  
  # Reactively print model comparison results when list of fits changes
  observeEvent(capow_list()$fit_list(), {
    # Loop through list of fits
    for(i in seq(along = capow_list()$fit_list())) {
      # Make each renderTable keep its value of i and not update to latest
      local({
        myi <- i
        
        # Render table of fit results
        output[[paste0("model_comp", myi)]] <- renderTable({
          # Get fit results
          res.fit <- capow_list()$fit_list()[[myi]]$fit
          
          # Find index of first variance in results
          n_pars <- match(paste0("var.", names(res.fit)[1]), names(res.fit)) - 1
          
          # Find model comparison features
          mod.comp <- data.frame(
            matrix(res.fit[(2 * n_pars + 1):length(res.fit)], nrow = 1)
          )
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

          # Return the data frame
          mod.comp[1:5]
        }, digits = 3)
      })
    }
  })
  
  # Display Fit, Dataset, and Model list summaries
  callModule(FitSummaryServer, "FitSummaryUI", capow_list)
}