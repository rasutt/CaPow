# Server logic for ModelComp module
ModelCompServer <- function(input, output, session, capow_list) {
  # Create namespace
  ns <- NS("ModelCompUI")
  
  # Render reactive UI for fit results
  output$model_comp <- renderUI({
    # Make empty list
    tl <- list()
    
    # Loop through list of fits
    for(i in seq(along = capow_list()$fit_list())) {
      # Make title for fit
      tl <- c(tl, list(h4(paste("Model Comparison -", capow_list()$fit_list()[[i]]$name))))
      
      # Make table for fit results
      tl <- c(tl, list(tableOutput(ns(paste0("model_comp", i)))))
    }
    
    # Turn list into tag list and return it
    tagList(tl)
  })
  
  # Reactively print fit results when list of fits changes
  observeEvent(capow_list()$fit_list(), {
    # Loop through list of fits
    for(i in seq(along = capow_list()$fit_list())) {
      # This somehow stops each renderTable from just using the latest value of
      # i, I don't understand it right now, but it's really important
      local({
        myi <- i
        
        # Render table of fit results
        output[[paste0("model_comp", myi)]] <- renderTable({
          # Get fit results
          res.fit <- capow_list()$fit_list()[[myi]]$fit
          
          # Find index of first variance in results
          before_first_var_index <- match(paste0("var.", names(res.fit)[1]), names(res.fit)) - 1
          
          # Find model_comp
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

          # Return the data frame
          # res.df3[1:6]
          # Out of bounds flag not updated yet?
          res.df3[1:5]
          
        },
        digits = 3
        )
      })
    }
  })
  
  # Display Fit, Dataset, and Model list summaries
  callModule(FitSummaryServer, "FitSummaryUI", capow_list)
}