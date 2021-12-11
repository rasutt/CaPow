# Server logic for Parameter Estimates module
ParameterEstimatesServer <- function(input, output, session, capow_list) {
  # Create namespace
  ns <- NS("ParameterEstimatesUI")
  
  # Render reactive UI for fit results
  output$fit_results <- renderUI({
    # Loop over list of fits making title and table for each
    tl <- list()
    for(i in seq(along = capow_list()$fit_list())) {
      title <- paste("Parameter Estimates -", capow_list()$fit_list()[[i]]$name)
      tl <- c(tl, list(h4(title)), list(tableOutput(ns(paste0("fit", i)))))
    }
    tagList(tl)
  })
  
  # Display parameter estimates whenever list of fits changes
  observeEvent(capow_list()$fit_list(), {
    # Loop through list of fits
    for(i in seq(along = capow_list()$fit_list())) {
      # Make each renderTable keep its value of i and not update to latest
      local({
        myi <- i
        
        # Render table of fit results
        output[[paste0("fit", myi)]] <- renderTable({
          # Get fit results
          res.fit <- capow_list()$fit_list()[[myi]]$fit
          
          # Find index of first variance in results
          n_pars <- match(paste0("var.", names(res.fit)[1]), names(res.fit)) - 1
          
          # Put estimates and variances into separate rows of a data frame
          par.ests <- data.frame(matrix(res.fit[1:(2 * n_pars)], nrow = 2, 
                                       byrow = T))
          
          # If any "negative variances" set standard errors to NA.  This avoids
          # sqrt giving warnings for negatives but allows them otherwise
          if (any(par.ests[2, ] < 0)) par.ests[2, par.ests[2, ] < 0] <- NA
          
          # Set standard errors to square roots of variances
          par.ests[2, ] <- sqrt(par.ests[2, ])
          
          # Name the rows and columns
          rownames(par.ests) <- c("Estimates", "Standard Errors")
          colnames(par.ests) <- names(res.fit)[1:n_pars]
          
          # Return the data frame
          par.ests
        }, 
        digits = 3, rownames = T)
      })
    }
  })
  
  # Display Fit, Dataset, and Model list summaries
  callModule(FitSummaryServer, "FitSummaryUI", capow_list)
}