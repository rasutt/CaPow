# Server logic for Parameter Estimates module
PopulationEstimatesServer <- function(input, output, session, capow_list) {
  # Create namespace
  ns <- NS("PopulationEstimatesUI")
  
  # Create UI for fit results
  output$pop_tables <- renderUI({
    # Loop over list of fits making title and table for each
    tl <- list()
    for(i in seq(along = capow_list()$fit_list())) {
      title <- paste("Population Estimates -", 
                     capow_list()$fit_list()[[i]]$name)
      table <- tableOutput(ns(paste0("pop_table", i)))
      tl <- c(tl, list(h4(title)), list(table))
    }
    tagList(tl)
  })
  
  # Display population estimates whenever list of fits changes
  observeEvent(capow_list()$fit_list(), {
    # Loop through list of fits
    for(i in seq(along = capow_list()$fit_list())) {
      # Make each renderTable keep its value of i and not update to latest
      local({
        myi <- i
        
        # Render table of fit results
        output[[paste0("pop_table", myi)]] <- renderTable({
          # Get fit results
          fit_list_i <- capow_list()$fit_list()[[myi]]
          res.fit <- fit_list_i$fit
          
          # Get corresponding model
          mod <- capow_list()$model_list()[[fit_list_i[[myi]]$model]]

          # Estimated expected population size over time and standard errors
          start <- match("exp_n_alive1", names(res.fit))
          pop.ests <- data.frame(matrix(res.fit[start:length(res.fit)], 
                                        nrow = 2, byrow = T))
          rownames(pop.ests) <- c("Estimates", "Standard Errors")
          colnames(pop.ests) <- mod$paramdf$timelabels[mod$chosentimes]
          
          # Find log normal confidence interval
          varhat.Nhat <- as.numeric(pop.ests[2, ]^2)
          Nhat <- as.numeric(pop.ests[1, ])
          ci.C <- exp(1.959964 * sqrt(log(1 + varhat.Nhat / Nhat^2)))
          N.ci.low <- Nhat / ci.C
          N.ci.hi <- Nhat * ci.C
          
          # Add confidence intervals to dataframe
          pop.ests <- rbind(pop.ests, N.ci.low, N.ci.hi)
          rownames(pop.ests) <- c(rownames(pop.ests)[1:2], "Log normal CI low", 
                                  "Log normal CI high") 
          
          # Return the data frame
          pop.ests
        }, 
        digits = 3, rownames = T)
      })
    }
  })
  
  # Display Fit, Dataset, and Model list summaries
  callModule(FitSummaryServer, "FitSummaryUI", capow_list)
}