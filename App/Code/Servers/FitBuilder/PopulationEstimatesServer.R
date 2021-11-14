# Server logic for Parameter Estimates module
PopulationEstimatesServer <- function(input, output, session, capow_list) {
  # Create namespace
  ns <- NS("PopulationEstimatesUI")
  
  # Render reactive UI for fit results
  output$pop_tables <- renderUI({
    # Make empty list
    tl <- list()
    
    # Loop through list of fits
    for(i in seq(along = capow_list()$fit_list())) {
      # Make title for fit
      tl <- c(tl, list(h4(paste("Population Estimates -", capow_list()$fit_list()[[i]]$name))))
      
      # Make table for fit results
      tl <- c(tl, list(tableOutput(ns(paste0("pop_table", i)))))
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
        output[[paste0("pop_table", myi)]] <- renderTable({
          # Get fit results
          res.fit <- capow_list()$fit_list()[[myi]]$fit
          
          # Get corresponding model
          chosenModel <- capow_list()$model_list()[[capow_list()$fit_list()[[myi]]$model]]
          
          # print(res.fit)
          
          # Estimated expected population size over time and standard errors
          first_Nt_index <- match("exp_n_alive1", names(res.fit))
          res.df <- data.frame(matrix(res.fit[first_Nt_index:length(res.fit)], nrow = 2, byrow = T))
          rownames(res.df) <- c("Estimates", "Standard Errors")
          colnames(res.df) <- chosenModel$paramdf$timelabels[chosenModel$chosentimes]
          
          # Find log normal confidence interval
          varhat.Nhat <- as.numeric(res.df[2, ]^2)
          Nhat <- as.numeric(res.df[1, ])
          
          ci.C <- exp(1.959964 * sqrt(log(1 + varhat.Nhat / Nhat^2)))
          
          N.ci.low <- Nhat / ci.C
          N.ci.hi <- Nhat * ci.C
          
          # N.ci.low[is.na(N.ci.low)] <- Nhat[is.na(N.ci.low)]
          # N.ci.hi[is.na(N.ci.hi)] <- Nhat[is.na(N.ci.hi)]
          # N.ci.low[is.nan(N.ci.low)] <- Nhat[is.nan(N.ci.low)]
          # N.ci.hi[is.nan(N.ci.hi)] <- Nhat[is.nan(N.ci.hi)]
          
          # print(N.ci.low)
          # print(N.ci.hi)
          
          # Add confidence intervals to dataframe
          res.df <- rbind(
            res.df, 
            N.ci.low,
            N.ci.hi
          )
          rownames(res.df) <- c(rownames(res.df)[1:2], "Log normal CI low", "Log normal CI high") 
          
          # Return the data frame
          res.df
        },
        digits = 3,
        rownames = T
        )
      })
    }
  })
  
  # Display Fit, Dataset, and Model list summaries
  callModule(FitSummaryServer, "FitSummaryUI", capow_list)
}