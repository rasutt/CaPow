# Server logic for View Data module
ViewPopPlotsServer <- function(input, output, session, capow_list) {
  # Create namespace
  ns <- NS("ViewPopPlotsUI")
  
  # Render reactive UI for fit results
  output$Nt_plots <- renderUI({
    # Make empty list
    tl <- list()
    
    # Loop through list of fits
    for(i in seq(along = capow_list()$fit_list())) {
      # Make title for fit
      tl <- c(tl, list(h4(paste("Population Plot -", capow_list()$fit_list()[[i]]$name))))
      
      # Make table for fit results
      tl <- c(tl, list(plotOutput(ns(paste0("plot", i)), width = 800, height = 450)))
    }
    
    # Turn list into tag list and return it
    tagList(tl)
  })
  
  # Reactively plot expected population size over time when list of fits changes
  observeEvent(capow_list()$fit_list(), {
    # Loop through list of fits
    for(i in seq(along = capow_list()$fit_list())) {
      # This somehow stops each renderTable from just using the latest value of
      # i, I don't understand it right now, but it's really important
      local({
        myi <- i
        
        # Render table of fit results
        output[[paste0("plot", myi)]] <- renderPlot({
          
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
          
          N.ci.low[is.na(N.ci.low)] <- Nhat[is.na(N.ci.low)]
          N.ci.hi[is.na(N.ci.hi)] <- Nhat[is.na(N.ci.hi)]
          N.ci.low[is.nan(N.ci.low)] <- Nhat[is.nan(N.ci.low)]
          N.ci.hi[is.nan(N.ci.hi)] <- Nhat[is.nan(N.ci.hi)]
          
          # print(N.ci.low)
          # print(N.ci.hi)
          
          # Make plot
          {
            plot(
              as.numeric(colnames(res.df)),
              res.df[1, ], 
              main = "Expected population size over time",
              sub = "Log normal 95% confidence intervals",
              ylab = "Population size",
              xlab = "Time",
              ylim = c(0, max(as.numeric(N.ci.hi), na.rm = T)),
              type = 'b'
            )
            
            lines(
              as.numeric(colnames(res.df)),
              N.ci.low,
              lty = 2
            )
            lines(
              as.numeric(colnames(res.df)),
              N.ci.hi,
              lty = 2
            )
            
            # legend(
            #   "bottomleft",
            #   legend = c("estimates", "log normal 95% confidence intervals"),
            #   lty = c(1, 2)
            # )
          }    
          
          # print("chosenModel")
          # print(chosenModel)
        })
      })
    }
  })
  
  # Display Fit, Dataset, and Model list summaries
  callModule(FitSummaryServer, "FitSummaryUI", capow_list)
}