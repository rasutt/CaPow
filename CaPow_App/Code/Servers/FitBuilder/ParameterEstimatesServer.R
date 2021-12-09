# Server logic for Parameter Estimates module
ParameterEstimatesServer <- function(input, output, session, capow_list) {
  # Create namespace
  ns <- NS("ParameterEstimatesUI")
  
  # Render reactive UI for fit results
  output$fit_results <- renderUI({
    # Make empty list
    tl <- list()
    
    # Loop through list of fits
    for(i in seq(along = capow_list()$fit_list())) {
      # Make title for fit
      tl <- c(tl, list(h4(paste("Parameter Estimates -", capow_list()$fit_list()[[i]]$name))))
      
      # Make table for fit results
      tl <- c(tl, list(tableOutput(ns(paste0("fit", i)))))
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
        output[[paste0("fit", myi)]] <- renderTable({
          # Get fit results
          res.fit <- capow_list()$fit_list()[[myi]]$fit
          
          # Find index of first variance in results
          before_first_var_index <- match(paste0("var.", names(res.fit)[1]), names(res.fit)) - 1
          
          # Put estimates and variances into separate rows of a data frame
          res.df1 <- data.frame(matrix(res.fit[1:(2 * before_first_var_index)], nrow = 2, byrow = T))
          
          # If any "negative variances" set standard errors to NA.  This avoids
          # sqrt giving warnings for negatives but allows them otherwise
          if (any(res.df1[2, ] < 0)) {
            res.df1[2, res.df1[2, ] < 0] <- NA
          }
          
          # Set standard errors to square roots of variances
          res.df1[2, ] <- sqrt(res.df1[2, ])
          
          # Name the rows and columns
          rownames(res.df1) <- c("Estimates", "Standard Errors")
          colnames(res.df1) <- names(res.fit)[1:before_first_var_index]
          
          # Include lambda and phi if held constant - assumes only ones
          # Commenting it out as I might want to do it properly later
          # chosenModel <- get("ModelList", envir=CPenv)[[capow_list()$fit_list()[[myi]]$model]]
          # if(chosenModel$modeltype == "lambdamodel" && !("lambda" %in% colnames(res.df1))) {
          #   res.df1 <- cbind(
          #     N = res.df1$N, 
          #     lambda = c(as.numeric(chosenModel$lambdaparam), NA), 
          #     phi1 = c(as.numeric(chosenModel$paramdf$survrate[1]), NA), 
          #     res.df1[, -1]
          #   )
          # }
          
          # Include calculated pent1 parameter if not lambda model (just for example fit - change later!)
          # Commenting it out as small chance I might want to do this properly later
          # if(chosenModel$modeltype != "lambdamodel") {
          #   # Find index of calculated pent parameter in model parameter table
          #   calcind <- match(x = "calculated", table = chosenModel$paramdf$prentry)
          #   
          #   # Insert in fit results assuming 8 parameters to the left and and 6 to the right lol
          #   res.df1 <- cbind(
          #     res.df1[, 1:8],
          #     pent1 = c(1 - sum(res.df1[1, 9:14]), NA),
          #     res.df1[, 9:14]
          #   )
          # }
          
          # print("chosenModel")
          # print(chosenModel)
          
          # Return the data frame
          res.df1
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