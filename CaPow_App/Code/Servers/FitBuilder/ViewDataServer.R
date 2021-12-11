# Server logic for View Data module
ViewDataServer <- function(input, output, session, capow_list) {
  # Create namespace
  ns <- NS("ViewDataUI")

  # Render reactive UI for datasets
  output$data_heads <- renderUI({
    # Make empty list
    tl <- list()
    
    # Loop through list of datasets
    for(i in seq(along = capow_list()$dataset_list())) {
      # Make title for dataset
      tl <- c(tl, list(h4(paste("Head of Dataset -", 
                                capow_list()$dataset_list()[[i]]$datasetname))))
      
      # Make table for dataset head
      tl <- c(tl, list(tableOutput(ns(paste0("dataset", i)))))
    }
    
    # Turn list into tag list and return it
    tagList(tl)
  })
  
  # Reactively print dataset heads when list of datasets changes
  observeEvent(capow_list()$dataset_list(), {
    # Loop through list of datasets
    for(i in seq(along = capow_list()$dataset_list())) {
      # This somehow stops each renderTable from just using the latest value of
      # i, I don't understand it right now, but it's really important
      local({
        myi <- i
        
        # Render table of dataset heads
        output[[paste0("dataset", myi)]] <- renderTable(
          head(capow_list()$dataset_list()[[myi]]$dataset)
        )
      })
    }
  })
  
  # Display Fit, Dataset, and Model list summaries
  callModule(FitSummaryServer, "FitSummaryUI", capow_list)
}