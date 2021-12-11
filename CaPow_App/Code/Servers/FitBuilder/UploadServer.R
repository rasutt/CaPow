# Server logic for Upload module
UploadServer <- function(input, output, session, capow_list) {
  # Setup error message and head of file display
  stopMessage <- reactiveVal("")
  filehead <- reactiveVal("No File selected")
  
  # Try to read file
  observeEvent(input$file, {
    # input$file will be NULL initially. After the user selects and uploads a
    # file, the head will be shown.
    req(input$file)
    tryCatch(
      df <- read.csv(input$file$datapath, header = input$header),
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
        stopMessage("Error reading file")
      }
    )
    if(is.data.frame(df)) {
      # Fix X's in colnames if necessary
      if(substr(names(df)[1], 1, 1) == "X") {
        names(df) <- substring(names(df), 2)
      }
      
      # Remove rownames
      row.names(df) <- NULL
      filehead(head(df))
      stopMessage("")
    }
    else filehead(NULL)
  })
  
  # Display head of file or error message
  output$head <- renderTable(filehead())
  output$errormessage <- renderText(stopMessage())
  
  # Try to upload dataset
  observeEvent(input$uploadbutton, {
    req(input$file)
    tryCatch(
      df <- read.csv(input$file$datapath, header = input$header),
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
        stopMessage("Error reading file")
      }
    )
    if(is.data.frame(df)) {
      # Fix X's in colnames if necessary
      if(substr(names(df)[1], 1, 1) == "X") {
        names(df) <- substring(names(df), 2)
      }
      
      # Remove rownames
      row.names(df) <- NULL
      
      datasetnames <- 
        unlist(lapply(capow_list()$dataset_list(), function(x) x$datasetname))
      if(input$dataname %in% datasetnames)
        stopMessage("Dataset with that name already exists")
      else if(input$dataname == "")
        stopMessage("Please enter a name for the dataset and click upload button again")
      else {
        currentDatasetList <- capow_list()$dataset_list()
        currentDatasetList[[input$dataname]] <- 
          list(
            "datasetname" = input$dataname,
            "description" = input$datadescrip,
            "dataset" = df
          )
        assign("DatasetList", currentDatasetList, envir = CPenv)
        capow_list()$dataset_list(currentDatasetList)
        stopMessage("")
      }
    }
  })
  
  # Display list summaries
  callModule(FitSummaryServer, "FitSummaryUI", capow_list)
  
  # Return reactive list of lists
  capow_list
}