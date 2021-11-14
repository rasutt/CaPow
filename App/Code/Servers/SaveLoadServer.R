# Server logic for Save module
SaveServer <- function(input, output, session, capow_list) {
  # Setup error message display
  stopMessage <- reactiveVal("")
  
  # Display error message
  output$errormessage <- renderText(stopMessage())
  
  # Try to save session and download it
  output$savesession <- downloadHandler(
    # These values must be wrapped in functions for some reason
    # Make filename from input
    filename = function() {
      input$filenameinput
    },

    # Save CPenv to file
    content = function(file) {
      save(CPenv, file = file, version = 2)
    }
  )
  
  # Display list summaries
  callModule(SummaryServer, "SummaryUI", capow_list)
}

# Server logic for Load module
LoadServer <- function(input, output, session, capow_list) {
  # Setup error message display
  stopMessage <- reactiveVal("")
  
  # Display error message
  output$errormessage <- renderText(stopMessage())
  
  # Try to load session
  observeEvent(
    input$loadbutton,
    {
      # input$file will be NULL initially.
      req(input$file)
      tryCatch(
        {
          # Load the saved CPenv
          load(input$file$datapath)
          
          # Somehow this superassignment writes this CPenv over the parent version 
          CPenv <<- CPenv
          
          # Update lists
          capow_list()$sim_list(get0("SimList", envir = CPenv))
          capow_list()$model_list(get0("ModelList", envir = CPenv))
          capow_list()$project_list(get0("ProjectList", envir = CPenv))
          capow_list()$dataset_list(get0("DatasetList", envir = CPenv))
          capow_list()$fit_list(get0("FitList", envir = CPenv))
          
          stopMessage("")
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          # I haven't seen one yet
          stop(safeError(e))
          # stopMessage("Error loading file")
        }
      )
    }
  )
  
  # Display list summaries
  callModule(SummaryServer, "SummaryUI", capow_list)
  
  # Return reactive list of lists
  capow_list
}