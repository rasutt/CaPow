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