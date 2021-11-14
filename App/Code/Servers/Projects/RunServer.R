# Server logic for Run module
RunServer <- function(input, output, session, capow_list) {
  # Reactive to find projects not run
  projects_not_run <- reactive({
    all.projects <- capow_list()$project_list()
    all.proj.run <- unlist(lapply(all.projects, function(x)x["run"]))
    names(all.projects)[all.proj.run==0]
  })
  
  # Indicate when no projects not run
  output$noprojects <- reactive(
    if(length(projects_not_run()) == 0) HTML(paste("No projects to run"))
    else HTML(paste(""))
  )
  
  # Reactive checkbox for project selection
  ns <- NS("RunUI")
  output$projselect <- renderUI(
    checkboxGroupInput(
      ns("projrun"), 
      "Choose Projects to run:", 
      choices = projects_not_run(),
      selected = NULL
    )
  )
  
  # Run projects when button clicked
  observeEvent(
    input$runbutton, 
    {
      if(length(input$projrun) > 0){
        # Loop over selected projects
        for(singleproj in input$projrun){
          # Get project
          chosenProject <- capow_list()$project_list()[[singleproj]]
          
          # If not yet run
          if(chosenProject["run"] != 1) {
            # Start progress bar
            withProgress(
              {
                # Get sim  
                chosenSim <- capow_list()$sim_list()[[chosenProject[["simset"]]]]
                
                # If it includes real data
                if(!(is.null(chosenSim[["fit"]]) || chosenSim[["fit"]] == "None")) {
                  # Get real data
                  chosenFit <- capow_list()$fit_list()[[chosenSim[["fit"]]]]
                  chosenDataset <- capow_list()$dataset_list()[[chosenFit[["dataset"]]]]
                  
                  # Run project
                  res.proj <- popan.sim.wrap(
                    Nsim = input$Nsim,
                    existing_data = chosenDataset[["dataset"]],
                    projname = singleproj
                  )
                  
                  # If no real data
                } else {
                  res.proj <- popan.sim.wrap(Nsim = input$Nsim, projname = singleproj)
                }
              },
              value = 0,
              message = paste("Running project", singleproj, "...")
            )
          }
        }
      }
      
      # Update reactive list.  CPenv is updated by popan.sim.wrap above.
      capow_list()$project_list(get("ProjectList", envir=CPenv))
    }
  )
  
  # Display summary lists
  callModule(SummaryServer, "SummaryUI", capow_list)
  
  # Return reactive objects list
  capow_list
}