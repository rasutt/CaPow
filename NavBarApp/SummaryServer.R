# Server logic for Summary module
SummaryServer <- function(input, output, session, capow_list) {
  # Display list summaries
  
  # Projects
  output$projlist <- renderTable({
    ## If projects already exist, grab them from CPenv, otherwise return a table with nothing in it:
    allproj <- capow_list()$project_list()
    if(length(allproj) > 0){
      ## Create the list of all projects for display:
      allproj <- allproj[naturalorder(names(allproj))]
      allprojnames <- unlist(lapply(allproj, function(x) x["name"]))
      allprojdescrip <- unlist(lapply(allproj, function(x) x["description"]))
      allprojsim <- unlist(lapply(allproj, function(x) x["simset"]))
      allprojmodel <- unlist(lapply(allproj, function(x) x["model"]))
      allprojrun <- unlist(lapply(allproj, function(x) x["run"]))
      
      data.frame(
        row.names = NULL,
        "Name" = allprojnames,
        "Description" = allprojdescrip,
        "Run" = allprojrun,
        "Simulation" = allprojsim,
        "Model"= allprojmodel
      )
    }
    else {
      data.frame("Name" = "No projects created")
    }
  })
  
  # Simulations - Should this also include whether and which fits are included? 
  output$simlist <- renderTable({
    if(length(capow_list()$sim_list()) > 0){
      allsims <- capow_list()$sim_list()
      allsims <- allsims[naturalorder(names(allsims))]
      data.frame(
        row.names = NULL,
        "Name" = unlist(lapply(allsims, function(x) x$simname)),
        "Description" = unlist(lapply(allsims, function(x) x$description))
      )
    }
    else {
      data.frame("Name" = "No Sims created")
    }
  })
  
  # Fits, Datasets, and Models
  callModule(FitSummaryServer, "FitSummaryUI", capow_list)
}