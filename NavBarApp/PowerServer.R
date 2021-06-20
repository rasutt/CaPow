# Server logic for Power module
PowerServer <- function(input, output, session, capow_list) {
  # Reactive checkbox selection for project results to analyze
  projects_run <- reactive({
    all.projects <- capow_list()$project_list()
    all.proj.run <- unlist(lapply(all.projects, function(x)x["run"]))
    names(all.projects)[all.proj.run==1]
  })
  output$projSelectBox <- renderUI({
    ns <- NS("PowerUI")
    checkboxGroupInput(
      ns("projSelect"), 
      "Include Projects:",
      choices = projects_run()
    )
  })
  
  ## ---------------------------------------------------------------------------------------------------
  ## SETTINGS CODE
  ## ---------------------------------------------------------------------------------------------------
  ## This code-block determines what settings are available to be input,
  ## based on the global selections made on the side-panel.
  ## For example, if lambda or N are selected on the side panel, there is no
  ## choice of time-labels to make, whereas if phi, pent, or p are selected, we
  ## need to determine which time-labels to display output for.
  
  ## Note: we define output$Settings twice: once with an observeEvent on input$projSelect,
  ## which expands or contracts the list of available time labels;
  ## and once with an observeEvent on input$parType, which resets the *selected* time labels
  ## (i.e. input$timeLabels) to NULL.  This second observeEvent ensures that we don't get errors
  ## when parType is changed to a type that doesn't have results for the most recently selected timelabels.
  
  ## 1: RESPONSE OF OUTPUT$SETTINGS TO A CHANGE IN WHICH PROJECTS ARE SELECTED:
  ## In this case, keep the current selections for input$timeLabels, but expand or contract the list of
  ## available choices based on the new selection of projects.
  observeEvent(
    input$projSelect,
    {
      output$Settings <- renderUI({
        if(input$parType %in% c("phi", "pent", "p")){
          ## For phi, pent, and p, we need to find all timelabels associated with
          ## all selected projects:
          allTimeLabels <- NULL
          if(length(input$projSelect)>0) {
            for(i in 1:length(input$projSelect)){
              resultsProj <- get(paste0("result.", input$projSelect[i]), envir=CPenv)
              modelTableProj <- attributes(resultsProj)$setup$ModelTable
              parcolm <- input$parType
              if(parcolm %in% c("phi", "p")) parcolm <- paste0(parcolm, "vec")
              allTimeLabels <- c(allTimeLabels,
                                 modelTableProj$time[
                                   grep(input$parType, modelTableProj[, parcolm])])
            }
            allTimeLabels <- unique(allTimeLabels)
            ## If allTimeLabels is numeric, use naturalsort:
            if(!any(is.na(suppressWarnings(as.numeric(allTimeLabels)))))
              allTimeLabels <- naturalsort(allTimeLabels)
            
            # Create namespace for outputs below
            ns <- NS("PowerUI")
            
            checkboxGroupInput(ns("timeLabels"), "Select Times",
                               choices = allTimeLabels,
                               selected = input$timeLabels)
            ## Using selected=input$timeLabels is what distinguishes option 1 for output$Settings
          }  ## End any projects selected
        }  ## End of parameter being one of phi, pent, p
      })  ## End renderUI for output$Settings
    }) ## End observeEvent
  
  ## 2: RESPONSE OF OUTPUT$SETTINGS TO A CHANGE IN PARAMETER TYPE:
  ## In this case, reset input$timeLabels to NULL below.
  ## This code block also resets output$PowerDisplay to NULL, because it has been invalidated by the
  ## change in input$parType.
  observeEvent(
    input$parType,
    {
      output$Settings <- renderUI({
        if(input$parType %in% c("phi", "pent", "p")){
          ## For phi, pent, and p, we need to find all timelabels associated with
          ## all selected projects:
          allTimeLabels <- NULL
          if(length(input$projSelect)>0) {
            for(i in 1:length(input$projSelect)){
              resultsProj <- get(paste0("result.", input$projSelect[i]), envir=CPenv)
              modelTableProj <- attributes(resultsProj)$setup$ModelTable
              parcolm <- input$parType
              if(parcolm %in% c("phi", "p")) parcolm <- paste0(parcolm, "vec")
              allTimeLabels <- c(allTimeLabels,
                                 modelTableProj$time[
                                   grep(input$parType, modelTableProj[, parcolm])])
            }
            allTimeLabels <- unique(allTimeLabels)
            ## If allTimeLabels is numeric, use naturalsort:
            if(!any(is.na(suppressWarnings(as.numeric(allTimeLabels)))))
              allTimeLabels <- naturalsort(allTimeLabels)
            
            # Create namespace for outputs below
            ns <- NS("PowerUI")
            
            checkboxGroupInput(ns("timeLabels"), "Select Times",
                               choices = allTimeLabels,
                               selected = NULL)
          }  ## End any projects selected
        }  ## End of parameter being one of phi, pent, p
        else NULL
      })  ## End renderUI for output$Settings
      
      ## ALSO RESET THE POWER-TABLE AND POWER-PLOT OUTPUTS TO NULL:
      output$PowerDisplay <- renderUI(NULL)
    }) ## End observeEvent for change in parType
  
  ## ---------------------------------------------------------------------------------------------------
  ## POWER TABLES AND PLOT CODE
  ## ---------------------------------------------------------------------------------------------------
  ## The line below sets up a dependence on any of the four global inputs involving which
  ## projects and parameters are selected, confidence level and collapse behaviour:
  observeEvent(
    input$powerSubmit,
    if(length(input$projSelect) > 0)
    {
      ## For some reason, using parIn instead of input$parType everywhere seems to stop
      ## the interface freezing when changing from a plot display of N back to a plot display of lambda,
      ## which seems to happen predictably (but just for this combination) without this intervention.
      parIn <- input$parType
      
      ## Coerce text inputs to numeric:
      gridspaceReqd <- suppressWarnings(as.numeric(eval(parse(text=paste0("input$Grid", parIn)))))
      ##
      Cmain <- suppressWarnings(as.numeric(input$CexMain))
      Caxis <- suppressWarnings(as.numeric(input$CexAxis))
      Cpoints <- suppressWarnings(as.numeric(input$CexPoints))
      ##
      Tsp <- suppressWarnings(c(as.numeric(input$plotFoot), as.numeric(input$plotHead)))
      
      ## ------------------------------------------------------------------------------------------
      ## SET UP FOR MULTIPLE RESULTS: POWER SUMMARY TABLES
      ## ------------------------------------------------------------------------------------------
      ## Set plotlength for the right number of outputs:
      if(parIn %in% c("lambda", "N")) tablength <- 1
      else tablength <- length(input$timeLabels)
      
      ## Create a list of outputs to be filled with titles, tables, and plots:
      output$PowerDisplay <- renderUI({
        ## Give each table a unique name to refer to in the output.
        ## We need a list of alternating title names, then tables, then plots:
        ## the total length of the list needs to be 3*tablength to give a title, table, and plot for each
        ## time label.
        PowerDisplayList <- vector("list", 3*tablength)
        
        # Create namespace for outputs below
        ns <- NS("PowerUI")
        
        ## Fill in the title slots of PowerDisplayList: slots 1, 4, 7, ...
        PowerDisplayList[seq(from=1, by=3, length=tablength)] <-
          lapply(1:tablength, function(i) {
            titlename <- paste("PowerTitle", i, sep="")
            htmlOutput(ns(titlename))
          })
        
        ## Fill in the table slots of PowerDisplayList: slots 2, 5, 8, ...
        PowerDisplayList[seq(from=2, by=3, length=tablength)] <-
          lapply(1:tablength, function(i) {
            tabname <- paste("PowerTable", i, sep="")
            tableOutput(ns(tabname))
          })
        
        ## Fill in the plot slots of PowerDisplayList: slots 2, 5, 8, ...
        PowerDisplayList[seq(from=3, by=3, length=tablength)] <-
          lapply(1:tablength, function(i) {
            plotname <- paste("PowerPlot", i, sep="")
            plotOutput(ns(plotname), height=input$plotHeight)
          })
        
        ## PowerDisplayList is a list of HTML tags, with list elements like:
        ## [[1]]
        ## <div id="PowerTitle1" class="shiny-html-output"></div>
        ##
        ## [[2]]
        ## <div id="PowerTable1" class="shiny-html-output"></div>
        ##
        ##<div id="PowerPlot1" class="shiny-plot-output" style="width:100%;height:1000px"></div>
        ##
        ## Convert the list to a tagList - this is necessary for the list of
        ## items to display properly (StackOverflow)
        do.call(tagList, PowerDisplayList)
      })

      ## Now go through the time-labels one at a time.  Although multiproj.power.plot can take
      ## multiple timelabel arguments at once, call it explicitly for each timelabel for ease.
      
      ## For each individual time-label, call renderText to display the project title, renderTable
      ## to display the Table, and renderPlot to display the plot.
      ## Fill each slot in the tagList with the appropriate output.
      for (i in 1:tablength){
        ## Need a local environment so that each item gets its own number.
        ## Without it, the value of i in the renderTable() will be the same
        ## across all instances, because of how the expression is evaluated
        ## (StackOverflow)
        local({
          myi <- i
          ## Define parIn instead of using input$parType - for some unknown
          ## reason seems to make the difference between interface freezing or not
          ## when some combinations are chosen, specifically when changing back
          ## from a power display involving N to one involving lambda:
          parIn <- input$parType
          
          ## Get power-table for this timelabel:
          titlename <- paste("PowerTitle", myi, sep = "")
          tabname <- paste("PowerTable", myi, sep = "")
          plotname <- paste("PowerPlot", myi, sep = "")
          ## Fill in the the output slots with the correct rendering:
          ## Titles:
          if(parIn %in% c("lambda", "N"))
            output[[titlename]] <-
            renderText(paste0("<p><hr><p><p><h2><font color=\"navy\">",
                              parIn, ": all time</font></h2>"))
          else output[[titlename]] <- renderText(paste0("<p><p><hr><p><p>",
                                                        "<h2><font color=\"navy\">", parIn, ": ",
                                                        input$timeLabels[myi],  "</font></h2>"))
          
          ## Tables are returned as list elements; but there's only one element
          ## because we are only feeding it one time-label at a time, so take the
          ## first element of the returned list.
          ## Render the table in the PowerDisplay table output:
          output[[tabname]] <- renderTable({
            tab <- multiproj.power.plot(projects=input$projSelect,
                                        partype=parIn,
                                        queryval=input$queryVal,
                                        timelabels=input$timeLabels[myi],
                                        conf = input$confLevel,
                                        plotit=F,
                                        ## Last two arguments are not incorporated into the UI:
                                        threshold.negvar = 1e-06, impose.01limits = T)[[1]]
            ## Beautify the table names:
            names(tab)[names(tab)=="QueriedValue"] <- "Queried Value"
            names(tab)[names(tab)=="OverallPower"] <- "Overall Power (%)"
            names(tab)[names(tab)=="CIabove"] <- "CI Above (%)"
            names(tab)[names(tab)=="CIbelow"] <- "CI Below (%)"
            tab
          }, include.rownames=F)   ## End renderTable
          
          ## Make another call to multiproj.power.plot to render the plot.
          ## Currently they are not displayed in the order given by "tab", because this would
          ## remove the auto-updating of tab when Conf level or Proj select are changed.
          ## This could be revisited by redesigning the multiproj.power.plot code.
          output[[plotname]] <- renderPlot({
            multiproj.power.plot(projects=input$projSelect,
                                 partype=parIn,
                                 queryval=input$queryVal,
                                 timelabels=input$timeLabels[myi],
                                 conf = input$confLevel,
                                 plotit=T,
                                 gridspace=gridspaceReqd,
                                 n.plotcol = input$plotCol,
                                 cex.main = Cmain, cex.axis = Caxis,
                                 cex.points = Cpoints,
                                 textspace = Tsp,
                                 ## Last two arguments are not incorporated into the UI:
                                 threshold.negvar = 1e-06, impose.01limits = T)
          })   ## End renderPlot
          
          
        })   ## End local
      }  ## End i in 1:tablength
    }) ## End observeEvent
  ## ---------------------------------------------------------------------------------------------------
  ## END SUMMARY TABLES CODE
  ## ---------------------------------------------------------------------------------------------------
}