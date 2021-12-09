## Define server logic required to summarize and view the selected dataset
PlotServer <- function(input, output, session, capow_list) {
  # Reactive checkbox selection for project results to analyse.
  projects_run <- function() {
    all.projects <- capow_list()$project_list()
    all.proj.run <- unlist(lapply(all.projects, function(x)x["run"]))
    names(all.projects)[all.proj.run==1]
  }
  output$projselectbox <- renderUI({
    ns <- NS("PlotUI")
    checkboxGroupInput(
      ns("projselect"), "Plot Projects:",
      choices = projects_run()
      )
  })
  
  ## ---------------------------------------------------------------------------------------------------
  ## BOXPLOT CODE:
  ## ---------------------------------------------------------------------------------------------------
  observeEvent(input$boxplotSubmit,
               {
                 ## Coerce text inputs to numeric:
                 bpGphi <- suppressWarnings(as.numeric(input$boxplotGridphi))
                 bpGp <- suppressWarnings(as.numeric(input$boxplotGridp))
                 bpGpent <- suppressWarnings(as.numeric(input$boxplotGridpent))
                 bpGN <- suppressWarnings(as.numeric(input$boxplotGridN))
                 bpGlambda <- suppressWarnings(as.numeric(input$boxplotGridlambda))
                 ##
                 bpCmain <- suppressWarnings(as.numeric(input$boxplotCexMain))
                 bpCaxis <- suppressWarnings(as.numeric(input$boxplotCexAxis))
                 bpCtext <- suppressWarnings(as.numeric(input$boxplotCexText))
                 bpClimit <- suppressWarnings(as.numeric(input$boxplotCexLimit))
                 ##
                 bpTsp <- suppressWarnings(c(as.numeric(input$boxplotFoot),
                                             as.numeric(input$boxplotHead)))
                 
                 ## ------------------------------------------------------------------------------------------
                 ## SET UP FOR MULTIPLE PLOTS: BOXPLOTS
                 ## ------------------------------------------------------------------------------------------
                 ## Set plotlength for the right number of outputs:
                 plotlength <- length(input$projselect)
                 ## Create a list of outputs to be filled with plots:
                 output$boxplot <- renderUI({
                   ## Give each plot a unique name to refer to in the output.
                   ## We need a list of alternating title names and plots:
                   ## the total length of the list needs to be 2*plotlength to give a title and a plot for each project.
                   boxplotOutputList <- vector("list", 2*plotlength)
                   ## Fill in the title slots of boxplotOutputList: slots 1, 3, 5, ...
                   
                   # Create namespace for outputs below
                   ns <- NS("PlotUI")
                   
                   boxplotOutputList[seq(from=1, by=2, length=plotlength)] <- lapply(1:plotlength, function(i) {
                     titlename <- paste("boxplotTitle", i, sep="")
                     htmlOutput(ns(titlename))
                   })
                   ## Fill in the plotted slots of boxplotOutputList: slots 2, 4, 6, ...
                   boxplotOutputList[seq(from=2, by=2, length=plotlength)] <- lapply(1:plotlength, function(i) {
                     plotname <- paste("boxplot", i, sep="")
                     plotOutput(ns(plotname), height=input$boxplotHeight)
                   })
                   ## boxplotOutputList is a list of HTML tags, with list elements like:
                   ## [[1]]
                   ## <div id="boxplotTitle1" class="shiny-html-output"></div>
                   ## [[2]]
                   ## <div id="boxplot1" class="shiny-plot-output" style="width: 100% ; height: 1000px"></div>
                   ##
                   ## Convert the list to a tagList - this is necessary for the list of
                   ## items to display properly (StackOverflow)
                   do.call(tagList, boxplotOutputList)
                 })
                 
                 ## Now go through the plots one at a time.
                 ## For each individual plot, call renderText to display the plot title, and renderPlot
                 ## to display the plot.   Fill each slot in the tagList with the appropriate output.
                 for (i in seq(along = input$projselect)){
                   ## Need a local environment so that each item gets its own number.
                   ## Without it, the value of i in the renderPlot() will be the same
                   ## across all instances, because of how the expression is evaluated
                   ## (StackOverflow)
                   local({
                     myi <- i
                     ## Get the results for this project:
                     resultsProj <- get(paste0("result.", input$projselect[myi]), envir=CPenv)
                     titlename <- paste("boxplotTitle", myi, sep = "")
                     plotname <- paste("boxplot", myi, sep = "")
                     ## Fill in the the output slots with the correct rendering:
                     ## Titles:
                     output[[titlename]] <- renderText(paste0("<p><p><hr><p><p>",
                                                              "<h2><font color=\"navy\">Project ",
                                                              input$projselect[myi],  "</font></h2>"))
                     ## Boxplots:
                     output[[plotname]] <- renderPlot({
                       popan.boxplot(res.raw=resultsProj,
                                     plotwhat =input$projPlotwhat,
                                     conf = input$projConf,
                                     collapse.single = input$collapseSingle,
                                     n.plotcol = input$boxplotPlotcol,
                                     time.axis = input$boxplotTimeAxis,
                                     gridspace.phi = bpGphi, gridspace.p=bpGp, gridspace.pent=bpGpent,
                                     gridspace.N = bpGN, gridspace.lambda = bpGlambda,
                                     plot.limits = input$boxplotPlotLimits,
                                     cex.main = bpCmain, cex.axis = bpCaxis,
                                     cex.text = bpCtext, cex.limit = bpClimit,
                                     citext = input$boxplotCItext,
                                     bottomline.text = input$boxplotBLText,
                                     textspace = bpTsp,
                                     ## Last two arguments are not incorporated into the UI:
                                     threshold.negvar = 1e-06, impose.01limits = T)
                     })   ## End renderPlot
                   })   ## End local
                 }  ## End i in 1:plotlength
               }) ## End observeEvent
  ## ---------------------------------------------------------------------------------------------------
  ## END BOXPLOT CODE
  ## ---------------------------------------------------------------------------------------------------
  
  ## ---------------------------------------------------------------------------------------------------
  ## MEANPLOT CODE:
  ## ---------------------------------------------------------------------------------------------------
  observeEvent(input$meanplotSubmit,
               {
                 ## Coerce text inputs to numeric:
                 ## Grid lines:
                 mpGphi <- suppressWarnings(as.numeric(input$mpGridphi))
                 mpGp <- suppressWarnings(as.numeric(input$mpGridp))
                 mpGpent <- suppressWarnings(as.numeric(input$mpGridpent))
                 mpGN <- suppressWarnings(as.numeric(input$mpGridN))
                 mpGlambda <- suppressWarnings(as.numeric(input$mpGridlambda))
                 ## Text size:
                 mpCmain <- suppressWarnings(as.numeric(input$mpCexMain))
                 mpCaxis <- suppressWarnings(as.numeric(input$mpCexAxis))
                 mpCtext <- suppressWarnings(as.numeric(input$mpCexText))
                 mpCpoints <- suppressWarnings(as.numeric(input$mpCexPoints))
                 ##
                 ## View-Window limits need to be entered as text strings "number, number".
                 ## If this doesn't parse correctly, use auto-view.
                 viewpar.func <- function(mpViewpar){
                   mpVpar <- suppressWarnings(as.numeric(strsplit(mpViewpar, split=",")[[1]]))
                   if(any(is.na(mpVpar))) mpVpar <- "auto"
                   else if(length(mpVpar)!=2) mpVpar <- "auto"
                   return(mpVpar)
                 }
                 mpVphi <- viewpar.func(input$mpViewphi)
                 mpVp <- viewpar.func(input$mpViewp)
                 mpVpent <- viewpar.func(input$mpViewpent)
                 mpVN <- viewpar.func(input$mpViewN)
                 mpVlambda <- viewpar.func(input$mpViewlambda)
                 
                 ## ------------------------------------------------------------------------------------------
                 ## SET UP FOR MULTIPLE PLOTS: MEANPLOTS
                 ## ------------------------------------------------------------------------------------------
                 ## Set plotlength for the right number of outputs:
                 plotlength <- length(input$projselect)
                 ## Create a list of outputs to be filled with plots:
                 output$meanplot <- renderUI({
                   ## Give each plot a unique name to refer to in the output.
                   ## We need a list of alternating title names and plots:
                   ## the total length of the list needs to be 2*plotlength to give a title and a plot for each project.
                   meanplotOutputList <- vector("list", 2*plotlength)
                   ## Fill in the title slots of meanplotOutputList: slots 1, 3, 5, ...
                   
                   # Create namespace for outputs below
                   ns <- NS("PlotUI")
                   
                   meanplotOutputList[seq(from=1, by=2, length=plotlength)] <- lapply(1:plotlength, function(i) {
                     titlename <- paste("meanplotTitle", i, sep="")
                     htmlOutput(ns(titlename))
                   })
                   ## Fill in the plotted slots of meanplotOutputList: slots 2, 4, 6, ...
                   meanplotOutputList[seq(from=2, by=2, length=plotlength)] <- lapply(1:plotlength, function(i) {
                     plotname <- paste("meanplot", i, sep="")
                     plotOutput(ns(plotname), height=input$meanplotHeight)
                   })
                   ## meanplotOutputList is a list of HTML tags, with list elements like:
                   ## [[1]]
                   ## <div id="meanplotTitle1" class="shiny-html-output"></div>
                   ## [[2]]
                   ##<div id="meanplot1" class="shiny-plot-output" style="width:100%; height:1000px"></div>
                   ##
                   ## Convert the list to a tagList - this is necessary for the list of
                   ## items to display properly (StackOverflow)
                   do.call(tagList, meanplotOutputList)
                 })
                 
                 ## Now go through the plots one at a time.
                 ## For each individual plot, call renderText to display the plot title, and renderPlot
                 ## to display the plot.   Fill each slot in the tagList with the appropriate output.
                 for (i in seq(along = input$projselect)){
                   ## Need a local environment so that each item gets its own number.
                   ## Without it, the value of i in the renderPlot() will be the same
                   ## across all instances, because of how the expression is evaluated
                   ## (StackOverflow)
                   local({
                     myi <- i
                     ## Get the results for this project:
                     resultsProj <- get(paste0("result.", input$projselect[myi]), envir=CPenv)
                     titlename <- paste("meanplotTitle", myi, sep = "")
                     plotname <- paste("meanplot", myi, sep = "")
                     ## Fill in the the output slots with the correct rendering:
                     ## Titles:
                     output[[titlename]] <- renderText(paste0("<p><p><hr><p><p>",
                                                              "<h2><font color=\"navy\">Project ",
                                                              input$projselect[myi],  "</font></h2>"))
                     ## Meanplots:
                     output[[plotname]] <- renderPlot({
                       popan.mean.plot(res.raw=resultsProj,
                                       plotwhat =input$projPlotwhat,
                                       view.phi=mpVphi, view.p=mpVp, view.pent=mpVpent,
                                       view.N=mpVN, view.lambda=mpVlambda,
                                       autorange=input$meanplotAutorange,
                                       gridspace.phi = mpGphi, gridspace.p=mpGp, gridspace.pent=mpGpent,
                                       gridspace.N = mpGN, gridspace.lambda = mpGlambda,
                                       include.constlevel=input$meanplotConstLevel,
                                       cex.pt = mpCpoints, cex.main = mpCmain,
                                       cex.text = mpCtext, cex.axis = mpCaxis,
                                       ## Last argument is not incorporated into the UI:
                                       threshold.negvar = 1e-06)
                     })   ## End renderPlot
                   })   ## End local
                 }  ## End i in 1:plotlength
               }) ## End observeEvent
  ## ---------------------------------------------------------------------------------------------------
  ## END MEANPLOT CODE
  ## ---------------------------------------------------------------------------------------------------
  
  ## ---------------------------------------------------------------------------------------------------
  ## CIPLOT CODE:
  ## ---------------------------------------------------------------------------------------------------
  observeEvent(input$CIplotSubmit,
               {
                 ## Coerce text inputs to numeric:
                 ciGphi <- suppressWarnings(as.numeric(input$CIplotGridphi))
                 ciGp <- suppressWarnings(as.numeric(input$CIplotGridp))
                 ciGpent <- suppressWarnings(as.numeric(input$CIplotGridpent))
                 ciGN <- suppressWarnings(as.numeric(input$CIplotGridN))
                 ciGlambda <- suppressWarnings(as.numeric(input$CIplotGridlambda))
                 ##
                 ciCmain <- suppressWarnings(as.numeric(input$CIplotCexMain))
                 ciCaxis <- suppressWarnings(as.numeric(input$CIplotCexAxis))
                 ciCpoints <- suppressWarnings(as.numeric(input$CIplotCexPoints))
                 ##
                 ciTsp <- suppressWarnings(c(as.numeric(input$CIplotFoot),
                                             as.numeric(input$CIplotHead)))
                 
                 ## ------------------------------------------------------------------------------------------
                 ## SET UP FOR MULTIPLE PLOTS: CIPLOTS
                 ## ------------------------------------------------------------------------------------------
                 ## Set plotlength for the right number of outputs:
                 plotlength <- length(input$projselect)
                 ## Create a list of outputs to be filled with plots:
                 output$CIplot <- renderUI({
                   ## Give each plot a unique name to refer to in the output.
                   ## We need a list of alternating title names and plots:
                   ## the total length of the list needs to be 2*plotlength to give a title and a plot for each project.
                   CIplotOutputList <- vector("list", 2*plotlength)
                   ## Fill in the title slots of CIplotOutputList: slots 1, 3, 5, ...
                   
                   # Create namespace for outputs below
                   ns <- NS("PlotUI")
                   
                   CIplotOutputList[seq(from=1, by=2, length=plotlength)] <- lapply(1:plotlength, function(i) {
                     titlename <- paste("CIplotTitle", i, sep="")
                     htmlOutput(ns(titlename))
                   })
                   ## Fill in the plotted slots of CIplotOutputList: slots 2, 4, 6, ...
                   CIplotOutputList[seq(from=2, by=2, length=plotlength)] <- lapply(1:plotlength, function(i) {
                     plotname <- paste("CIplot", i, sep="")
                     plotOutput(ns(plotname), height=input$CIplotHeight)
                   })
                   ## CIplotOutputList is a list of HTML tags, with list elements like:
                   ## [[1]]
                   ## <div id="CIplotTitle1" class="shiny-html-output"></div>
                   ## [[2]]
                   ## <div id="CIplot1" class="shiny-plot-output" style="width: 100% ; height: 1000px"></div>
                   ##
                   ## Convert the list to a tagList - this is necessary for the list of
                   ## items to display properly (StackOverflow)
                   do.call(tagList, CIplotOutputList)
                 })
                 
                 ## Now go through the plots one at a time.
                 ## For each individual plot, call renderText to display the plot title, and renderPlot
                 ## to display the plot.   Fill each slot in the tagList with the appropriate output.
                 for (i in seq(along = input$projselect)){
                   ## Need a local environment so that each item gets its own number.
                   ## Without it, the value of i in the renderPlot() will be the same
                   ## across all instances, because of how the expression is evaluated
                   ## (StackOverflow)
                   local({
                     myi <- i
                     ## Get the results for this project:
                     resultsProj <- get(paste0("result.", input$projselect[myi]), envir=CPenv)
                     titlename <- paste("CIplotTitle", myi, sep = "")
                     plotname <- paste("CIplot", myi, sep = "")
                     ## Fill in the the output slots with the correct rendering:
                     ## Titles:
                     output[[titlename]] <- renderText(paste0("<p><p><hr><p><p>",
                                                              "<h2><font color=\"navy\">Project ",
                                                              input$projselect[myi],  "</font></h2>"))
                     ## CIplots:
                     output[[plotname]] <- renderPlot({
                       popan.ci.plot(res.raw=resultsProj,
                                     plotwhat =input$projPlotwhat,
                                     conf = input$projConf,
                                     collapse.single = input$collapseSingle,
                                     gridspace.phi = ciGphi, gridspace.p=ciGp, gridspace.pent=ciGpent,
                                     gridspace.N = ciGN, gridspace.lambda = ciGlambda,
                                     n.plotcol = input$CIplotPlotcol,
                                     cex.main = ciCmain, cex.axis = ciCaxis,
                                     cex.points = ciCpoints,
                                     textspace = ciTsp,
                                     ## Last two arguments are not incorporated into the UI:
                                     threshold.negvar = 1e-06, impose.01limits = T)
                     })   ## End renderPlot
                   })   ## End local
                 }  ## End i in 1:plotlength
               }) ## End observeEvent
  ## ---------------------------------------------------------------------------------------------------
  ## END CIPLOT CODE
  ## ---------------------------------------------------------------------------------------------------
  
  ## ---------------------------------------------------------------------------------------------------
  ## SUMMARY TABLES CODE
  ## ---------------------------------------------------------------------------------------------------
  ## The line below sets up a dependence on any of the four global inputs involving which
  ## projects and parameters are selected, confidence level and collapse behaviour:
  observeEvent(c(input$projselect, input$projPlotwhat, input$projConf, input$collapseSingle),
               {
                 ## ------------------------------------------------------------------------------------------
                 ## SET UP FOR MULTIPLE PLOTS: SUMMARY TABLES
                 ## ------------------------------------------------------------------------------------------
                 ## Set plotlength for the right number of outputs:
                 tablength <- length(input$projselect)
                 ## Create a list of outputs to be filled with plots:
                 output$SummaryTables <- renderUI({
                   ## Give each plot a unique name to refer to in the output.
                   ## We need a list of alternating title names and plots:
                   ## the total length of the list needs to be 2*tablength to give a title and a plot for each project.
                   SummaryTablesOutputList <- vector("list", 2*tablength)
                   ## Fill in the title slots of SummaryTablesOutputList: slots 1, 3, 5, ...
                   
                   # Create namespace for outputs below
                   ns <- NS("PlotUI")
                   
                   SummaryTablesOutputList[seq(from=1, by=2, length=tablength)] <-
                     lapply(1:tablength, function(i) {
                       titlename <- paste("SummaryTablesTitle", i, sep="")
                       htmlOutput(ns(titlename))
                     })
                   ## Fill in the plotted slots of SummaryTablesOutputList: slots 2, 4, 6, ...
                   SummaryTablesOutputList[seq(from=2, by=2, length=tablength)] <-
                     lapply(1:tablength, function(i) {
                       tabname <- paste("SummaryTables", i, sep="")
                       tableOutput(ns(tabname))
                     })
                   ## SummaryTablesOutputList is a list of HTML tags, with list elements like:
                   ## [[1]]
                   ## <div id="SummaryTablesTitle1" class="shiny-html-output"></div>
                   ##
                   ## [[2]]
                   ## <div id="SummaryTables1" class="shiny-html-output"></div>
                   ##
                   ## Convert the list to a tagList - this is necessary for the list of
                   ## items to display properly (StackOverflow)
                   do.call(tagList, SummaryTablesOutputList)
                 })
                 
                 ## Now go through the tables one at a time.
                 ## For each individual project, call renderText to display the project title, and renderTable
                 ## to display the Table.   Fill each slot in the tagList with the appropriate output.
                 # Changed to seq_along as this seems to happen before projselect is initialised by renderUI above.
                 # Better practice anyway since could always have none selected.
                 for (i in seq_along(input$projselect)){
                   ## Need a local environment so that each item gets its own number.
                   ## Without it, the value of i in the renderTable() will be the same
                   ## across all instances, because of how the expression is evaluated
                   ## (StackOverflow)
                   local({
                     myi <- i
                     ## Get the results for this project:
                     resultsProj <- get(paste0("result.", input$projselect[myi]), envir=CPenv)
                     titlename <- paste("SummaryTablesTitle", myi, sep = "")
                     plotname <- paste("SummaryTables", myi, sep = "")
                     ## Fill in the the output slots with the correct rendering:
                     ## Titles:
                     output[[titlename]] <- renderText(paste0("<p><p><hr><p><p>",
                                                              "<h2><font color=\"navy\">Project ",
                                                              input$projselect[myi],  "</font></h2>"))
                     ## SummaryTables:
                     output[[plotname]] <- renderTable({
                       tab <- popan.summary.func(res.raw=resultsProj,
                                                 plotwhat =input$projPlotwhat,
                                                 conf = input$projConf,
                                                 collapse.single = input$collapseSingle,
                                                 ## Last two arguments are not incorporated into the UI:
                                                 threshold.negvar = 1e-06, impose.01limits = T)
                       tab$CIavail <- NULL
                       tab$details <- NULL
                       tab
                     }, include.rownames=F)   ## End renderTable
                   })   ## End local
                 }  ## End i in 1:tablength
               }) ## End observeEvent
  ## ---------------------------------------------------------------------------------------------------
  ## END SUMMARY TABLES CODE
  ## ---------------------------------------------------------------------------------------------------
  
  ## ---------------------------------------------------------------------------------------------------
  ## PROJECT LIST, MODEL LIST, SIM LIST
  ## ---------------------------------------------------------------------------------------------------
  # output$projlist <- renderTable({
  #   ## Create the list of all projects for display:
  #   allproj <- get("ProjectList", envir=CPenv)
  #   allprojnames <- unlist(lapply(allproj, function(x)x["name"]))
  #   allprojdescrip <- unlist(lapply(allproj, function(x)x["description"]))
  #   allprojsim <- unlist(lapply(allproj, function(x)x["simset"]))
  #   allprojmodel <- unlist(lapply(allproj, function(x)x["model"]))
  #   allprojrun <- unlist(lapply(allproj, function(x)x["run"]))
  #   
  #   ## Next line ensures that the "run" code really corresponds to projects with results existing,
  #   ## so apply it to all projects, not just those that might have changed recently:
  #   for(pr in allprojnames) if(exists(paste("result.", pr, sep=""), envir=CPenv))
  #     allprojrun[allprojnames==pr] <- 1
  #   
  #   ## Get the simsets and models so we can display their descriptions:
  #   allsims <- get("SimList", envir=CPenv)
  #   allmodels <- get("ModelList", envir=CPenv)
  #   
  #   simdescrip <- unlist(lapply(allsims, function(x)x$description))
  #   simname <- unlist(lapply(allsims, function(x)x$simname))
  #   allproj.simdescrip <- simdescrip[match(allprojsim, simname)]
  #   
  #   modeldescrip <- unlist(lapply(allmodels, function(x)x$description))
  #   modelname <- unlist(lapply(allmodels, function(x)x$modelname))
  #   allproj.modeldescrip <- modeldescrip[match(allprojmodel, modelname)]
  #   
  #   ## Create the output display table:
  #   output.table <- data.frame(row.names=NULL,
  #                              "Name"=allprojnames,
  #                              "Description" = allprojdescrip,
  #                              "Run" = allprojrun,
  #                              "Simulation" = allprojsim,
  #                              "Model"= allprojmodel,
  #                              "Sim_Description"=allproj.simdescrip,
  #                              "Model_Description"=allproj.modeldescrip)
  #   
  #   return(output.table)
  # })
  # 
  # ## ---------------------------------------------------------------------------------------------------
  # output$simlist <- renderTable({
  #   allsims <- get("SimList", envir=CPenv)
  #   data.frame(row.names=NULL,
  #              "Name" =unlist(lapply(allsims, function(x)x$simname)),
  #              "Description" =unlist(lapply(allsims, function(x)x$description)))
  # })
  # 
  # ## ---------------------------------------------------------------------------------------------------
  # output$modellist <- renderTable({
  #   allmodels <- get("ModelList", envir=CPenv)
  #   data.frame(row.names=NULL,
  #              "Name" =unlist(lapply(allmodels, function(x)x$modelname)),
  #              "Description" =unlist(lapply(allmodels, function(x)x$description)))
  # })
}