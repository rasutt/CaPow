# UI for Plot module
PlotUI <- function(id = "PlotUI") {
  ns <- NS(id)
  
  tabPanel(
    title = "Plots and Summaries",
    div(
      style = "width:1100px;",
      fluidPage(
        title = "Plots and Summaries",
        sidebarLayout(
          sidebarPanel(
            uiOutput(ns("projselectbox")),
            br(),
            checkboxGroupInput(ns("projPlotwhat"), "Plot Parameters:",
                               choices = c("N", "lambda", "phi", "p", "pent"),
                               selected=c("N", "lambda", "phi", "p", "pent")),
            br(),
            numericInput(ns("projConf"), "Confidence level:", value=0.95,
                         min=0, max=1, step=0.01),
            br(),
            checkboxInput(ns("collapseSingle"),
                          "Collapse multiple parameters to a single plot where possible",
                          value=TRUE),
            width=2  ## width of the sidebarPanel (default = 4)
          ),  ## End of sidebarPanel for global selections
          
          ## -------------------------------------------------------------------------------
          ## MAIN PANEL WITH SUBTABS STARTS HERE
          ## -------------------------------------------------------------------------------
          mainPanel(
            tabsetPanel(
              ## ---------------------------------------------------
              ## BOXPLOTS TAB
              ## ---------------------------------------------------
              tabPanel(
                "Boxplots",
                ## Boxplot settings at top:
                fluidPage(fluidRow(
                  column(4,
                         HTML("<h4><font color=\'#CC0033\'>Boxplot Settings</font></h4>")),
                  column(4, checkboxInput("showBoxplotSettings",
                                          "Show settings", value=FALSE)),
                  column(4,
                         actionButton(ns("boxplotSubmit"), label = "Draw Boxplots",
                                      style="color:green",#CC0033;",
                                      styleclass = "success"))
                ),
                ## HTML("<hr>"),
                conditionalPanel(
                  condition="input.showBoxplotSettings==true",
                  ## ------------------------------------------------------------
                  ## Row for Time axis, CItext, Limits,
                  ## and second row for plotting columns,
                  ## plot height, and text to plot below boxplots:
                  ## ------------------------------------------------------------
                  fluidRow(
                    column(4,
                           checkboxInput(ns("boxplotTimeAxis"),
                                         "Draw time axis",
                                         value=TRUE),
                           numericInput(ns("boxplotPlotcol"),
                                        "Plotting columns:",
                                        value=3, min=1, max=5, step=1)),
                    column(4,
                           checkboxInput(ns("boxplotPlotLimits"),
                                         "Draw limit lines", value=TRUE),
                           numericInput(ns("boxplotHeight"),
                                        "Plot height (pixels):",
                                        value=1000, min=0, max=2000,
                                        step=50)),
                    # boxplotCItext needed in server so pass namespace to conditionalpanel.
                    column(4,
                           checkboxInput(ns("boxplotCItext"),
                                         "Plot summary text on boxplots",
                                         value=TRUE),
                           conditionalPanel(
                             ns = ns,
                             condition=
                               "input.boxplotCItext == true",
                             selectInput(ns("boxplotBLText"),
                                         "Select text to go below boxes:",
                                         choices=list(
                                           "Median CI width"=
                                             "medCIwidth",
                                           "Mean estimated %CV"=
                                             "meanEstCV",
                                           "Empirical %CV"=
                                             "empiricalCV",
                                           "Mean SE"="meanSE",
                                           "Empirical SD"=
                                             "empiricalSD",
                                           "%Relative Bias"=
                                             "relativeBias",
                                           "Root Mean Square Error"=
                                             "rootMeanSqError",
                                           "Mean estimate"=
                                             "meanEst",
                                           "None"=""),
                                         selected="medCIwidth")
                           )
                    )),  ## End row
                  HTML("<hr>"),
                  ## ------------------------------------------------------------
                  ## Row for boxplot gridlines
                  ## ------------------------------------------------------------
                  fluidRow(
                    column(4,
                           HTML("<strong>Gridline spacing:</strong>"),
                           helpText("Leave the gridline boxes blank for no gridlines")),
                    column(4,
                           matLay(matrix(list(
                             HTML("N"),
                             extInput(id = ns("boxplotGridN"),
                                      type = "text",
                                      style = "width:3em",
                                      value = "200"),
                             HTML("lambda"),
                             extInput(id = ns("boxplotGridlambda"), type = "text",
                                      style = "width:3em",
                                      value = "0.05"),
                             HTML("phi"),
                             extInput(id = ns("boxplotGridphi"),
                                      type = "text",
                                      style = "width:3em",
                                      value = "0.05"),
                             HTML("p"),
                             extInput(id = ns("boxplotGridp"),
                                      type = "text",
                                      style = "width:3em",
                                      value = "0.1"),
                             HTML("pent"),
                             extInput(id = ns("boxplotGridpent"),
                                      type = "text",
                                      style = "width:3em",
                                      value = "0.1")
                           ), ncol = 5))
                    )),  ## End row
                  ## ------------------------------------------------------------
                  ## Row for boxplot text size and clear space:
                  ## ------------------------------------------------------------
                  HTML("<hr>"),
                  fluidRow(
                    column(4,
                           HTML("<strong>Text size:</strong>")),
                    column(4,
                           matLay(matrix(list(
                             HTML("Title"),
                             extInput(id = ns("boxplotCexMain"),
                                      type = "text",
                                      style = "width:3em",
                                      value = "1.8"),
                             HTML("Text"),
                             extInput(id = ns("boxplotCexText"),
                                      type = "text",
                                      style = "width:3em",
                                      value = "1.2"),
                             HTML("Axis"),
                             extInput(id = ns("boxplotCexAxis"),
                                      type = "text",
                                      style = "width:3em",
                                      value = "1.5"),
                             HTML("Limits"),
                             extInput(id = ns("boxplotCexLimit"),
                                      type = "text",
                                      style = "width:3em",
                                      value = "1.0")
                           ), ncol = 4))
                    ),
                    column(4,
                           ## Clear space header / footer:
                           matLay(matrix(list(
                             HTML("<strong>Clear Space:</strong>"),
                             HTML(""),
                             HTML("Header"),
                             extInput(id=ns("boxplotHead"),
                                      type = "text",
                                      style = "width:3em",
                                      value = "0.6"),
                             HTML("Footer"),
                             extInput(id=ns("boxplotFoot"),
                                      type = "text",
                                      style = "width:3em",
                                      value = "0.6")
                           ), ncol = 3))
                    )),
                  HTML("<hr>")
                ) ## End row
                ),  ## End conditional panel for boxplot settings
                ## ------------------------------------------------------------------------------------------
                ## PLOT BOXPLOTS
                ## ------------------------------------------------------------------------------------------
                uiOutput(ns("boxplot"))
                
              ),
              ## --------------------------------------------------------------------------------------------------
              ## END BOXPLOT TAB
              ## --------------------------------------------------------------------------------------------------
              
              
              ## --------------------------------------------------------------------------------------------------
              ## MEAN PLOT TAB
              ## --------------------------------------------------------------------------------------------------
              tabPanel("Mean Plots",
                       
                       ## Mean-Plot settings at top:
                       fluidPage(fluidRow(
                         column(4,
                                HTML("<h4><font color=\'#CC0033\'>Mean-Plot Settings</font></h4>")),
                         column(4, checkboxInput("showMeanplotSettings",
                                                 "Show settings", value=FALSE)),
                         column(4,
                                actionButton(ns("meanplotSubmit"),
                                             label = "Draw Mean-Plots",
                                             style="color:green",#CC0033;",
                                             styleclass = "success"))
                       ),
                       ## HTML("<hr>"),
                       conditionalPanel(condition="input.showMeanplotSettings==true",
                                        ## ------------------------------------------------------------
                                        ## Row for mean-plot Zoom margin, plot height,
                                        ## and whether to plot the upper level for fixed
                                        ## parameters:
                                        ## ------------------------------------------------------------
                                        HTML("<p><p>"),
                                        fluidRow(
                                          column(4,
                                                 numericInput(ns("meanplotAutorange"),
                                                              "Zoom margin (%):",
                                                              value=10, min=1, max=100,
                                                              step=1)),
                                          column(4,
                                                 numericInput(ns("meanplotHeight"),
                                                              "Plot height (pixels):",
                                                              value=600, min=0, max=2000,
                                                              step=50)),
                                          column(4,
                                                 selectInput(ns("meanplotConstLevel"),
                                                             "Plot level for fixed parameters:",
                                                             choices=list(
                                                               "Only when needed"="auto",
                                                               "Always"="yes"),
                                                             selected="auto")
                                          )),  ## End row
                                        HTML("<hr>"),
                                        ## ------------------------------------------------------------
                                        ## Rows for MeanPlot gridlines, View windows
                                        ## ------------------------------------------------------------
                                        fluidRow(
                                          column(4,
                                                 HTML("<strong>Gridline spacing:</strong>"),
                                                 helpText("Leave the gridline boxes blank for no gridlines"),
                                                 HTML("<strong>Axis limits:</strong>"),
                                                 helpText("Leave blank for automatic zoom, or enter limits such as 0,1")
                                          ),
                                          column(4,
                                                 matLay(matrix(list(
                                                   HTML("N"),
                                                   extInput(id = ns("mpGridN"),
                                                            type = "text",
                                                            style = "width:3em",
                                                            value = "200"),
                                                   HTML("lambda"),
                                                   extInput(id = ns("mpGridlambda"),
                                                            type = "text",
                                                            style = "width:3em",
                                                            value = "0.05"),
                                                   HTML("phi"),
                                                   extInput(id = ns("mpGridphi"),
                                                            type = "text",
                                                            style = "width:3em",
                                                            value = "0.05"),
                                                   HTML("p"),
                                                   extInput(id = ns("mpGridp"),
                                                            type = "text",
                                                            style = "width:3em",
                                                            value = "0.1"),
                                                   HTML("pent"),
                                                   extInput(id = ns("mpGridpent"),
                                                            type = "text",
                                                            style = "width:3em",
                                                            value = "0.1")
                                                 ), ncol = 5)),
                                                 
                                                 ## Axis-limits (view windows) for MeanPlots:
                                                 matLay(matrix(list(
                                                   HTML("N"),
                                                   extInput(id = ns("mpViewN"),
                                                            type = "text",
                                                            style = "width:3em",
                                                            value = ""),
                                                   HTML("lambda"),
                                                   extInput(id=ns("mpViewlambda"),
                                                            type = "text",
                                                            style = "width:3em",
                                                            value = ""),
                                                   HTML("phi"),
                                                   extInput(id = ns("mpViewphi"),
                                                            type = "text",
                                                            style = "width:3em",
                                                            value = ""),
                                                   HTML("p"),
                                                   extInput(id = ns("mpViewp"),
                                                            type = "text",
                                                            style = "width:3em",
                                                            value = ""),
                                                   HTML("pent"),
                                                   extInput(id = ns("mpViewpent"),
                                                            type = "text",
                                                            style = "width:3em",
                                                            value = "")
                                                 ), ncol = 5))
                                          )),  ## End row
                                        ## ------------------------------------------------------------
                                        ## Row for mean-plot text size
                                        ## ------------------------------------------------------------
                                        HTML("<hr>"),
                                        fluidRow(
                                          column(4,
                                                 HTML("<strong>Text size:</strong>")),
                                          column(4,
                                                 matLay(matrix(list(
                                                   HTML("Title"),
                                                   extInput(id=ns("mpCexMain"),
                                                            type = "text",
                                                            style = "width:3em",
                                                            value = "1.8"),
                                                   HTML("Text"),
                                                   extInput(id=ns("mpCexText"),
                                                            type = "text",
                                                            style = "width:3em",
                                                            value = "1.8"),
                                                   HTML("Axis"),
                                                   extInput(id=ns("mpCexAxis"),
                                                            type = "text",
                                                            style = "width:3em",
                                                            value = "1.5"),
                                                   HTML("Points"),
                                                   extInput(id=ns("mpCexPoints"),
                                                            type = "text",
                                                            style = "width:3em",
                                                            value = "2.0")
                                                 ), ncol = 4))
                                          )),
                                        HTML("<hr>")
                       ) ## End row
                       ),  ## End conditional panel for mean-plot settings
                       ## ------------------------------------------------------------------------------------------
                       ## PLOT MEAN-PLOTS
                       ## ------------------------------------------------------------------------------------------
                       uiOutput(ns("meanplot"))
                       
              ),
              
              ## --------------------------------------------------------------------------------------------------
              ## END MEAN PLOT TAB
              ## --------------------------------------------------------------------------------------------------
              
              ## --------------------------------------------------------------------------------------------------
              ## CI PLOT TAB
              ## --------------------------------------------------------------------------------------------------
              tabPanel("CI Plots",
                       ## CIplot settings at top:
                       fluidPage(fluidRow(
                         column(4,
                                HTML("<h4><font color=\'#CC0033\'>Confidence Interval Settings</font></h4>")),
                         column(4, checkboxInput("showCIplotSettings",
                                                 "Show settings", value=FALSE)),
                         column(4,
                                actionButton(ns("CIplotSubmit"), label = "Draw CI-Plots",
                                             style="color:green",#CC0033;",
                                             styleclass = "success"))
                       ),
                       ## HTML("<hr>"),
                       conditionalPanel(condition="input.showCIplotSettings==true",
                                        ## ------------------------------------------------------------
                                        ## Row for CI plotting columns and plot height
                                        ## ------------------------------------------------------------
                                        fluidRow(
                                          column(4,
                                                 numericInput(ns("CIplotPlotcol"),
                                                              "Plotting columns:",
                                                              value=3, min=1, max=5, step=1)),
                                          column(4,
                                                 numericInput(ns("CIplotHeight"),
                                                              "Plot height (pixels):",
                                                              value=1000, min=0, max=2000,
                                                              step=50))
                                        ),  ## End row
                                        HTML("<hr>"),
                                        ## ------------------------------------------------------------
                                        ## Row for CIplot gridlines
                                        ## ------------------------------------------------------------
                                        fluidRow(
                                          column(4,
                                                 HTML("<strong>Gridline spacing:</strong>"),
                                                 helpText("Leave the gridline boxes blank for no gridlines")),
                                          column(4,
                                                 matLay(matrix(list(
                                                   HTML("N"),
                                                   extInput(id = ns("CIplotGridN"),
                                                            type = "text",
                                                            style = "width:3em",
                                                            value = "200"),
                                                   HTML("lambda"),
                                                   extInput(id = ns("CIplotGridlambda"), type = "text",
                                                            style = "width:3em",
                                                            value = "0.05"),
                                                   HTML("phi"),
                                                   extInput(id = ns("CIplotGridphi"),
                                                            type = "text",
                                                            style = "width:3em",
                                                            value = "0.05"),
                                                   HTML("p"),
                                                   extInput(id = ns("CIplotGridp"),
                                                            type = "text",
                                                            style = "width:3em",
                                                            value = "0.1"),
                                                   HTML("pent"),
                                                   extInput(id = ns("CIplotGridpent"),
                                                            type = "text",
                                                            style = "width:3em",
                                                            value = "0.1")
                                                 ), ncol = 5))
                                          )),  ## End row
                                        ## ------------------------------------------------------------
                                        ## Row for CI plot text size and clear space
                                        ## ------------------------------------------------------------
                                        HTML("<hr>"),
                                        fluidRow(
                                          column(4,
                                                 HTML("<strong>Text size:</strong>")),
                                          column(4,
                                                 matLay(matrix(list(
                                                   HTML("Title"),
                                                   extInput(id = ns("CIplotCexMain"),
                                                            type = "text",
                                                            style = "width:3em",
                                                            value = "1.8"),
                                                   HTML("Axis"),
                                                   extInput(id = ns("CIplotCexAxis"),
                                                            type = "text",
                                                            style = "width:3em",
                                                            value = "1.5"),
                                                   HTML("Points"),
                                                   extInput(id = ns("CIplotCexPoints"),
                                                            type = "text",
                                                            style = "width:3em",
                                                            value = "1.0")
                                                 ), ncol = 3))
                                          ),
                                          column(4,
                                                 matLay(matrix(list(
                                                   HTML("<strong>Clear Space:</strong>"),
                                                   HTML(""),
                                                   HTML("Header"),
                                                   extInput(id=ns("CIplotHead"),
                                                            type = "text",
                                                            style = "width:3em",
                                                            value = "0.3"),
                                                   HTML("Footer"),
                                                   extInput(id=ns("CIplotFoot"),
                                                            type = "text",
                                                            style = "width:3em",
                                                            value = "0.3")
                                                 ), ncol = 3))
                                          )),
                                        HTML("<hr>")
                       ) ## End row
                       ),  ## End conditional panel for CIplot settings
                       ## ------------------------------------------------------------------------------------------
                       ## PLOT CI PLOTS
                       ## ------------------------------------------------------------------------------------------
                       uiOutput(ns("CIplot"))
                       
              ),
              ## --------------------------------------------------------------------------------------------------
              ## END CIPLOT TAB
              ## --------------------------------------------------------------------------------------------------
              
              
              ## --------------------------------------------------------------------------------------------------
              ## SUMMARY TABLES TAB
              ## --------------------------------------------------------------------------------------------------
              tabPanel("Summary Tables",
                       checkboxInput("showDefinitions", "Show definitions", value=FALSE),
                       conditionalPanel(condition="input.showDefinitions==true",
                                        matLay(matrix(list(
                                          HTML("<font color=\"#CC0033\">TrueValue: <font color=\"grey\">generating value for simulations</font>"),
                                          HTML("<font color=\"#CC0033\">meanEst: <font color=\"grey\">mean of estimated values</font>"),
                                          HTML("<font color=\"#CC0033\">relativeBias: <font color=\"grey\">(meanEst - TrueValue) / TrueValue</font>"),
                                          HTML("<font color=\"#CC0033\">empiricalSD: <font color=\"grey\">standard deviation of estimated values</font>"),
                                          HTML("<font color=\"#CC0033\">empiricalCV: <font color=\"grey\">coefficient of variation (CV) of estimated values = empiricalSD / meanEst</font>"),
                                          HTML("<font color=\"#CC0033\">RMSE: <font color=\"grey\">root mean squared error = sqrt[ mean{ &nbsp; (Estimates - TrueValue)<sup>2 &nbsp;</sup> } ]</font>"),
                                          HTML("<font color=\"#CC0033\">meanSE: <font color=\"grey\">mean of estimated standard errors = mean { sqrt( Variance Estimates ) }</font>"),
                                          HTML("<font color=\"#CC0033\">meanEstCV: <font color=\"grey\">mean of estimated CVs = mean { sqrt( Variance Estimates ) / Parameter Estimates  }</font>"),
                                          HTML("<font color=\"#CC0033\">medCIwidth: <font color=\"grey\">median width of confidence intervals</font>"),
                                          HTML("<font color=\"#CC0033\">CIcover: <font color=\"grey\">percentage of simulations for which the confidence interval encloses TrueValue</font>")
                                        ), ncol = 1))
                       ),
                       ## -------------------------------------------------------------------
                       ## DISPLAY SUMMARY TABLES:
                       ## -------------------------------------------------------------------
                       uiOutput(ns("SummaryTables"))
              )#,
              
              ## --------------------------------------------------------------------------------------------------
              ## END SUMMARY TABLES TAB
              ## --------------------------------------------------------------------------------------------------
              
              
              ## --------------------------------------------------------------------------------------------------
              ## REFERENCE LISTS: projects, sim and model
              ## --------------------------------------------------------------------------------------------------
              ## Project list: taken directly from the RunUI interface:
              # tabPanel("Project List",
              #          HTML("This tab displays a reference list of Projects already created. It is not affected by any changes made in this session."),
              #          h3("Project List"),
              #          tableOutput(ns("projlist")),
              #          br(),
              #          h4("Simulation List"),
              #          tableOutput(ns("simlist")),
              #          h4("Model List"),
              #          tableOutput(ns("modellist"))
              # ),
              # 
              # tabPanel("Sim List",
              #          HTML("This tab displays a reference list of SimSets already created. It is not affected by any changes made in this session."),
              #          DisplSim.func(names(get("SimList", envir=CPenv)))
              # ),
              # 
              # tabPanel("Model List",
              #          HTML("This tab displays a reference list of Models already created. It is not affected by any changes made in this session."),
              #          DisplModel.func(names(get("ModelList", envir=CPenv)))
              # )
              ## ---------------------------------------------------------------------------------------------------
              
            ) ## End of tabset on main panel
          )))))  ## End of ShinyUI and div display
}