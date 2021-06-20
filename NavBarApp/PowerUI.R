# UI for Power module
PowerUI <- function(id = "PowerUI") {
  ns <- NS(id)
  
  tabPanel(
    title = "Power Analysis",
    div(
      style = "width:1100px;",
      tabsetPanel(
        ## --------------------------------------------------------------------------------------------------
        ## SUMMARY TABLES TAB
        ## --------------------------------------------------------------------------------------------------
        tabPanel(
          title = "Power Summaries",
          fluidPage(
            title = "Power Analysis",
            sidebarLayout(
              sidebarPanel(
                # Changed to be not global to match Project and Run modules
                ## ---------------------------------------------------
                ## GLOBAL SELECTIONS SIDEBAR:
                ## ---------------------------------------------------
                selectInput(ns("parType"),
                            "Parameter:",
                            choices=list(
                              "lambda"="lambda", "phi"="phi", "N"="N", "pent"="pent", "p"="p"),
                            selected="lambda"),
                br(),
                numericInput(ns("queryVal"), "Queried value:", value=1.00, min=0, step=0.1),
                br(),
                numericInput(ns("confLevel"), "Confidence level:", value=0.95,
                             min=0, max=1, step=0.01),
                br(),
                # checkboxGroupInput(ns("projSelect"), "Include Projects:",
                #                    choices = projects.run,
                #                    selected = projects.run[1]),
                uiOutput(ns("projSelectBox")),
                br(),
                width=2  ## width of the sidebarPanel (default = 4)
              ),  ## End of sidebarPanel for global selections
              
              ## -------------------------------------------------------------------------------
              ## MAIN PANEL WITH SUBTABS STARTS HERE
              ## -------------------------------------------------------------------------------
              mainPanel(
                fluidPage(
                  fluidRow(
                    column(8, uiOutput(ns("Settings"))),
                    column(4,
                           actionButton(ns("powerSubmit"), label = "Power Display",
                                        style = "color:green", #CC0033;",
                                        styleclass = "success"))
                  ),
                  
                  fluidRow(
                    matLay(matrix(list(
                      HTML("<font color=\"#CC0033\">Overall Power (%): <font color=\"grey\">percentage of simulations in which the queried value is outside the confidence interval. Specifies power to detect the true parameter differs from the queried value.</font>"),
                      HTML("<font color=\"#CC0033\">CI Above (%): <font color=\"grey\">the CI lies wholly above the queried value. Specifies power to detect the true parameter is greater than the queried value.</font>"),
                      HTML("<font color=\"#CC0033\">CI Below (%): <font color=\"grey\">the CI lies wholly below the queried value. Specifies power to detect the true parameter is less than the queried value.</font>")
                    ), ncol = 1))
                  ),
                  
                  ## -------------------------------------------------------------------
                  ## DISPLAY POWER TABLES:
                  ## -------------------------------------------------------------------
                  uiOutput(ns("PowerDisplay"))
                )
              )
            )
          )
        ),
        
        ## --------------------------------------------------------------------------------------------------
        ## END POWER TABLES TAB
        ## --------------------------------------------------------------------------------------------------
        
        ## --------------------------------------------------------------------------------------------------
        ## POWER PLOT TAB
        ## --------------------------------------------------------------------------------------------------
        tabPanel(
          "Plot settings",
          ## Powerplot settings at top:
          fluidPage(
            fluidRow(
              column(4,
                     HTML("<h4><font color=\'#CC0033\'>Power Plot Settings</font></h4>"))
            ),
            ## HTML("<hr>"),
            ## ------------------------------------------------------------
            ## Row for plotting columns and plot height
            ## ------------------------------------------------------------
            fluidRow(
              column(4,
                     numericInput(ns("plotCol"),
                                  "Plotting columns:",
                                  value=2, min=1, max=5, step=1)),
              column(4,
                     numericInput(ns("plotHeight"),
                                  "Plot height (pixels):",
                                  value=500, min=0, max=2000,
                                  step=50))
            ),  ## End row
            HTML("<hr>"),
            ## ------------------------------------------------------------
            ## Row for gridlines
            ## ------------------------------------------------------------
            fluidRow(
              column(4,
                     HTML("<strong>Gridline spacing:</strong>"),
                     helpText("Leave the gridline boxes blank for no gridlines")),
              column(4,
                     matLay(matrix(list(
                       HTML("N"),
                       extInput(id = ns("GridN"),
                                type = "text",
                                style = "width:3em",
                                value = "200"),
                       HTML("lambda"),
                       extInput(id = ns("Gridlambda"),
                                type = "text",
                                style = "width:3em",
                                value = "0.05"),
                       HTML("phi"),
                       extInput(id = ns("Gridphi"),
                                type = "text",
                                style = "width:3em",
                                value = "0.05"),
                       HTML("p"),
                       extInput(id = ns("Gridp"),
                                type = "text",
                                style = "width:3em",
                                value = "0.1"),
                       HTML("pent"),
                       extInput(id = ns("Gridpent"),
                                type = "text",
                                style = "width:3em",
                                value = "0.1")
                     ), ncol = 5))
              )),  ## End row
            ## ------------------------------------------------------------
            ## Row for text size and clear space
            ## ------------------------------------------------------------
            HTML("<hr>"),
            fluidRow(
              column(4,
                     HTML("<strong>Text size:</strong>")),
              column(4,
                     matLay(matrix(list(
                       HTML("Title"),
                       extInput(id = ns("CexMain"),
                                type = "text",
                                style = "width:3em",
                                value = "1.8"),
                       HTML("Axis"),
                       extInput(id = ns("CexAxis"),
                                type = "text",
                                style = "width:3em",
                                value = "1.5"),
                       HTML("Points"),
                       extInput(id = ns("CexPoints"),
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
                       extInput(id=ns("plotHead"),
                                type = "text",
                                style = "width:3em",
                                value = "0.3"),
                       HTML("Footer"),
                       extInput(id=ns("plotFoot"),
                                type = "text",
                                style = "width:3em",
                                value = "0.3")
                     ), ncol = 3))
              )
            ),
            HTML("<hr>")
          )
        )
      )
    )
  )
}