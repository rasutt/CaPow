FitModelUI <- function(id = "FitModelUI", label = "FitModelUI") {
  ns <- NS(id)
  
  tabPanel(
    title = "Fit Model",
    tags$head(tags$style("#FitUI-errormessage{color: red;}")),
    div(
      style = "width:1070px;",
      fluidPage(
        title = "Fit Model",
        sidebarLayout(
          sidebarPanel(
            textInput(ns("fitname"), "Fit name:", ""),
            textInput(ns("fitdescrip"), "Fit description:", ""),
            uiOutput(ns("datasetselection")),
            uiOutput(ns("modelselection")),
            actionButton(ns("fitbutton"), "Fit Model")
          ),
          
          mainPanel(
            h4(textOutput(ns("errormessage"))),
            
            h4("Population Plot"),
            plotOutput(ns("Nt_plot")),
            
            h4("Parameter Estimates"),
            tableOutput(ns("fitresults1")),
            
            h4("Expected Population Size"),
            tableOutput(ns("fitresults2")),
            
            h4("Model Comparison"),
            tableOutput(ns("fitresults3")),
            
            FitSummaryUI(id = ns("FitSummaryUI"))
          )
        )
      )
    )
  )
}