# UI for data upload module
UploadUI <- function(id = "UploadUI", label = "UploadUI") {
  ns <- NS(id)
  
  tabPanel(
    title = "Upload Data",
    tags$head(tags$style("#UploadUI-errormessage{color: red;}")),
    div(
      style = "width:1070px;",
      fluidPage(
        title = "Upload Data",
        sidebarLayout(
          sidebarPanel(
            textInput(ns("dataname"), "Dataset name:", ""),
            textInput(ns("datadescrip"), "Dataset description:", ""),
            fileInput(
              ns("file"), "Choose CSV File (one column/survey)",
              multiple = FALSE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
            ),
            checkboxInput(ns("header"), "File includes column-names", TRUE),
            helpText("If head of file correct confirm upload with button below"),
            actionButton(ns("uploadbutton"), "Upload Dataset")
          ),
          
          mainPanel(
            h4(textOutput(ns("errormessage"))),
            h4("Head of File"),
            tableOutput(ns("head")),
            FitSummaryUI(id = ns("FitSummaryUI"))
          )
        )
      )
    )
  )
}

# UI for view data module
ViewDataUI <- function(id = "ViewDataUI", label = "ViewDataUI") {
  ns <- NS(id)
  
  tabPanel(
    title = "View Data",
    uiOutput(ns("data_heads")),
    FitSummaryUI(id = ns("FitSummaryUI"))
  )   
}

# UI for fit model module
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
            tableOutput(ns("parests")),
            h4("Expected Population Size"),
            tableOutput(ns("popests")),
            h4("Model Comparison"),
            tableOutput(ns("modcomp")),
            
            FitSummaryUI(id = ns("FitSummaryUI"))
          )
        )
      )
    )
  )
}

# UI for view population plots tab
ViewPopPlotsUI <- function(id = "ViewPopPlotsUI", label = "ViewPopPlotsUI") {
  ns <- NS(id)
  
  tabPanel(
    title = "Population Plots",
    uiOutput(ns("Nt_plots")),
    FitSummaryUI(id = ns("FitSummaryUI"))
  )   
}

# UI for parameter estimates module
ParameterEstimatesUI <- function(
  id = "ParameterEstimatesUI", 
  label = "ParameterEstimatesUI"
) {
  ns <- NS(id)
  
  tabPanel(
    title = "Parameter Estimates",
    uiOutput(ns("fit_results")),
    FitSummaryUI(id = ns("FitSummaryUI"))
  )   
}

# UI for population estimates module
PopulationEstimatesUI <- function(
  id = "PopulationEstimatesUI", 
  label = "PopulationEstimatesUI"
) {
  ns <- NS(id)
  
  tabPanel(
    title = "Population Estimates",
    uiOutput(ns("pop_tables")),
    FitSummaryUI(id = ns("FitSummaryUI"))
  )   
}

# UI for model comparison module
ModelCompUI <- function(id = "ModelCompUI", label = "ModelCompUI") {
  ns <- NS(id)
  
  tabPanel(
    title = "Model Comparison",
    uiOutput(ns("model_comp")),
    FitSummaryUI(id = ns("FitSummaryUI"))
  )   
}

# UI for fit builder tab
FitBuilderUI <- function(id = "FitBuilderUI") {
  tabPanel(
    title = "Fit Builder",
    tabsetPanel(
      UploadUI(),
      ViewDataUI(),
      FitModelUI(),
      ViewPopPlotsUI(),
      ParameterEstimatesUI(),
      PopulationEstimatesUI(),
      ModelCompUI()
    )
  )
}