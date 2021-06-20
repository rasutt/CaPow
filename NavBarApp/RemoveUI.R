# UI for Remove Items module
RemoveUI <- function(id = "RemoveUI") {
  ns <- NS(id)

  tabPanel(
    title = "Remove Items",
    tags$head(tags$style("#RemoveUI-errormessage{color: red;}")),
    div(
      style = "width:1070px;",
      fluidPage(
        title = "Remove Items",
        sidebarLayout(
          ## Sidebar for inputs
          sidebarPanel(
            h4("What do you want to remove?"),
            radioButtons(
              ns("itemtype"), "",
              choices=list(
                "Project and/or Results" = "projecttype",
                "Simulation"="simtype",
                "Model"="modeltype",
                "Fit"="fittype",
                "Dataset"="datasettype"
              ),
              selected=""
            ),
            br(),
            ## ------------------------------------------------------------------------------------------
            ## Conditional Panel for Remove Project:
            ## ------------------------------------------------------------------------------------------
            conditionalPanel(
              ns = ns,
              condition = "input.itemtype == 'projecttype'",
              h4("Remove Project:"),
              uiOutput(ns("projselect")),
              br(),
              radioButtons(ns("resultsonly"), "Remove:",
                           list("Entire project" = "removeproj",
                                "Results only" = "resonly")),
              actionButton(inputId = ns("projremovebutton"), label = "Remove")
            ),
            ## End of conditional panel for Remove Project
            
            ## ------------------------------------------------------------------------------------------
            ## Conditional Panel for Remove Sim:
            ## ------------------------------------------------------------------------------------------
            conditionalPanel(
              ns = ns,
              condition = "input.itemtype == 'simtype'",
              h4("Remove Simulation:"),
              uiOutput(ns("simselect")),
              actionButton(inputId = ns("simremovebutton"), label = "Remove")
            ),
            
            ## ------------------------------------------------------------------------------------------
            ## Conditional Panel for Remove Model:
            ## ------------------------------------------------------------------------------------------
            conditionalPanel(
              ns = ns,
              condition = "input.itemtype == 'modeltype'",
              h4("Remove Model:"),
              uiOutput(ns("modelselect")),
              actionButton(inputId = ns("modelremovebutton"), label = "Remove")
            ),
            
            ## ------------------------------------------------------------------------------------------
            ## Conditional Panel for Remove Fit:
            ## ------------------------------------------------------------------------------------------
            conditionalPanel(
              ns = ns,
              condition = "input.itemtype == 'fittype'",
              h4("Remove Fit:"),
              uiOutput(ns("fitselect")),
              actionButton(inputId = ns("fitremovebutton"), label = "Remove")
            ),

            ## ------------------------------------------------------------------------------------------
            ## Conditional Panel for Remove Dataset:
            ## ------------------------------------------------------------------------------------------
            conditionalPanel(
              ns = ns,
              condition = "input.itemtype == 'datasettype'",
              h4("Remove Dataset:"),
              uiOutput(ns("datasetselect")),
              actionButton(inputId = ns("datasetremovebutton"), label = "Remove")
            )
          ),
          
          ## Show the projname, the simset description, and the model description
          mainPanel(
            h4(textOutput(ns("errormessage"))),
            SummaryUI(id = ns("SummaryUI"))
          )
        )
      )
    )
  )
}