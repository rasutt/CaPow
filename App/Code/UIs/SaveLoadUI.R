# UI for Save module
SaveUI <- function(id = "SaveUI", label = "SaveUI") {
  ns <- NS(id)
  
  tabPanel(
    title = "Save Session",
    tags$head(tags$style("#SaveUI-errormessage{color: red;}")),
    div(
      style = "width:1070px;",
      fluidPage(
        title = "Save Session",
        sidebarLayout(
          sidebarPanel(
            textInput(ns("filenameinput"), "Enter filename"),
            downloadButton(ns("savesession"), "Save Session")
          ),
          
          mainPanel(
            h4(textOutput(ns("errormessage"))),
            SummaryUI(id = ns("SummaryUI"))
          )
        )
      )
    )
  )
}

# UI for load module
LoadUI <- function(id = "LoadUI", label = "LoadUI") {
  ns <- NS(id)
  
  tabPanel(
    title = "Load Session",
    tags$head(tags$style("#LoadUI-errormessage{color: red;}")),
    div(
      style = "width:1070px;",
      fluidPage(
        title = "Load Session",
        sidebarLayout(
          sidebarPanel(
            fileInput(ns("file"), "Choose dat File", multiple = FALSE),
            actionButton(ns("loadbutton"), "Load Session")
          ),
          
          mainPanel(
            h4(textOutput(ns("errormessage"))),
            SummaryUI(id = ns("SummaryUI"))
          )
        )
      )
    )
  )
}

# UI for Save/Load tab
SaveLoadUI <- function(id = "SaveLoadUI") {
  tabPanel(
    title = "Save/Load",
    tabsetPanel(
      SaveUI(),
      LoadUI()
    )
  )
}