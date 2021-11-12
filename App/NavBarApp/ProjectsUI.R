# UI for Projects module
ProjectsUI <- function(id = "ProjectsUI") {
  ns <- NS(id)
  
  tabPanel(
    title = "Projects",
    tabsetPanel(
      ProjectBuilderUI(),
      RunUI()
    )
  )
}