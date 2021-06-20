SaveLoadUI <- function(id = "SaveLoadUI") {
  tabPanel(
    title = "Save/Load",
    tabsetPanel(
      SaveUI(),
      LoadUI()
    )
  )
}