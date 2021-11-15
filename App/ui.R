# UI for CaPow web app

# Initialize app environment.  This is now done in server for each session, but
# it should come back when I have time to disentangle CPenv from everything
# else.
# source("capow_tmb.R")

# Load functions to support UIs
source("Code/CaPowGUIFuncs.R")

# Load component UIs
files.sources = list.files("Code/UIs")
sapply(paste0("Code/UIs/", files.sources), source)

# Define combined UI as navbarPage with component UIs for tabs
shinyUI(
  navbarPage(
    title = "CaPow!",
    position = "fixed-top",
    header = tags$style(type="text/css", "body {padding-top: 70px;}"),
    collapsible = T,
    WelcomeUI(),
    ModelUI(),
    FitBuilderUI(),
    SimUI(),
    ProjectsUI(),
    PowerUI(),
    PlotUI(),
    RemoveUI(),
    SaveLoadUI()
  )
)