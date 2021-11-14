# UI for combined CaPow app

# NOTE - Restart R when running a test version of the app from a different
# directory for the first time.  Avoids TMB giving crazy errors, and failing
# to produce MLEs: 
# Error in f(x, order = 2) : Atomic 'D_lgamma' order not implemented.

# Initialize app environment.  This is now done in server for each session, but
# it should come back when I have time to disentangle CPenv from everything
# else.
# source("capow_tmb.R")

# Source functions to support UIs
source("Code/CaPowGUIFuncs.R")

# Source module UIs
files.sources = list.files("Code/UIs")
sapply(paste0("Code/UIs/", files.sources), source)

# Define combined UI as navbarPage with module UI's for tabs
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