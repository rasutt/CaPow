# UI for combined CaPow app

# NOTE - Restart R when running a test version of the app from a different
# directory for the first time, to avoid TMB giving crazy errors, and failing
# to produce MLEs: 
# Error in f(x, order = 2) : Atomic 'D_lgamma' order not implemented.

# Initialize app environment.  This is now done in server for each session, but
# it should come back when I have time to disentangle CPenv from everything
# else.
# source("capow_tmb.R")

# Source functions to support UIs
source("CaPowGUIFuncs.R")

# Source module UIs
source("NavBarApp/WelcomeUI.R")
source("NavBarApp/ModelUI.R")
source("NavBarApp/FitSummaryUI.R")
source("NavBarApp/UploadUI.R")
source("NavBarApp/FitModelUI.R")
source("NavBarApp/ViewDataUI.R")
source("NavBarApp/ParameterEstimatesUI.R")
source("NavBarApp/ModelCompUI.R")
source("NavBarApp/PopulationEstimatesUI.R")
source("NavBarApp/ViewPopPlotsUI.R")
source("NavBarApp/FitBuilderUI.R")
source("NavBarApp/SimUI.R")
source("NavBarApp/SummaryUI.R")
source("NavBarApp/ProjectBuilderUI.R")
source("NavBarApp/RunUI.R")
source("NavBarApp/ProjectsUI.R")
source("NavBarApp/PowerUI.R")
source("NavBarApp/PlotUI.R")
source("NavBarApp/RemoveUI.R")
source("NavBarApp/SaveLoadUI.R")
source("NavBarApp/SaveUI.R")
source("NavBarApp/LoadUI.R")

# Define combined UI as navbarPage with module UI's for tabs
shinyUI(
  navbarPage(
    title = "CaPow!",
    position = "fixed-top",
    collapsible = T,
    WelcomeUI(),
    ModelUI(),
    FitBuilderUI(),
    SimUI(),
    ProjectsUI(),
    PowerUI(),
    PlotUI(),
    RemoveUI(),
    SaveLoadUI(),
    tags$style(type="text/css", "body {padding-top: 70px;}")
  )
)