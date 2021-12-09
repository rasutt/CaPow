# UI for CaPow web app

# Load packages
library(shiny)
library(naturalsort)
library(TMB)

# Load functions to support UIs
source("Code/capow_ui_funcs.R")

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