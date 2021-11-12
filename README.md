# CaPow

A Shiny web app for capture-recapture model fitting and study design via fast simulation and automatic differentiation in R using TMB.

## General notes

I started writing code to use jax in python to find the nll and do automatic differentiation.  I made some progress but I'm not sure I got it working.  Now TMB is working perfectly so I'm going back to it.

I had a lot of trouble with TMB not compiling or giving random errors.  I'm still not sure what the problem was.  After reinstalling windows it worked I think.  Just gonna have to get really expert with it.  Got time now.  And it's really cool stuff.

## Files and folders

### R and TMB Code

- Results.dat - Current CaPow.dat file.  I deleted the results from Example project in the one in the app for quickly testing running projects.
- Saved CaPow objects.dat - Various saved models, simulations, and projects that have useful as examples or experiments.
- capow.R - Primary code for data simulation and model fitting without using TMB.
- CaPow_with_TMB.Rmd and .html - Code testing model fitting with and without TMB when I first implemented it in TMB.  Also explored using the Hessian to diagnose parameter redundancy.
- example_dataset.csv - A simulated example dataset that can be uploaded into the app.
- maui_dolphin_dataset.csv - A real example dataset that can be uploaded into the app.  I think I was also using this to compare the results of my implementation of simulating from existing data with Rachel's.
- things_to_do.R - Old list of things to do.
- App folder - The app.
- rsconnect folder - Created automatically when app deployed to shinyapps.io.  Probably in the wrong place now I moved the app to its own folder.

### Python JAX Code

- popan.ipynb - Code to test functions for nll and gradient in JAX.
- starting_reticulate.R - Code to setup reticulate and JAX and load python functions for nll and gradient.
- python folder - Python JAX code for nll and gradient.

