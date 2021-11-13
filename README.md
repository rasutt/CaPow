### CaPow!

A Shiny web app for capture-recapture model fitting and study design via fast simulation and optimisation using automatic differentiation.

#### General notes

- I had a lot of trouble with TMB not compiling or giving random errors, but it seems to be working again now.

- I started writing code to use jax in python to find the nll and do automatic differentiation.  I made some progress but I'm not sure I got it working.

#### Files and folders

- App folder - The app.
  - CaPow.dat - Saved capow objects loaded when the app is started as examples.  I deleted the results from Example project to quickly test running projects.
  - example_dataset.csv - A simulated example dataset that can be uploaded into the app.
  - popan.cpp - NLL function in C++ template for automatic differentiation with TMB.
  - popan.o and popan.dll - Files created by compilation of popan.cpp by TMB.
  - capow_tmb.R - Primary code for data simulation and model fitting using TMB.
- things_to_do.R - Old list of things to do.  Should merge with this readme.
- Extra folder
  - python folder
    - popan.ipynb - Code to test functions for nll and gradient in JAX.
    - starting_reticulate.R - Code to setup reticulate and JAX and load python functions for nll and gradient.
    - popan.py - Python JAX code for nll and gradient.
    - __pycache__ - Bytecode cache files that are automatically generated by python.
  - Results.dat - Saved capow objects loaded when the app is started as examples, including results for Example project.
  - Saved CaPow objects.dat - Various saved models, simulations, and projects that have been useful as examples or experiments.
  - capow.R - Primary code for data simulation and model fitting without using TMB.
  - maui_dolphin_dataset.csv - A real example dataset that can be uploaded into the app.  I think I was also using this to compare the results of my implementation of simulating from existing data with Rachel's.





