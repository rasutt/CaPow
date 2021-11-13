### CaPow!

A Shiny web app for capture-recapture model fitting and study design via fast simulation and optimisation using automatic differentiation.

#### General notes

- I had a lot of trouble with TMB not compiling or giving random errors, but it seems to be working again now.

- I started writing code to use jax in python to find the nll and do automatic differentiation.  I made some progress but I'm not sure I got it working.

#### Files and folders

- App folder - The app.
  - example_dataset.csv - A simulated example dataset that can be uploaded into the app.
- things_to_do.R - Old list of things to do.  Should merge with this readme.
- Extra folder
  - python folder
    - popan.ipynb - Code to test functions for nll and gradient in JAX.
    - starting_reticulate.R - Code to setup reticulate and JAX and load python functions for nll and gradient.
    - python folder - Python JAX code for nll and gradient.- Results.dat - Current CaPow.dat file.  I deleted the results from Example project in the one in the app for quickly testing running projects.
  - Saved CaPow objects.dat - Various saved models, simulations, and projects that have useful as examples or experiments.
  - capow.R - Primary code for data simulation and model fitting without using TMB.
  - maui_dolphin_dataset.csv - A real example dataset that can be uploaded into the app.  I think I was also using this to compare the results of my implementation of simulating from existing data with Rachel's.





