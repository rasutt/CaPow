### CaPow!

A Shiny web app for capture-recapture model fitting and study design via fast simulation and optimization using automatic differentiation.

#### General notes

-   In the first version of CaPow the model fitting often gave negative estimates of variances. After implementing it in TMB I realized that the original code was converging to false minima due to tricks used to enforce parameter bounds manually and avoid what turned out to be legitimate boundary estimates.

-   I have had a lot of trouble with TMB not compiling or giving random errors when I reinstall, but it seems to be working now.

-   I started writing code to use jax in python to find the nll and do automatic differentiation. I made some progress but I'm not sure I got it working.

#### Files and folders

-   things_to_do.R - Old list of things to do. Should merge with this readme.

-   App folder - The app.

    -   ui.R - Code specifying GUI for app.

    -   CaPowGUIFuncs.R - Functions creating components for GUI.

    -   server.R - Code to run app according to specified GUI.

    -   capow_tmb.R - Primary code for data simulation and model fitting using TMB.

    -   popan.cpp - NLL function in C++ template for automatic differentiation with TMB.

    -   popan.o and popan.dll - Files created by compilation of popan.cpp by TMB.

    -   CaPow.dat - Saved capow objects loaded when the app is started as examples. I deleted the results from Example project to quickly test running projects.

    -   example_dataset.csv - A simulated example dataset that can be uploaded into the app.

    -   www folder - Presentation and video intro to display on welcome tab.

    -   NavBarApp folder - Module UI and Server files for app.

        -   WelcomeUI.R

-   Extra folder - Code for testing, future/alternative ideas, and demonstrations.

    -   python folder

        -   capow_tmb_with_reticulate.R - data simulation and model fitting with as far as I got with reticulate commented out.
        -   popan.ipynb - Code to test functions for nll and gradient in JAX.
        -   starting_reticulate.R - Code to setup reticulate and JAX and load python functions for nll and gradient.
        -   popan.py - Python JAX code for nll and gradient.
        -   pycache - Bytecode cache files that are automatically generated by python.

    -   Results.dat - Saved capow objects loaded when the app is started as examples, including results for Example project.

    -   Saved CaPow objects.dat - Various saved models, simulations, and projects that have been useful as examples or experiments.

    -   capow.R - Primary code for data simulation and model fitting without using TMB.

    -   maui_dolphin_dataset.csv - A real example dataset that can be uploaded into the app. I think I was also using this to compare the results of my implementation of simulating from existing data with Rachel's.
