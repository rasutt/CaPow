### CaPow!

A Shiny web application for open population capture-recapture model fitting and study design via fast simulation and optimization using automatic differentiation. This repository contains the current version of the application, and some associated resources.

#### How it works

The application begins by running the UI and server files, which form the basis of a shiny application. They source other files which load the necessary functions and the define the UI's and server functions of the components of the app. Most of the components are the main tabs of the application, but a few are sub-components of the fit builder, projects, and save/load tabs, and one or two are re-used within various other components. Some example objects are also loaded when the application is started.

#### General notes

-   Running pre-compile after TMB is installed speeds up compilation a lot. Should only do once as it takes a long time though it doesn't actually happen till the next time you compile a function. Doesn't matter for users because they only compile the function once.

-   You may have to restart R if you recompile the objective function. Either windows or pre-compilation seems to have fixed this, but there is also a fix for linux on the TMB install github page.

-   I have had a lot of trouble with TMB not compiling or giving random errors when I re-install, but it seems to be working now. This may have been due to changing compiler settings to use certain deep learning libraries.

-   I started writing code to use JAX in python to find the NLL and do automatic differentiation. I made some progress but I'm not sure I got it working.

-   The first version of CaPow sometimes converged to false minima due to tricks used to enforce parameter bounds manually and avoid boundary estimates.

#### Files and folders

-   things_to_do.R - Old list of things to do. Should merge with this readme.

-   App folder - The app.

    -   ui.R - Code specifying GUI for app.

    -   server.R - Code to run app according to specified GUI.

    -   popan.cpp - NLL function in C++ template for automatic differentiation with TMB.

    -   popan.o and popan.dll - Files created by compilation of popan.cpp by TMB.

    -   Data folder

        -   CaPow.dat - Saved capow objects loaded when the app is started as examples. I deleted the results from Example project to quickly test running projects.

        -   example_dataset.csv - A simulated example dataset that can be uploaded into the app.

    -   www folder - Slides and video intro to display on welcome tab.

    -   Code folder

        -   capow_tmb.R - Primary code for data simulation and model fitting using TMB.

        -   CaPowGUIFuncs.R - Functions creating components for GUI.

        -   UIs folder - Files with UIs for all main tabs, and object summary displays.

        -   Servers folder - Files with code to serve all main tabs except Welcome, and object summary displays, according to specified GUIs.

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
