WelcomeUI <- function(id = "WelcomeUI", label = "WelcomeUI") {
  ns <- NS(id)
  
  tabPanel(
    "Welcome",
    div(
      style = "width:1070px;",
      fluidPage(
        h2("CaPow! Capture-Recapture Power Analysis and Model Exploration"),
        tags$em('A web app by Robin Aldridge-Sutton (rald898@aucklanduni.ac.nz), Jimmy Oh, Emma Carroll, & Rachel Fewster'),
        tags$br(),
        tags$em('Deptartment of Statistics & School of Biological Sciences, University of Auckland, New Zealand'),
        
        h3("Welcome"),
        p("CaPow! is a new Shiny web application for open-population
        capture-recapture models, with an emphasis on study design and model
        exploration as well as data analysis. The key features are an
        easy-to-use web interface for designing simulations and models, and
        efficient computation which enables thousands of simulated datasets to
        be fitted within seconds. Modelling follows the well-known POPAN
        style, including a superpopulation with time-varying population sizes,
        survival rates, and either free birth parameters or a constant
        population growth rate. Outputs include power analysis of proposed
        study designs, and diagnostic tools for exploring model precision and
        effects of model misspecification."),
        p("CaPow! runs online in a web browser and does not require any software
        to be installed. Sessions can be saved and uploaded again for future
        use."),
        p("Watch the video introduction, explore the modules above, 
          or view the presentation slides below for a more detailed explanation."),
        p("Note - Any objects that you create in the app will be deleted if you disconnect, refresh the page, hit the back button, 
          or load a previously saved session, so save your work often!"),
        p("Feedback sent to the email address above would be greatly appreciated, and is likely to guide further development."),
        
        h3("Video Introduction"),
        tags$video(
          width="1000",
          height="500",
          src="video_intro.mp4", 
          type="video/mp4",
          controls="controls"
        ),
        
        h3("Presentation Slides"),
        tags$iframe(
          src="Capow_Demo2.pdf#view=FitV", 
          width="1000", 
          height="500"
        )
      )
    )
  )
}