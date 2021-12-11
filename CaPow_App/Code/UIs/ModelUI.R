# Define UI for model module
ModelUI <- function(id = "ModelUI") {
  ns <- NS(id)
  
  tabPanel(
    "Model Builder",
    div(
      style = "width:1070px;",
      tabsetPanel(
        tabPanel("Model Builder", {
          ## Load defaults
          varModelName = "e.g. M1"
          varDescription = "e.g. lambda phi(.) p(t)"
          varTimeN = "10"
          varsuperpopn = "N"
          varModelType = "lambdamodel"
          varlambdaparam = ""
          varsinglephi = ""
          vartimestart = "2001"
          
          listModelType <- list(
            "Lambda-POPAN" = "lambdamodel",
            "Standard POPAN, single Phi" = "singlephimodel",
            "Standard POPAN" = "fullmodel"
          )
          
          ## ----------------------------------------------------------------------------------------------
          ## BEGINNING OF OVERALL DISPLAY
          ## ----------------------------------------------------------------------------------------------
          tagList(  ## Beginning of overall display (left- and right-hand panels)
            ## ----------------------------------------------------------------------------------------------
            ## LEFT-HAND DISPLAY
            ## ----------------------------------------------------------------------------------------------
            ## This div starts the left-hand display
            div(style = "margin:10px; width:500px; float:left;",
                matLay(matrix(list(
                  HTML("Model Name"),
                  HTML("Description"),
                  HTML("Number of Time Periods"),
                  HTML("Superpopulation Size (N<sub>s</sub>)"),
                  extInput(id = ns("ModelName"), type = "text",
                           style = "width:20em", value = varModelName),
                  extInput(id = ns("Description"), type = "text",
                           style = "width:20em", value = varDescription),
                  extInput(id = ns("TimeN"), type = "text",
                           style = "width:20em", value = varTimeN),
                  extInput(id = ns("superpopn"), type = "text",
                           style = "width:20em", value = varsuperpopn)
                ), ncol = 2)),
                br(),
                radioButtons(ns("modeltype"), h4("Type of model:"), choices=listModelType,
                             selected=varModelType),
                br(),
                
                ## ------------------------------------------------------------------------------------------
                ## Conditional Panel for lambda-POPAN:
                ## ------------------------------------------------------------------------------------------
                conditionalPanel(
                  ns = NS(id),
                  condition = "input.modeltype == 'lambdamodel'",
                  matLay(matrix(list(
                    HTML("Lambda (&lambda;)"),
                    extInput(id = ns("lambdaparam"), type = "text",
                             style = "width:7em", value = varlambdaparam),
                    HTML("Phi (&Phi;)"),
                    extInput(id = ns("survratelambda"), type = "text",
                             style = "width:7em", value=varsinglephi),
                    HTML("Start time"),
                    extInput(id = ns("startlambda"), type = "text",
                             style = "width:7em", value=vartimestart)
                  ), ncol = 3)),
                  div(style = "padding:10px; border:1px solid; width:450px; float:left;",
                      HTML("<p><b>Easy-Fill Panel:</b></p>"),
                      tagList(
                        # extInput(id = ns("efpticklambda"), type = "checkbox",
                        #          label = "Capture Probability", reverse = TRUE,
                        #          checked = FALSE),
                        extInput(id = ns("efplambda"), type = "text",
                                 label = "Capture Probability", 
                                 style = "width:400px", value = "p(t)"), HTML("</br>")
                      ),
                      HTML("<p><b>Use the examples to fill in an expression.</b></br>",
                           'e.g. <code>p(t) or p_%t</code> generates p1, p2, p3, ...</br>',
                           'e.g. <code>p(.) or p1 </code> generates p1, p1, p1, ...</br>',
                           'e.g. <code>0.1, p2, p3, 0.2, ... </code> comma-separated values.</br> </p>'))),
                ## End of conditional panel for lambda model
                
                ## ------------------------------------------------------------------------------------------
                ## Conditional Panel for single-phi models:
                ## ------------------------------------------------------------------------------------------
                conditionalPanel(
                  ns = NS(id),
                  condition = "input.modeltype == 'singlephimodel'",
                  matLay(matrix(list(
                    HTML("Phi (&Phi;)"),
                    extInput(id = ns("survratesingle"), type = "text",
                             style = "width:7em", value=varsinglephi),
                    HTML("Start time"),
                    extInput(id = ns("startsingle"), type = "text",
                             style = "width:7em", value=vartimestart)
                  ), ncol = 2)),
                  div(style = "padding:10px; border:1px solid; width:450px; float:left;",
                      HTML("<p><b>Easy-Fill Panel:</b></p>"),
                      tagList(
                        extInput(id = ns("efpsingle"), type = "text",
                                 label = "Capture Probability", 
                                 style = "width:400px", value = "p(t)"),
                        HTML("</br>")
                      ),
                      tagList(
                        extInput(id = ns("efpentsingle"), type = "text",
                                 label = "Probability of Entry", 
                                 style = "width:400px", value = "pent(t)")
                      ),
                      
                      HTML("<p><b>Use the examples to fill in an expression.</b></br>",
                           'e.g. <code>p(t) or p_%t</code> generates p1, p2, p3, ...</br>',
                           'e.g. <code>p(.) or p1 </code> generates p1, p1, p1, ...</br>',
                           'e.g. <code>0.1, p2, p3, 0.2, ... </code> comma-separated values.</br> </p>'))),
                ## End of conditional panel for single-phi model
                
                
                ## ------------------------------------------------------------------------------------------
                ## Conditional Panel for full models:
                ## ------------------------------------------------------------------------------------------
                conditionalPanel(
                  ns = NS(id),
                  condition = "input.modeltype == 'fullmodel'",
                  
                  div(style = "padding:10px; border:1px solid; width:450px; float:left;",
                      HTML("<p><b>Easy-Fill Panel:</b></p>"),
                      tagList(
                        extInput(id = ns("efTimeLabels"), type = "text",
                                 label = "Time Labels", 
                                 style = "width:400px", value = '2000 + %t'),
                        HTML("</br>")
                      ),
                      tagList(
                        extInput(id = ns("efsurvrate"), type = "text",
                                 label = "Survival Rate", 
                                 style = "width:400px", value = "phi(t)"),
                        HTML("</br>")
                      ),
                      tagList(
                        extInput(id = ns("efpfull"), type = "text",
                                 label = "Capture Probability", 
                                 style = "width:400px", value = "p(t)"),
                        HTML("</br>")
                      ),
                      tagList(
                        extInput(id = ns("efpentfull"), type = "text",
                                 label = "Probability of Entry", 
                                 style = "width:400px", value = "pent(t)")
                      ),
                      HTML("<p><b>Use the examples to fill in an expression.</b></br>",
                           "<ul><li><code>%t</code> generates times 1, 2, 3, ...</li>",
                           "<li><code>_</code> pastes strings together</li></ul>",
                           'e.g. <code>%t</code> generates time labels 1, 2, 3, ...</br>',
                           'e.g. <code>2000 + %t</code> generates 2001, 2002, 2003, ...</br>',
                           'e.g. <code>Week_%t</code> generates Week1, Week2, Week3, ...</br>',
                           'e.g. <code>p(t) or p_%t</code> generates p1, p2, p3, ...</br>',
                           'e.g. <code>0.1, p2, p3, 0.2, ... </code> comma-separated values.</br> </p>')
                  )  ## End of div for easy-fill
                ) ## End of conditionalPanel for standard POPAN
            ),  ## End of left-hand div display
            
            ## ------------------------------------------------------------------------------------------
            ## RIGHT-HAND DIV DISPLAY
            ## ------------------------------------------------------------------------------------------
            
            ## SAVE MODEL BUTTON:
            div(style = "padding:10px; border:1px solid; width:480px; float:left;",
                actionButton(ns("saveModel"), label = "Save Model"),
                htmlOutput(ns("modelSaveMessage")),
                uiOutput(ns("savemodelclicked"))
            ),  ## end of div for Save model
            
            ## PARAMETER MATRIX DISPLAY:
            div(style = "padding:10px; border:1px solid; width:480px; float:left;",
                ## Matrix of results / inputs
                uiOutput(ns("modelParamUI"))
            ) ## end of right-hand div display
          )  ## end of tagList ** that begins the left-hand and right-hand displays
        }),  ## end of main tab
        
        ## -------------------------------------------------------------------------------------------------------------
        ## REFERENCE LISTS: sim and model
        ## -------------------------------------------------------------------------------------------------------------
        tabPanel("View Models",
                 htmlOutput(ns("detailedmodellist"))
        ),
        
        ## -------------------------------------------------------------------------------------------------------------
        ## HELP PANELS
        ## -------------------------------------------------------------------------------------------------------------
        
        ## ------------------------------
        ## GENERAL HELP:
        ## ------------------------------
        tabPanel("HELP : General",
                 HTML("The ModelBuilder lets you enter the parameters that you want to estimate, and specify any parameters that will be constrained to equal other parameters (e.g. p1=p2), or any parameters that will be given fixed numerical values and not estimated (e.g. phi=0.9)."),
                 h4("Parameter names and allowed entries:"),
                 HTML("Parameters must be entered either as numbers, or as symbols following a specified protocol:<ul><li>Superpopulation size is either 'N', or a number greater than 0. <li>Annual survival probability, phi, can be 'phi' for a single constant value; 'phi1', 'phi2', ... for time-varying survival; or a number between 0 and 1. <li>Annual population growth rate, lambda, is either 'lambda', 'phi', or a number greater than 0. See the Lambda-POPAN tab.<li>Capture probabilities are either 'p1', 'p2', ... or numbers between 0 and 1. <li>Entry proportions, known as 'probability of entry' or 'pent', are either 'pent1', 'pent2', ... or numbers between 0 and 1. The pent probabilities must sum to 1, so they can not all be estimated freely. The ModelBuilder displays the pent belonging to the first survey time as 'calculated'. This means its value is calculated from the values of the other pent probabilities to ensure that they all sum to 1. However, the parameter still exists: so for example if pent1 displays 'calculated', you can still constrain other pent parameters to be equal to pent1.</ul>"),
                 HTML("You can only constrain a parameter to be equal to another parameter that is defined in its own time period. For example, if you want to constrain p2=p1, this is only possible if p1 is defined for time slot 1. Defining the capture probabilities as (say) 0.1, p1, ... will give an error when you try to save the model, because p2 has been given the value p1, but p1 does not appear in its own timeslot: instead the capture probability for time 1 is given the fixed value 0.1."),
                 h4("Entering values:"),
                 HTML("You can enter values into the entry boxes on the left, or through the EasyFill panel, or directly into the matrix on the right. <p><p>"),
                 HTML("<strong>Entering values into the matrix:</strong> Press Enter or click out of the box to commit the change.<p>"),
                 HTML("<strong>Using the EasyFill Panel:</strong>"),
                 HTML("The EasyFill panel offers several commonly-used short cuts. Changes will not be applied until the corresponding checkbox is ticked. You should untick the box straight after applying the changes, to avoid confusion when you make subsequent changes in the matrix or switch between model types. <p>Follow these examples:<ul><li>p(.) generates p1, p1, p1, ... Similarly, phi(.) generates phi, phi, phi, ... <li> pent(.) generates pent2, pent2, pent2, ... because you probably intend equal pent probabilities <i>after</i> the initial survey. If you want equal pent probabilities for all surveys including the first, then all of the probabilities must be 1/T where T is the number of surveys. Just enter 1/T into the panel as a decimal number for the correct T. <li>p(t), phi(t), and pent(t) generate fully time-varying results: e.g. p1, p2, p3, ... <li>To enter all values without needing to type into the matrix, enter them as comma-separated values: e.g. p1, 0.1, p3, 0.1, ... constrains p2=p4=0.1 whereas p1 and p3 are estimated as free parameters. If there is no survey at a particular time, leave the entry blank or use '-', e.g. p1, , p3 or p1, -, p3, ... if there is no survey at time 2. If you don't enter enough values for all the time periods displayed on the matrix, your entry will be recycled. For example, if there are 5 time periods but you only enter p1, p2, p3, then you will receive p1, p2, p3, p1, p2.<li>Special characters for shortcuts are '%t', which generates 1, 2, 3, ..., and the underscore '_', which pastes strings together. For example, entering Week_%t in Time Labels will generate Week1, Week2, Week3, ... because the 1, 2, 3, ... given by %t is pasted to 'Week'. </ul>"),
                 h4("Saving models:"),
                 HTML("To save your model, tick the Save Model box. The model format will be checked and any errors reported back to you. If you get an error, your model has not been saved and you must untick the box, correct the error, and tick the box again to save the model.  If you do not tick Save again, your model will be saved only in temporary draft form and not to the permanent model list. You can open up the draft and save the model by restarting ModelBuilder(), but the draft will be overwritten if you open other models in the interim.</p><p>After you have saved the model, you must untick the Save Model box to continue to edit new models."),
                 HTML("<p>In some cases, you will get a warning when saving and you must decide whether you want to continue. These cases are:<ul><li>The model name corresponds to a model that is already saved. If you want to continue and overwrite the specified model, enter the requested number in the Confirm Number box and your model will be saved. You can then untick the Save Model box and continue to edit new models.<li>The model name corresponds to a model that is already saved <i>and</i> part of a project. The software will not allow you to make changes to a model that is already part of a project. You must either agree to delete the project, including any results, or you must untick the Save Model box and change the name of the model before saving it. <li>Your model is suspected to be non-identifiable. This can only happen for Standard POPAN models with time-varying p, phi, and pent parameters. It is not possible to estimate all parameters in the full p(t) phi(t) pent(t) model, and some values of p, phi, or pent must be constrained to be equal to others or fixed at numeric values. The software will allow you to continue saving your model, but you should be aware that your results may be suspect and the boxplots should be inspected carefully when you get your CaPow results. The software does not have exhaustive checks for non-identifiability, and a model can be identifiable for some data sets and not for others, so this feature should be taken only as a guideline. </ul>")
        ),
        tabPanel("All Models",
                 h5("Model Name and Description:"),
                 HTML("Enter a short name, e.g. M1, and description, e.g. lambda phi(.) p(t). The description will be used when displaying results for multiple models.<p>"),
                 h5("Number of time periods:"),
                 HTML("Number of periods from the first survey to the last survey, inclusive.<p>"),
                 HTML("The superpopulation size, N, will cover the period from the first ticked survey to the last ticked survey, regardless of how many time periods are entered here. Time periods must be equally-spaced if using the lambda-POPAN model or the standard POPAN model with single Phi.<p>"),
                 h5("Selecting time periods for surveys:"),
                 HTML("Tick the boxes on the right-hand panel to denote that a survey will take place in the corresponding time period.<p>"),
                 h5("Superpopulation size, N:"),
                 HTML("<ul><li>Enter 'N' if N is to be estimated as a free parameter; <li>Enter a number if N is to be fixed at that number and not estimated. For example, enter 1000 if constraining N=1000. </ul>"),
                 HTML("The superpopulation size is the number of animals available for capture between the first ticked survey and the last ticked survey inclusive. It does <i>not</i> include any animals that were born and died entirely within a survey gap, because these animals were never available for capture.<p>"),
                 h5("Capture probabilities:"),
                 HTML("Capture probabilities can be entered from the Easy-Fill panel following the instructions given. The survey box must be ticked for a capture probability to be entered.
Capture probabilities can also be entered directly into the parameter matrix on the right, for the ticked time periods in which surveys will occur.<p>"),
                 HTML("<ul><li>Enter 'p1' if the first capture probability is to be estimated as a free parameter; <li>Enter a number if p1 is to be fixed at that number and not estimated; <li>Enter 'pt' for another t if the first capture probability is to be constrained equal to the capture probability for time period t. For example, if constraining p1=p2, you can either enter 'p1' under both of the first two time periods, or 'p2' under both of the first two time periods. Note that you can only use pt for a time period if pt is also used for its own time period t.</ul>")
        ),
        
        ## --------------------------------------------------------------------------------------------------
        ## LAMBDA-POPAN HELP:
        ## --------------------------------------------------------------------------------------------------
        tabPanel("Lambda-POPAN",
                 h4("Lambda-POPAN models"),
                 HTML("The pent parameters are constrained to follow a growth curve. Overall growth from one time period to the next is lambda.  For example, if lambda=1.02, then for every 100 animals alive in one time period there are 102 alive in the next, after births and deaths have been accommodated. <br>Survival probability from one time period to the next is phi. <br>Lambda and phi are constant for all time periods. The minimum value of lambda is phi. Time periods are assumed to be equally-spaced because constant survival and growth is applied to each period.<br>"),
                 HTML("<br>When a lambda-POPAN model is selected, the pent parameters display 'Calculated'. This means they are calculated from the lambda and phi values and are not estimated as free parameters. Values for pent can not be entered in the matrix. The value of phi can not be entered in the matrix either, but must be entered via the single-phi entry box.</p>"),
                 HTML("<p>Three entry boxes are displayed for lambda-POPAN models:</p>"),
                 HTML("<b>Lambda:</b>"),
                 HTML("<ul><li>Enter 'lambda' if lambda is to be estimated as a free parameter; <li>Enter a number if lambda is to be fixed at that number and not estimated. For example, enter 1.0 if constraining lambda=1 for a population remaining at constant size; <li>Enter 'phi' if lambda is to be constrained equal to survival, phi. This represents a population suffering deaths but no births.</ul> "),
                 HTML("If lambda and phi are both numbers, lambda must be greater than or equal to phi. <br>If lambda=1 and phi=1, the result is a closed-population model (Model M<sub>t</sub>).</p>"),
                 HTML("<b>Phi:</b>"),
                 HTML("<ul><li>Enter 'phi' if phi is to be estimated as a free parameter; <li>Enter a number if phi is to be fixed at that number and not estimated. For example, enter 0.95 if constraining phi=0.95. This can be useful if there is biological knowledge of survival rates and the data are too sparse to enable phi to be estimated effectively.</ul>"),
                 HTML("The 'phi' displayed at any time point governs survival <i>from</i> that time <i>to</i> the next time, whether or not surveys occur at these times. All values of phi from the last ticked survey onwards are shown as blank in the matrix, because any survival occurring after the last survey can not be included in the estimation process.</p>"),
                 HTML("<b>Start time:</b>"),
                 HTML("<ul><li>Enter the time label for the first survey, e.g. 1 or 2001. Time periods must be equally-spaced for lambda-POPAN models, so time labels are automatically determined from the start time and the number of time periods. The start time should be a number.</ul>"),
                 HTML("In addition, capture probabilities can be entered via the Easy-Fill panel or directly into the matrix. You can only enter capture probabilities into the matrix when the corresponding checkbox is ticked, denoting that a survey will take place in that time period.<p>")
        ),
        
        ## --------------------------------------------------------------------------------------------------
        ## SINGLE-PHI HELP
        ## --------------------------------------------------------------------------------------------------
        tabPanel("Single-Phi POPAN",
                 h4("Standard POPAN with single Phi"),
                 HTML("This option provides a short-cut for models where survival is assumed constant for every time period. Time periods in such models will generally be equally-spaced, so you only have to specify a start time to establish the matrix layout.</p>The single-Phi model allows the pent parameters to be estimated as free parameters, rather than constraining them to a growth curve as in the lambda-POPAN model.</p>"),
                 HTML("<p>Two entry boxes are displayed for single-Phi models:</p>"),
                 HTML("<b>Phi:</b>"),
                 HTML("<ul><li>Enter 'phi' if phi is to be estimated as a free parameter; <li>Enter a number if phi is to be fixed at that number and not estimated. For example, enter 0.95 if constraining phi=0.95. This can be useful if there is biological knowledge of survival rates and the data are too sparse to enable phi to be estimated effectively.</ul>"),
                 HTML("The 'phi' displayed at any time point governs survival <i>from</i> that time <i>to</i> the next time, whether or not surveys occur at these times. All values of phi from the last ticked survey onwards are shown as blank in the matrix, because any survival occurring after the last survey can not be included in the estimation process.</p>"),
                 HTML("<b>Start time:</b>"),
                 HTML("<ul><li>Enter the time label for the first survey, e.g. 1 or 2001. Time periods must be equally-spaced for single-Phi models, so time labels are automatically determined from the start time and the number of time periods. The start time should be a number.</ul>"),
                 HTML("In addition, you must select parameter inputs for capture probabilities and entry proportions. Each of these apply only at times when surveys take place. <ul><li>Capture probabilities can be entered via the Easy-Fill panel or directly into the matrix. <li>Entry proportions can be entered via the Easy-Fill panel or directly into the matrix.</ul><p>"),
                 h4("Identifiability check"),
                 HTML("When capture probabilities and pent parameters are time-varying, there is confounding between the first p and the first pent, because there is no previous information for the model to be able to distinguish between animals that entered the population but were not detected at survey 1, and those that had not entered at survey 1. <p>This means that a model of the form p(t)pent(t) is not identifiable, because p1 and pent1 are confounded. (If no survey occurs at time 1, the problem applies to the first time at which there is a survey.)<p>To unconfound these parameters, choose another p to make equal to p1, or another pent to make equal to pent1. Note that pent1 is listed as 'Calculated', but you can still enter pent1 as the pent parameter for any other time t.<p>The software will allow you to save a model suspected to be non-identifiable, but you should be careful that outputs from the model are likely to be unreliable. Check the boxplots and other CaPow results carefully for evidence of problems.")
        ),
        
        ## --------------------------------------------------------------------------------------------------
        ## STANDARD-POPAN HELP:
        ## --------------------------------------------------------------------------------------------------
        tabPanel("Standard POPAN",
                 h4("Standard-POPAN models"),
                 HTML("This tab offers the most flexible model-building options. You can have time-varying p, phi, and pent parameters - note the comments about identifiability below.  Surveys do not need to be equally-spaced.</p>This is the only model that allows time-varying Phi, so some special comments apply."),
                 h5("Time-varying Phi:"),
                 HTML("The survival rate, phi, can be entered using the Easy-Fill panel or directly into the matrix, like the other parameters. Things to be aware of:<ul><li>You can use 'phi' everywhere, to give a single-phi model with or without equal time-spacing; or you can use phi1, phi2, ... to give time-varying Phi.  However, you may not mix these two types of symbols. Only a single 'phi' everywhere, <i>or</i> the symbols phi1, phi2, ..., are accepted. If you do mix the symbols, you will get an error when you try to save your model.<li>Every value of phi corresponds to survival <i>between</i> two time periods. The phi that you enter at time period 1 corresponds to survival <i>between</i> times 1 and 2; and so on for the other times. There is no phi entry from the last ticked survey time onwards, because we can not estimate survival after the end of the surveys.<li>The software expects you to enter Phi values for intermediate times even if surveys do <i>not</i> take place, because the process of survival must continue regardless of survey occurrence. If you do not wish to specify Phi values in survey gaps, you should redefine your time periods so that you do not have survey gaps. For example, if you have surveys in years 2001, 2003, and 2008, you could set the matrix to have three time periods with labels 2001, 2003, and 2008, and two different Phi values: the first (entered under 2001) to control survival between 2001 and 2003; and the second (entered under 2003) to control survival between 2003 and 2008.<li>If you prefer to specify your model with survey gaps, and to assign a parameter for Phi across these gaps, you may only use Phi parameters that are associated with survey times. For example, if surveys occur in 2001, 2003, and 2008, you might decide that you want to use parameter phi1 to describe survival from 2001-2 and 2002-3, parameter phi3 for survival from 2003-4 and 2004-5, and parameter phi1 again for 2005-6, 2006-7, and 2007-8, basing your choice on years with similar weather conditions. You may only use phi1 and phi3, because these are the only years (prior to the last) at which surveys occur. Note also the comments below about identifiability of this model. Alternatively, any Phi parameters can be entered as numerical values to avoid estimating them.</ul>"),
                 h5("Other parameters:"),
                 HTML("As usual, you must select parameter inputs for capture probabilities and entry proportions. Each of these apply only at times when surveys take place. <ul><li>Capture probabilities can be entered via the Easy-Fill panel or directly into the matrix. <li>Entry proportions can be entered via the Easy-Fill panel or directly into the matrix.</ul><p>For standard-POPAN models, you have a completely free choice of how you describe your time periods, and whether or not they are equally spaced. These are for your reference only, and will not be used in model fitting."),
                 h4("Identifiability check"),
                 HTML("When capture probabilities and pent parameters are time-varying, there is confounding between the first p and the first pent, because there is no previous information for the model to be able to distinguish between animals that entered the population but were not detected at survey 1, and those that had not entered at survey 1. <br>Additionally, when capture probabilities and phi parameters are time-varying, there is confounding between the last p (associated with the last ticked survey) and the last phi (associated with the penultimate ticked survey), because there is no later information to distinguish between animals that survived the last time period but were not detected in the last survey, and those that died before the last survey.<p>This means that a model of the form p(t)pent(t) is not identifiable, because the first p and the first pent are confounded. Similarly, a model of the form p(t)phi(t) is not identifiable, because the last p and the last phi are confounded.<p>To unconfound the first p and pent, choose another p to make equal to p1, or another pent to make equal to pent1. Note that pent1 is listed as 'Calculated', but you can still enter pent1 as the pent parameter for any other time t. (If there is no survey at time 1, the same applies to the first time at which there is a survey.)<p>To unconfound the last p and phi, choose another p to make equal to the last p, or another phi to make equal to the last phi (corresponding to the penultimate ticked survey).<p>The software will allow you to save a model suspected to be non-identifiable, but you should be careful that outputs from the model are likely to be unreliable. Check the boxplots and other CaPow results carefully for evidence of problems.")
        )
      )
    )
  )
}