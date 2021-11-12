# Define UI for sim module
SimUI <- function(id = "SimUI", label = "SimUI") {
  ns <- NS(id)

  tabPanel(
    "Sim Builder",
    div(
      style = "width:1070px;",
      tabsetPanel(
        tabPanel("Sim Builder", {
          ## If previous sim given, load that
          # if(exists("tempSim", envir = CPenv, inherits = FALSE)){
          #   tempSim = get("tempSim", envir = CPenv)
          #   varSimName = tempSim$simname
          #   varDescription = tempSim$description
          #   varTimeN = nrow(tempSim$paramdf)
          #   varsuperpopn = tempSim$superpopn
          #   varSimType = tempSim$simtype
          #   varlambdaparam = tempSim$lambdaparam
          #   if(varSimType=="lambdasim" | varSimType=="singlephisim"){
          #     varsinglephi =
          #       tempSim$paramdf$survrate[tempSim$paramdf$survrate!=""][1]
          #     vartimestart = tempSim$paramdf$timelabels[1]
          #   }
          #   else {
          #     varsinglephi = ""
          #     vartimestart = ""
          #   }
          #   
          # } else {
            ## Otherwise use defaults
            varSimName = "e.g. S1"
            varDescription = "e.g. lambda=1.05  phi=0.9  p=0.1"
            varTimeN = "10"
            varsuperpopn = ""
            varSimType = "lambdasim"
            varlambdaparam = ""
            varsinglephi = ""
            vartimestart = "2001"
          # }
          
          listSimType <- list("Lambda-POPAN" = "lambdasim",
                              "Standard POPAN, single Phi" = "singlephisim",
                              "Standard POPAN" = "fullsim")
          
          ## The line below no longer applies: it used to be the case that when
          ## radiobuttons were defined,
          ## their selected choice used the name (e.g. Standard POPAN) rather than the ID (e.g.
          ## fullsim).  However this seems to have changed in later versions of Shiny.
          ## nameSimType <- names(listSimType)[listSimType==varSimType]
          
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
                  HTML("Sim Name"),
                  HTML("Description"),
                  HTML("Number of Time Periods"),
                  HTML("Superpopulation Size (N<sub>s</sub>)"),
                  extInput(id = ns("SimName"), type = "text",
                           style = "width:20em", value = varSimName),
                  extInput(id = ns("Description"), type = "text",
                           style = "width:20em", value = varDescription),
                  extInput(id = ns("TimeN"), type = "text",
                           style = "width:20em", value = varTimeN),
                  extInput(id = ns("superpopn"), type = "text",
                           style = "width:20em", value = varsuperpopn)
                ), ncol = 2)),
                
                br(),
                radioButtons(ns("simtype"), h4("Type of simulation:"), 
                             choices=listSimType,
                             selected=varSimType),
                
                br(),
                
                ## ------------------------------------------------------------------------------------------
                ## Conditional Panel for lambda-POPAN:
                ## ------------------------------------------------------------------------------------------
                conditionalPanel(ns = NS(id),
                                 condition = "input.simtype == 'lambdasim'",
                                 matLay(matrix(list(
                                   HTML("Lambda (&lambda;)"),
                                   extInput(id = ns("lambdaparam"), type = "text",
                                            style = "width:7em", value = varlambdaparam),
                                   HTML("Phi (&Phi;)"),
                                   extInput(id = ns("survratelambda"), type = "text",
                                            style = "width:7em", value = varsinglephi),
                                   HTML("Start time"),
                                   extInput(id = ns("startlambda"), type = "text",
                                            style = "width:7em", value = vartimestart)
                                 ), ncol = 3)),
                                 div(style = "padding:10px; border:1px solid; width:450px; float:left;",
                                     HTML("<p><b>Easy-Fill Panel:</b></p>"),
                                     tagList(
                                       # extInput(id = ns("efpticklambda"), type = "checkbox",
                                       #          label = "Capture Probability", reverse = TRUE,
                                       #          checked = FALSE),
                                       extInput(id = ns("efplambda"), type = "text", style = "width:400px",
                                                label = "Capture Probability", 
                                                value = paste0(rep("0.1", 10), collapse=", ")), HTML("</br>")
                                     ),
                                     HTML("<p><b>Use the examples to fill in an expression.</b></br>",
                                          'e.g. <code>0.2 </code> generates 0.2, 0.2, 0.2, ...</br>',
                                          'e.g. <code>0.1, 0.2, 0.3, ... </code> comma-separated values.</br> </p>'))),
                ## End of conditional panel for lambda sim
                
                ## ------------------------------------------------------------------------------------------
                ## Conditional Panel for single-phi sims:
                ## ------------------------------------------------------------------------------------------
                conditionalPanel(ns = NS(id),
                                 condition = "input.simtype == 'singlephisim'",
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
                                       # extInput(id = ns("efpticksingle"), type = "checkbox",
                                       #          label = "Capture Probability", reverse = TRUE,
                                       #          checked = FALSE),
                                       extInput(id = ns("efpsingle"), type = "text",
                                                style = "width:400px",
                                                label = "Capture Probability", 
                                                value= paste0(rep("0.1", 10), collapse=", ")),
                                       HTML("</br>")
                                     ),
                                     tagList(
                                       # extInput(id = ns("efpentticksingle"), type = "checkbox",
                                       #          label = "Probability of Entry", reverse = TRUE,
                                       #          checked = FALSE),
                                       extInput(id = ns("efpentsingle"), type = "text",
                                                style = "width:400px",
                                                label = "Probability of Entry", 
                                                value =paste0(c("10", rep("1", 9)), collapse=", ")
                                       )
                                     ),
                                     HTML("<p><b>Use the examples to fill in an expression.</b></br>",
                                          'e.g. <code>0.2 </code> generates 0.2, 0.2, 0.2, ...</br>',
                                          'e.g. <code>0.1, 0.2, 0.3, ... </code> comma-separated values.</br> </p>'))),
                
                ## End of conditional panel for single-phi sim
                
                
                ## ------------------------------------------------------------------------------------------
                ## Conditional Panel for full sims:
                ## ------------------------------------------------------------------------------------------
                conditionalPanel(ns = NS(id),
                                 condition = "input.simtype == 'fullsim'",
                                 
                                 div(style = "padding:10px; border:1px solid; width:450px; float:left;",
                                     HTML("<p><b>Easy-Fill Panel:</b></p>"),
                                     tagList(
                                       # extInput(id = ns("efTimeLabelstick"), type = "checkbox",
                                       #          label = "Time Labels", reverse = TRUE, checked = FALSE),
                                       extInput(id = ns("efTimeLabels"), type = "text",
                                                label = "Time Labels", 
                                                style = "width:400px", value = '2000 + %t'), HTML("</br>")
                                     ),
                                     tagList(
                                       # extInput(id = ns("efptickfull"), type = "checkbox",
                                       #          label = "Capture Probability", reverse = TRUE,
                                       #          checked = FALSE),
                                       extInput(id = ns("efpfull"), type = "text",
                                                style = "width:400px",
                                                label = "Capture Probability", 
                                                value= paste0(rep("0.1", 10), collapse=", ")),
                                       HTML("</br>")
                                     ),
                                     tagList(
                                       # extInput(id = ns("efsurvratetick"), type = "checkbox",
                                       #          label = "Survival Rate", reverse = TRUE, checked = FALSE),
                                       extInput(id = ns("efsurvrate"), type = "text",
                                                style = "width:400px",
                                                label = "Survival Rate", 
                                                value= paste0(rep("0.9", 10), collapse=", ")),
                                       HTML("</br>")
                                     ),
                                     tagList(
                                       # extInput(id = ns("efpenttickfull"), type = "checkbox",
                                       #          label = "Probability of Entry", reverse = TRUE,
                                       #          checked = FALSE),
                                       extInput(id = ns("efpentfull"), type = "text",
                                                style = "width:400px",
                                                label = "Probability of Entry", 
                                                value =paste0(c("10", rep("1", 9)), collapse=", "))
                                     ),
                                     HTML("<p><b>Use the examples to fill in an expression.</b></br>",
                                          "<ul><li><code>%t</code> generates times 1, 2, 3, ...</li>",
                                          "<li><code>_</code> pastes strings together</li></ul>",
                                          'e.g. <code>%t</code> generates time labels 1, 2, 3, ...</br>',
                                          'e.g. <code>2000 + %t</code> generates 2001, 2002, 2003, ...</br>',
                                          'e.g. <code>Week_%t</code> generates Week1, Week2, Week3, ...</br>',
                                          'e.g. <code>0.2 </code> generates 0.2, 0.2, 0.2, ...</br>',
                                          'e.g. <code>0.1, 0.2, 0.3, ... </code> comma-separated values.</br> </p>')
                                 )  ## End of div for easy-fill
                ) ## End of conditionalPanel for standard POPAN
            ),  ## End of left-hand div display
            
            ## ------------------------------------------------------------------------------------------
            ## RIGHT-HAND DIV DISPLAY
            ## ------------------------------------------------------------------------------------------
            
            ## SAVE SIM BUTTON:
            div(style = "padding:10px; border:1px solid; width:500px; float:left;",
                actionButton(ns("saveSim"), label = "Save Sim"),
                htmlOutput(ns("simSaveMessage")),
                uiOutput(ns("savesimclicked"))
            ),  ## end of div for Save sim
            
            # # Dataset and fit model selection
            # div(style = "padding:10px; border:1px solid; width:500px; float:left;",
            #     uiOutput(ns("fitselection"))
            # ),  ## end of div for fit selection         
            
            ## PARAMETER MATRIX DISPLAY:
            div(style = "padding:10px; border:1px solid; width:500px; float:left;",
                ## Matrix of results / inputs
                uiOutput(ns("simParamUI"))
            ) ## end of right-hand div display
          )  ## end of tagList ** that begins the left-hand and right-hand displays
        }),  ## end of main tab
        
        ## -------------------------------------------------------------------------------------------------------------
        ## Ns CALCULATOR TAB
        ## -------------------------------------------------------------------------------------------------------------
        tabPanel(HTML("N<sub>s</sub> Calculator"),
                 h4(HTML("N<sub>s</sub> Calculator for lambda-POPAN models")),
                 ## This div starts the left-hand input display
                 div(style = "margin:10px; width:500px; float:left;",
                     HTML("This tab enables you to calculate the superpopulation size, N<sub>s</sub>, under the assumptions of a lambda-POPAN model with constant lambda and constant phi, given the desired population size at a single specified time, which we will call the <i>base time, t</i>.</p><p>For example, you can find the correct superpopulation size N<sub>s</sub> for the period 2001 to 2014 inclusive, such that the expected population size in 2003 is E(N<sub>2003</sub>)=400 animals. The base time is t=2003, and the base population size is E(N<sub>t</sub>)=400.</p>"),
                     HTML("<p><strong>Accommodating Survey Gaps:</strong><br>The N<sub>s</sub> value covers the total number of animals that were ever <i>exposed to a survey</i>. If there are gaps between surveys, some animals may be born and die within a gap and are omitted from the superpopulation. You can accommodate survey gaps by using the entry box 'Surveys' instead of the 'Start' and 'End' boxes.</p>"),
                     HTML("<p>To set the input parameters, enter:<ul><li>Values for lambda and phi;<li>The base time, t (e.g. 2003);<li>The base population size at this time, E(N<sub>t</sub>) (e.g. 400).</ul>"),
                     HTML("<p>To set the time range over which N<sub>s</sub> will be calculated, <i>either</i> :<ul><li>Enter the start time for N<sub>s</sub> (e.g. 2001), and the end time for N<sub>s</sub> (e.g. 2014), and <strong>leave the 'Surveys' box blank</strong>. The calculator will assume that surveys occur at every intermediate timepoint.</ul>
<i>Or:</i><ul><li>Enter the survey times as a comma-separated list. Use a dash (-) or colon (:) to indicate ranges. For example, entering '1-3, 8, 10:12' will give the times 1, 2, 3, 8, 10, 11, 12.</ul>"),
                     HTML("<p>The 'Start' and 'End' boxes are ignored if the 'Surveys' box has an entry.</p>"),
                     HTML("The base time does not need to be in the range specified by N<sub>s</sub>: for example, you can specify there should be E(N<sub>1999</sub>)=300 animals in 1999, and find the correct superpopulation size N<sub>s</sub> for the period 2001-2014.</p><p>Constant values of lambda and phi will be assumed for the full span of time embracing the base time and the N<sub>s</sub> period. Time points are assumed equally spaced, and must be integers.</p>"),
                     HTML("<strong>Output:</strong><ul><li>The Cumulative N<sub>s</sub> column shows the superpopulation size from the first survey specified up to and including the time shown.  <li> The p<sub>ent</sub> column shows the entry proportions at each survey time corresponding to the survey configuration specified. <li>The E(N<sub>t</sub>) column shows the average number of animals in the population at each survey time. The actual number will vary about the average due to natural variability in birth and death.</ul>")
                 ),
                 div(style = "padding:10px; border:1px solid; width:500px; float:left;",
                     h5("Input parameters"),
                     matLay(matrix(list(
                       HTML("Lambda (&lambda;)"),
                       extInput(id = ns("lambdaDesired"), type = "text", style = "width:5em",
                                value = "1.0"),
                       HTML("Phi (&Phi;)"),
                       extInput(id = ns("phiDesired"), type = "text", style = "width:5em",
                                value="0.9"),
                       HTML("Base time, t"),
                       extInput(id = ns("baseTime"), type = "text", style = "width:5em",
                                value="2003"),
                       HTML("Base pop, E(N<sub>t</sub>)"),
                       extInput(id = ns("baseNt"), type = "text", style = "width:5em",
                                value="400")
                     ), ncol = 4)),
                     h5(HTML("Time range for N<sub>s</sub>")),
                     matLay(matrix(list(
                       HTML("Start time"),
                       extInput(id = ns("startTimeNs"), type = "text", style = "width:5em",
                                value = "2001"),
                       HTML("End time"),
                       extInput(id = ns("endTimeNs"), type = "text", style = "width:5em",
                                value="2014"),
                       HTML("<strong>Or:</strong>"),
                       HTML(""),
                       HTML("Surveys, e.g. 2001-2003, 2008, 2014"),
                       extInput(id = ns("survTimesNs"), type = "text", style = "width:15em",
                                value="")
                     ), ncol=4)),
                     br(),
                     htmlOutput(ns("NsCalc"))
                 )  ## End right-hand div
                 
        ),
        
        ## -------------------------------------------------------------------------------------------------------------
        ## REFERENCE LISTS: sim and model
        ## -------------------------------------------------------------------------------------------------------------
        tabPanel("View Sims",
                 htmlOutput(ns("detailedsimlist"))
        ),

        ## -------------------------------------------------------------------------------------------------------------
        ## HELP PANELS
        ## -------------------------------------------------------------------------------------------------------------
        
        ## ------------------------------
        ## GENERAL HELP:
        ## ------------------------------
        tabPanel("HELP : General",
                 HTML("The SimBuilder lets you enter the parameter values you want to simulate from. The set of values you choose is called the 'simset' for short."),
                 # h4("Entry and exit:"),
                 # HTML("You can start SimBuilder to edit or copy an existing simset.<ul><li>SimBuilder() opens with default settings. By default, the interface will open with the most recent simset you have been working on. If you don't want this, use SimBuilder(reset=T). <li>SimBuilder(\"S1\") opens simset S1 for editing. <li>SimBuilder(copy=\"S1\") copies simset S1 to create a new simset. The new simset is based on simset S1, but has a blank name to ensure simset S1 is not accidentally overwritten.</ul><p>"),
                 # HTML("To finish, close the process in R using the Escape key or the Stop button. (Closing your browser is not necessary and does not end the process in R.) The simset that you have just been editing will be saved temporarily in draft form, so if your system crashes it will not be lost. However, it will be lost if you reopen the system with SimBuilder(reset=T)."),
                 h4("Parameter values and allowed entries:"),
                 HTML("Parameter values must be numbers following these rules:<ul><li>Superpopulation size is a number greater than 0. <li>Annual survival probability, phi, is a number between 0 and 1. <li>Annual population growth rate, lambda, is a number greater than 0. Lambda must be no less than phi. See the Lambda-POPAN tab.<li>Capture probabilities are numbers between 0 and 1. <li>Entry proportions, known as 'probability of entry' or 'pent', are numbers greater than 0. The pent probabilities must sum to 1, but you can enter them as relative numbers in the 'Relative p<sub>ent</sub>' column and let the software rescale them. For example, if you enter pent = 3, 1, 1 in the 'Relative p<sub>ent</sub>' column, they will be scaled with the denominator (3+1+1) = 5, to display 3/5, 1/5, 1/5 in the scaled 'p<sub>ent</sub>' column to the right of the matrix.<li>Fractions like '1/5' must be entered as decimal values (e.g. 0.2).</ul>"),
                 h4("Entering values:"),
                 HTML("You can enter values into the entry boxes on the left, or through the EasyFill panel, or directly into the matrix on the right. <p><p>"),
                 HTML("<strong>Entering values into the matrix:</strong> Press Enter or click out of the box to commit the change.<p>"),
                 ## HTML("<p>When entering into the matrix on the right, you must type quickly or the matrix will update before you have finished. Errors can result if you are typing while an update occurs. If the numbers start blinking, you will have to close the process in R and start again. Your work will be saved in draft form, so simply type SimBuilder() to restart where you left off.<p>These problems happen with the matrix because it is displaying outputs as well as inputs, and the system is constantly updating its outputs to respond to new inputs. All the other entry boxes are inputs only, and do not have this problem. <p>To avoid problems with entering values into the matrix, first decide what you need to type, and then type it without hesitating. If you need to delete characters, it's best to highlight them with the mouse and delete them in a block rather than deleting one character at a time with the backspace key.  If you need to delete and retype more than two or three characters, you might find it easier to enter the values as a comma-separated list in the EasyFill panel."),
                 HTML("<strong>Using the EasyFill Panel:</strong>"),
                 HTML("The EasyFill panel offers several short cuts. Changes will not be applied until the corresponding checkbox is ticked. You should untick the box straight after applying the changes, to avoid confusion when you make subsequent changes in the matrix or switch between model types. <p>Follow these examples:<ul><li>Entering a single number, like 0.2, generates 0.2, 0.2, 0.2, ...
<li>To enter all values without needing to type into the matrix, enter them as comma-separated values: e.g. 0.4, 0.3, 0.4, ... . If there is no survey at a particular time, leave the entry blank or use '-', e.g. 0.4, , 0.4 or 0.4, -, 0.4, ... if there is no survey at time 2. If you don't enter enough values for all the time periods displayed on the matrix, your entry will be recycled. For example, if there are 5 time periods but you only enter 0.1, 0.2, 0.3, then you will receive 0.1, 0.2, 0.3, 0.1, 0.2.<li>Special characters for shortcuts are '%t', which generates 1, 2, 3, ..., and the underscore '_', which pastes strings together. For example, entering Week_%t in Time Labels will generate Week1, Week2, Week3, ... because the 1, 2, 3, ... given by %t is pasted to 'Week'. </ul>"),
                 h4("Saving SimSets:"),
                 HTML("To save your simset, tick the Save Sim box. The entries will be checked and any errors reported back to you. If you get an error, your simset has not been saved and you must untick the box, correct the error, and tick the box again to save the simset.  If you do not tick Save again, your simset will be saved only in temporary draft form and not to the permanent simset list. You can open up the draft and save the simset by restarting SimBuilder(), but the draft will be overwritten if you open other simsets in the interim.</p><p>After you have saved the simset, you must untick the Save Sim box to continue to edit new simsets."),
                 HTML("<p>In some cases, you will get a warning when saving and you must decide whether you want to continue. These cases are:<ul><li>The simset name corresponds to a simset that is already saved. If you want to continue and overwrite the specified simset, enter the requested number in the Confirm Number box and your simset will be saved. You can then untick the Save Sim box and continue to edit new simsets.<li>The simset name corresponds to a simset that is already saved <i>and</i> part of a project. The software will not allow you to make changes to a simset that is already part of a project. You must either agree to delete the project, including any results, or you must untick the Save Sim box and change the name of the simset before saving it.</ul>")
        ),
        tabPanel("All Models",
                 h5("Sim Name and Description:"),
                 HTML("Enter a short name, e.g. S1, and description, e.g. lambda=1.05  phi=0.9  p=0.1. The description will be used when displaying results for multiple simsets.<p>"),
                 h5("Number of time periods:"),
                 HTML("Number of periods from the first survey to the last survey, inclusive.<p>"),
                 HTML("The superpopulation size, N, will cover the period from the first ticked survey to the last ticked survey, regardless of how many time periods are entered here. Time periods must be equally-spaced if using the lambda-POPAN model or the standard POPAN model with single Phi.<p>"),
                 h5("Selecting time periods for surveys:"),
                 HTML("Tick the boxes on the right-hand panel to denote that a survey will take place in the corresponding time period.<p>"),
                 h5("Superpopulation size, N:"),
                 HTML("<ul><li>Enter a number greater than 0 for the superpopulation size.</ul>"),
                 HTML("The superpopulation size is the number of animals available for capture between the first ticked survey and the last ticked survey inclusive. It does <i>not</i> include any animals that were born and died entirely within a survey gap, because these animals were never available for capture.<p>"),
                 h5("Capture probabilities:"),
                 HTML("Capture probabilities can be entered from the Easy-Fill panel following the instructions given. The survey box must be ticked for a capture probability to be entered.
Capture probabilities can also be entered directly into the parameter matrix on the right, for the ticked time periods in which surveys will occur.<p>"),
                 HTML("<ul><li>Enter numbers between 0 and 1 for the capture probabilities.</ul>"),
                 h5(HTML("p<sub>ent</sub> and E(N<sub>t</sub>) columns:")),
                 HTML("For all models, the parameter matrix displays two columns of derived values that are calculated from the other values that you enter. You can not change these values directly. <ul><li>The 'p<sub>ent</sub>' column displays pent values scaled so that they sum to 1, so that you can enter them in the 'Relative p<sub>ent</sub>' column as relative values (e.g. 3, 1, 1, ...) if you wish. <li>The 'E(N<sub>t</sub>)' column shows the expected numbers of animals in the population in each time period, derived from the N<sub>s</sub>, &Phi;, and &lambda; or p<sub>ent</sub> parameters that you have specified. Across survey gaps, the animals already present are allowed to die. However, new animals only enter the population at survey times, because the superpopulation is defined to be the number of animals ever available for capture. Any animals that are born and die within a survey gap are not counted and are not included in the E(N<sub>t</sub>) column.</ul>")
        ),
        
        
        ## --------------------------------------------------------------------------------------------------
        ## LAMBDA-POPAN HELP:
        ## --------------------------------------------------------------------------------------------------
        tabPanel("Lambda-POPAN",
                 h4("Lambda-POPAN models"),
                 HTML("The pent values follow a growth curve determined by the lambda and phi values. Overall growth from one time period to the next is lambda.  For example, if lambda=1.02, then for every 100 animals alive in one time period there are 102 alive in the next, after births and deaths have been accommodated. <br>Survival probability from one time period to the next is phi. <br>Lambda and phi are constant for all time periods. The minimum value of lambda is phi. Time periods are assumed to be equally-spaced because constant survival and growth is applied to each period.<br>"),
                 HTML("<br>When a lambda-POPAN model is selected, the 'Relative p<sub>ent</sub>' and 'p<sub>ent</sub>' columns display the values derived from the supplied values of lambda and phi. You can not change these values except by changing lambda and phi, or selecting different surveys. The values shown are rounded to 3 digits. In the simulation code, the full-precision unrounded values will be used.</p>Because the values shown in the 'Relative p<sub>ent</sub>' column are already scaled to sum to 1, the 'p<sub>ent</sub>' column just repeats the 'Relative p<sub>ent</sub>' column for lambda-POPAN models.</p> If there are gaps between surveys, the pent parameters account for births and deaths that occur during the gaps. Any animals that are born and die within a single survey gap are omitted and are not represented by any of the pent values.</p>"),
                 HTML("<p>Three entry boxes are displayed for lambda-POPAN models:</p>"),
                 HTML("<b>Lambda:</b>"),
                 HTML("<ul><li>Enter a number greater than 0 for lambda. The number must be greater than or equal to the value specified for phi.</ul> "),
                 HTML("If lambda=1 and phi=1, you will be simulating a closed population (equivalent to Model M<sub>t</sub>) in which the superpopulation size is equal to the overall constant population size.</p>"),
                 HTML("<b>Phi:</b>"),
                 HTML("<ul><li>Enter a number between 0 and 1 for phi.</ul>"),
                 HTML("The 'phi' displayed at any time point governs survival <i>from</i> that time <i>to</i> the next time, whether or not surveys occur at these times. All values of phi from the last ticked survey onwards are shown as blank in the matrix, because any survival occurring after the last survey is not included in the simulation process.</p>"),
                 HTML("<b>Start time:</b>"),
                 HTML("<ul><li>Enter the time label for the first survey, e.g. 1 or 2001. Time periods must be equally-spaced for lambda-POPAN models, so time labels are automatically determined from the start time and the number of time periods. The start time should be a number.</ul>"),
                 HTML("In addition, capture probabilities can be entered via the Easy-Fill panel or directly into the matrix. You can only enter capture probabilities into the matrix when the corresponding checkbox is ticked, denoting that a survey will take place in that time period.<p>")
        ),
        
        ## --------------------------------------------------------------------------------------------------
        ## SINGLE-PHI HELP
        ## --------------------------------------------------------------------------------------------------
        tabPanel("Single-Phi POPAN",
                 h4("Standard POPAN with single Phi"),
                 HTML("This option provides a short-cut for models where survival is assumed constant for every time period. Time periods in such models will generally be equally-spaced, so you only have to specify a start time to establish the matrix layout.</p>The single-Phi model allows the pent parameters to be entered as chosen values, rather than constraining them to a growth curve as in the lambda-POPAN model.</p>"),
                 HTML("<p>Two entry boxes are displayed for single-Phi models:</p>"),
                 HTML("<b>Phi:</b>"),
                 HTML("<ul><li>Enter a number between 0 and 1 for phi.</ul>"),
                 HTML("The 'phi' displayed at any time point governs survival <i>from</i> that time <i>to</i> the next time, whether or not surveys occur at these times. All values of phi from the last ticked survey onwards are shown as blank in the matrix, because any survival occurring after the last survey is not included in the simulation process.</p>"),
                 HTML("<b>Start time:</b>"),
                 HTML("<ul><li>Enter the time label for the first survey, e.g. 1 or 2001. Time periods must be equally-spaced for single-Phi models, so time labels are automatically determined from the start time and the number of time periods. The start time should be a number.</ul>"),
                 HTML("In addition, you must select values for capture probabilities and entry proportions. Each of these apply only at times when surveys take place. <ul><li>Capture probabilities can be entered via the Easy-Fill panel or directly into the matrix. <li>Entry proportions can be entered via the Easy-Fill panel or directly into the 'Relative p<sub>ent</sub>' column on the matrix. You can enter them as relative values (not summing to 1) if you wish. The 'p<sub>ent</sub>' column displays the values scaled so that they sum to 1.</ul><p>")
        ),
        
        
        ## --------------------------------------------------------------------------------------------------
        ## STANDARD-POPAN HELP:
        ## --------------------------------------------------------------------------------------------------
        tabPanel("Standard POPAN",
                 h4("Standard-POPAN models"),
                 HTML("This tab offers the most flexible model options. You can have time-varying p, phi, and pent values.  Surveys do not need to be equally-spaced.</p>This is the only model that allows time-varying Phi, so some special comments apply."),
                 h5("Time-varying Phi:"),
                 HTML("The survival rate, phi, can be entered using the Easy-Fill panel or directly into the matrix, like the other parameters.<ul><li>Each phi should be a number between 0 and 1.<li>Each phi corresponds to survival <i>between</i> two time periods. The phi that you enter at time period 1 corresponds to survival <i>between</i> times 1 and 2; and so on for the other times. There is no phi entry from the last ticked survey time onwards, because we do not simulate survival after the end of the surveys.<li>The software expects you to enter phi values for intermediate times even if surveys do not take place, because the process of survival must continue regardless of survey occurrence. If you do not wish to specify phi values in survey gaps, you should redefine your time periods so that you do not have survey gaps. For example, if you have surveys in years 2001, 2003, and 2008, you could set the matrix to have three time periods with labels 2001, 2003, and 2008, and two different phi values: the first (entered under 2001) to control survival between 2001 and 2003; and the second (entered under 2003) to control survival between 2003 and 2008.</ul>"),
                 h5("Other parameters:"),
                 HTML("As usual, you must select values for capture probabilities and entry proportions. Each of these apply only at times when surveys take place. <ul><li>Capture probabilities can be entered via the Easy-Fill panel or directly into the matrix. <li>Entry proportions can be entered via the Easy-Fill panel or directly into the 'Relative p<sub>ent</sub>' column on the matrix. You can enter them as relative values (not summing to 1) if you wish. The 'p<sub>ent</sub>' column displays the values scaled so that they sum to 1.</ul><p>For standard-POPAN models, you have a completely free choice of how you describe your time periods, and whether or not they are equally spaced. These are for your reference only, and will not be used in model fitting.")
        )
      )
    )
  )
}