SimServer <- function(input, output, session, capow_list) {
  # Create namespace for sim builder module
  ns <- NS("SimUI")
  
  # Reactive dataset and fit model selection
  output$fitselection <- renderUI({
    selectInput(
      ns("fitselected"),
      "Choose a dataset and fit model:",
      choices = c("None", naturalsort(names(capow_list()$fit_list()))),
      selected = "None"
    )
  })
  
  # Update the Ns, lambdamodel, lambda, and phi inputs to the appropriate values
  # when a dataset is selected. This isn't working cause the param matrix
  # doesn't update when those inputs are changed, but apparently they are still
  # saved into the simset. Also only implemented assuming a lambda-model and
  # that all other parameters estimated.  See if you can figure out the errors
  # and then maybe try generalising.
  
  # A lot of this computation is repeated in the param matrix, would be nice to
  # make a reactive to do it for both.
  observeEvent(input$fitselected, {
    # fitselected is renderUI input so have to check not NULL first.
    if(!(is.null(input$fitselected) || input$fitselected == "None")) {
      # Get fit results and model
      chosenFit <- capow_list()$fit_list()[[input$fitselected]]
      data_estimates <- chosenFit[["fit"]]
      chosenModel <- capow_list()$model_list()[[chosenFit[["model"]]]]
      data_surv_inds <- chosenModel$chosentimes
      
      # Find lambda and phi as either estimates or constants in fitted model
      if(chosenModel$lambdaparam == "lambda") {
        chosenLambda <- data_estimates["lambda"]
      } else {
        chosenLambda <- as.numeric(chosenModel$lambdaparam)
      }
      if("phi1" %in% names(data_estimates)) {
        chosenPhi <- data_estimates["phi1"]
      } else {
        chosenPhi <- as.numeric(chosenModel$paramdf$survrate[1])
      }
      
      # Get number of time periods covered by existing data
      data_TimeN <- nrow(chosenModel$paramdf)
      
      # Survey occasions and number of time periods
      tempSim = get("tempSim", envir = CPenv)
      timeoptl = try(tempSim$paramdf$timeopt, silent = F)
      timeoptl <- c(chosenModel$paramdf$timeopt, timeoptl)
      
      # Get number of time periods input or NA
      valTimeN = suppressWarnings(as.numeric(input$TimeN))
      
      # If number of time periods NA or smaller than one set to zero
      if(is.na(valTimeN) || valTimeN < 1) {
        valTimeN = 0
      }
      
      # Add number of time periods from data
      valTimeN <- valTimeN + data_TimeN
      
      # Time labels
      first_time_label <- as.numeric(chosenModel$paramdf$timelabels[1])
      valTimeLabels <- first_time_label:(first_time_label + valTimeN - 1)
      
      # Find expected numbers alive in original study according to fit results
      nalive <- nalive.calc.func(
        survs = valTimeLabels[data_surv_inds],
        Ns = data_estimates[1], ## Assuming for now that Ns was estimated 
        lambda = chosenLambda,
        phi = chosenPhi
      )
      
      # Find Ns for extended sim
      Ns_val <- Ns.calc.func(
        nalive.in = nalive[1, 2], 
        year.in = nalive[1, 1], 
        lambda = chosenLambda, 
        phi = chosenPhi, 
        years.out = valTimeLabels[timeoptl]
      )
      
      # Enter values in inputs
      updateTextInput(
        session = session, 
        inputId = "superpopn", 
        value = as.character(format(signif(Ns_val$Ns, digits = 4), nsmall = 1))
      )
      updateRadioButtons(
        session = session,
        inputId = "simtype",
        selected = "lambdasim"
      )
      updateTextInput(
        session = session, 
        inputId = "lambdaparam", 
        value = as.character(format(round(chosenLambda, digits = 4), nsmall = 4))
      )
      updateTextInput(
        session = session, 
        inputId = "survratelambda", 
        value = as.character(format(round(chosenPhi, digits = 4), nsmall = 4))
      )
      updateTextInput(
        session = session, 
        inputId = "startlambda", 
        value = as.character(as.numeric(
          tail(chosenModel$paramdf$timelabels, 1)) + 1)
      )
    }
  })
  
  # Read and update temporal parameter matrix.  Could be trouble reading with a
  # fitted model now because loop over rows of "out" may go from row one to row
  # zero.  Actually seems to be working somehow.  Great.  Dunno if it'll save
  # properly though.  Yeah the save function doesn't consider datasets or fits
  # at all...
  output$simParamUI <- renderUI({
    ## input$TimeN is the number of time periods.  This is given to valTimeN and set to 1 if missing:
    valTimeN = suppressWarnings(as.numeric(input$TimeN))
    # Check if number of time periods NA or smaller than one
    if(is.na(valTimeN) || valTimeN < 1) {
      # If fit results selected set it to zero, otherwise set it to one
      if(!(is.null(input$fitselected) || input$fitselected == "None")) {
        valTimeN = 0
      } else {
        valTimeN = 1
      }
    }
    
    ## This line is needed for opening up the initial sim when first starting the interface.
    ## tempSim will be overwritten later, but this is needed to establish the opening sim.
    if(exists("tempSim", envir = CPenv, inherits = FALSE))
      tempSim = get("tempSim", envir = CPenv)
    
    ##----------------------------------------
    ## Generate "out"
    ##----------------------------------------
    
    ## This takes all the components of "input" defined in ui.R and puts them into a list, called "out":
    out = reactiveValuesToList(input)
    outnames = names(out)
    ## Collect matLay values into data.frame:
    ## ndigits is the number of digits in the number of time periods,
    ## e.g. if there are 10 time periods then ndigits=2,
    ## if there are 100 time periods then ndigits=3:
    ndigits = nchar(valTimeN)
    
    ## makeregexp is a function of column ID (name of the column), and current row (currow):
    ## e.g. if the number of time periods is in double digits (10 to 99: so ndigits=2),
    ## then makeregexp(colIDs[1], 1) = makeregexp("timelabels", 1) = "^timelabels(01)$"
    ## This is then used to check whether the corresponding name appears in outnames.
    ## The ^ and $ are just placemarkers in grep's regular expression matching:
    ## ^ means this is the beginning of the string, and $ means it is the end.  E.g.
    ## grep("hello", "hellogoodbye") = 1
    ## grep("^hello", "hellogoodbye") = 1 because the beginning of the strings match;
    ## grep("^hello$", "hellogoodbye") = integer(0) because there is no match for the entire string
    ## from beginning to end.
    makeregexp = function(ID, currow){
      paste0("^", ID, "(", sprintf(paste0("%0", ndigits, "d"), currow), ")$")
    }
    outdf = NULL
    colIDs = c("timelabels", "timeopt",  "survrate", "capturepr", "prentry", "pentscaled", "Nt")
    
    ## Check if UI has finished loading:
    ## the line below checks whether there is an entry present in "out" with name "timelabels01"
    ## or equivalent:
    # Changed to check for Nt to ensure reloaded after switching from model tab.  This won't work if
    # there are other tabs with "Nt" columns.  Also model tab might not work if before sim tab?
    # Actually interface in conditional panel not working...
    if(length(out[grep(makeregexp(colIDs[7], 1), outnames)]) > 0){
      ## UI has finished loading if we get inside here, so continue:
      ## Loop through each row (time period), and *remake* the output data frame:
      
      # If we are simulating from existing data we need to start at the first time
      # period after it
      if(!(is.null(input$fitselected) || input$fitselected == "None")) {
        chosenFit <- capow_list()$fit_list()[[input$fitselected]]
        chosenModel <- capow_list()$model_list()[[chosenFit[["model"]]]]
        data_TimeN <- nrow(chosenModel$paramdf)
        # Have to check that whether the parameter matrix has updated with the selected model
        if(length(out[grep(makeregexp(colIDs[1], data_TimeN + valTimeN), outnames)]) == 0)
          data_TimeN <- 0
      } else {
        data_TimeN <- 0
      }
      
      # This should be wrong when a fitted dataset is selected because valTimeN
      # should be zero...  Seems like it only comes here when it's NA or zero
      # and then these only apply to after the dataset stuff, and then nothing
      # would be shown anyway...  Not sure what this is supposed to do anyway.
      for(i in (data_TimeN + 1:valTimeN)){
        outrow = NULL
        ## Loop through each column and grab value to append to the current row, outrow:
        for(curID in colIDs)
          outrow = c(outrow, out[grep(makeregexp(curID, i), outnames)])
        if(length(outrow) > 0){
          names(outrow) = colIDs
          ## Bind row values to data.frame outdf:
          outdf = rbind(outdf, as.data.frame(outrow, stringsAsFactors=F))
        }
      }

      ## We've now rebuilt the output data frame and called it outdf.

      ## Collect remaining and do some computation
      chosentimes = which(outdf$timeopt)

      ## Only report a lambda value if it's a lambda-type sim:
      if(out$simtype=="lambdasim") lambdareport <- out$lambdaparam
      else lambdareport <- ""

      # Changed input names to have Sim prefix
      tempSim = list(simname = out$SimName,
                     description = out$Description,
                     fit = input$fitselected,
                     chosentimes = chosentimes,
                     gapvec = diff(chosentimes),
                     superpopn = out$superpopn,
                     simtype = out$simtype, ## All of these above here come from data frame out
                     lambdaparam = lambdareport,
                     paramdf = outdf)   ## paramdf comes from "outdf" which has just been rebuilt
      assign("tempSim", tempSim, envir = CPenv)   ## Whole lot is now assigned to CPenv
    }
    
    ## -----------------------------------------------------------------------------------------------------
    ## CREATE THE MATRIX PANEL DISPLAY
    ## -----------------------------------------------------------------------------------------------------
    
    ## timeoptl = vector of True / False saying whether each time period is selected for a survey:
    ## First try to get it from tempSim:
    timeoptl = try(tempSim$paramdf$timeopt, silent = TRUE)
    
    ## If that doesn't work or is the wrong length, replace it with FALSE everywhere:
    if(class(timeoptl) == "try-error" || length(timeoptl) != valTimeN) timeoptl = rep(FALSE, valTimeN)
    
    ## -----------------------------------------------------------------------------------------------------
    ## TIME LABELS
    ## -----------------------------------------------------------------------------------------------------
    ## Go through the sim types: lambda-POPAN, standard-POPAN with single phi, and full POPAN:
    if(input$simtype=="lambdasim"){
      ## Time labels have to start at startTime and finish at startTime + valTimeN - 1:
      if(input$startlambda=="" | is.na(as.numeric(input$startlambda)))
        valTimeLabels <- seq(1,  valTimeN)
      else
        valTimeLabels <- seq(as.numeric(input$startlambda),
                             as.numeric(input$startlambda) + valTimeN - 1)
      ## All time labels boxes are disabled on the matrix for lambda sims:
      TLdisabled <- rep(T, valTimeN)
    }
    ## ------------------------------------------------------------------------------------------------
    else if(input$simtype=="singlephisim"){
      ## Time labels have to start at startTime and finish at startTime + valTimeN - 1:
      if(input$startsingle =="" | is.na(as.numeric(input$startsingle)))
        valTimeLabels <- seq(1,  valTimeN)
      else
        valTimeLabels <- seq(as.numeric(input$startsingle),
                             as.numeric(input$startsingle) + valTimeN - 1)
      ## All time labels boxes are disabled on the matrix for single-phi sims:
      TLdisabled <- rep(T, valTimeN)
    }
    ## ------------------------------------------------------------------------------------------------
    else if(input$simtype=="fullsim"){
      ## EasyFill Time Labels
      valTimeLabels = try(efeval(input$efTimeLabels, valTimeN), silent = TRUE)
      if(class(valTimeLabels) == "try-error") valTimeLabels = "error"
      
      ## No time labels boxes are disabled on the matrix for full sims:
      TLdisabled <- rep(F, valTimeN)
    }
    
    ## -----------------------------------------------------------------------------------------------------
    ## SURVIVAL
    ## -----------------------------------------------------------------------------------------------------
    ## Go through the sim types: lambda-POPAN, standard-POPAN with single phi, and full POPAN:
    
    if(input$simtype=="lambdasim"){
      ## Survival rate has to be survratelambda:
      valphi <- rep(input$survratelambda, valTimeN)
      ## Any phi's strictly before the first ticked survey are blank:
      if(any(timeoptl)) if(min(which(timeoptl))>1) valphi[1:(min(which(timeoptl))-1)] <- ""
      ## All phi's from the last ticked survey to the end, inclusive, are blank:
      ## in particular, the phi *at* the last ticked survey is blank:
      if(any(timeoptl)) valphi[max(which(timeoptl)):valTimeN] <- ""
      ## All survival-rate boxes are disabled on the matrix for lambda sims:
      phidisabled <- rep(T, valTimeN)
    }
    ## ------------------------------------------------------------------------------------------------
    else if(input$simtype=="singlephisim"){
      ## Survival rate has to be survratesingle:
      valphi <- rep(input$survratesingle, valTimeN)
      ## Any phi's strictly before the first ticked survey are blank:
      if(any(timeoptl)) if(min(which(timeoptl))>1) valphi[1:(min(which(timeoptl))-1)] <- ""
      ## All phi's from the last ticked survey to the end, inclusive, are blank:
      ## in particular, the phi *at* the last ticked survey is blank:
      if(any(timeoptl)) valphi[max(which(timeoptl)):valTimeN] <- ""
      ## All survival-rate boxes are disabled on the matrix for single-phi sims:
      phidisabled <- rep(T, valTimeN)
    }
    ## ------------------------------------------------------------------------------------------------
    else if(input$simtype=="fullsim"){
      ## EasyFill for survival:
      valphi = try(efeval(input$efsurvrate, valTimeN), silent = TRUE)
      ## EasyFill gives an error:
      if(class(valphi) == "try-error") valphi = "error"
      
      ## Any phi's strictly before the first ticked survey are blank:
      if(any(timeoptl)) if(min(which(timeoptl))>1) valphi[1:(min(which(timeoptl))-1)] <- ""
      ## All values of phi from the last ticked survey to the end (inclusive) must be blank:
      ## in particular, the phi *at* the last ticked survey is blank:
      if(any(timeoptl)) valphi[max(which(timeoptl)):valTimeN] <- ""
      ## Disable the values before the first ticked survey, and from the last ticked
      ## survey to the end (inclusive):
      phidisabled <- rep(F, valTimeN)
      if(any(timeoptl)){
        if(min(which(timeoptl))>1) phidisabled[1:(min(which(timeoptl))-1)] <- T
        phidisabled[max(which(timeoptl)):valTimeN] <- T
      }
    }
    
    ## -----------------------------------------------------------------------------------------------------
    ## CAPTURE PROBABILITIES
    ## -----------------------------------------------------------------------------------------------------
    ## EasyFill for capture probabilities
    if(input$simtype=="lambdasim")
      valp = try(efeval(input$efplambda, valTimeN), silent = TRUE)
    else if(input$simtype=="singlephisim")
      valp = try(efeval(input$efpsingle, valTimeN), silent = TRUE)
    else if(input$simtype=="fullsim")
      valp = try(efeval(input$efpfull, valTimeN), silent = TRUE)
    ## If the relevant one failed, return an error:
    if(class(valp) == "try-error") valp = "error"
    valp <- as.character(valp)
    
    ## Work out which p boxes are disabled and set them to blank:
    
    ## pdisabled is the opposite of timeoptl: it is true when there is NO survey, false when there IS a survey:
    pdisabled <- !timeoptl
    ## Reset disabled capture probabilities to blank:
    valp_temp <- rep("", valTimeN)
    valp_temp[timeoptl] <- valp[1:sum(timeoptl)]
    valp <- valp_temp
    
    ## -----------------------------------------------------------------------------------------------------
    ## ENTRY PROBABILITIES
    ## -----------------------------------------------------------------------------------------------------
    ## Go through the sim types: lambda-POPAN, standard-POPAN with single phi, and full POPAN:
    
    if(input$simtype=="lambdasim"){
      ## If it's a lambda survey, the pent values are calculated from lambda and phi:
      ## We will accommodate the survey gaps, so the pent parameters change according to
      ## which surveys are selected.  Purpose of doing this is that users might want to compare
      ## the fit using a standard POPAN model with what the right answers are under the lambda model:
      ## this will show them the right answers.
      ##
      ## Thus pent accumulates entries where there is a gap, e.g. if the survey years are
      ## c(1, 2, 3, 8, 9, 10) then pent[4], corresponding to year 8, will accumulate the entries from years
      ## 4, 5, 6, 7, and 8.
      
      ## If either lambda or phi is not a number, return blank for valpent:
      if(is.na(suppressWarnings(as.numeric(input$survratelambda))) |
         is.na(suppressWarnings(as.numeric(input$lambdaparam))))
        valpent <- rep("", valTimeN)   ##
      ## Else, if there are any ticked surveys, calculate pent:
      else if(any(timeoptl)) {
        ## Retrieve the values of phi and lambda from the correct input boxes:
        phival <- as.numeric(input$survratelambda)
        lambdaval <- as.numeric(input$lambdaparam)
        
        ## Find the first ticked survey:
        survvec <- which(timeoptl)
        firstsurv <- min(survvec)
        nsurv <- length(survvec)
        ## Set the pent for the first ticked survey equal to 1, temporarily:
        valpent <- rep(0, valTimeN)
        valpent[firstsurv] <- 1
        
        ## If there are any more ticked surveys, find their pent's relative to the first survey:
        if(nsurv>1){
          gapvec <- diff(survvec)
          cumvec <- c(0, cumsum(gapvec))
          for(t in 2:nsurv){
            valpent[survvec[t]] <- (lambdaval - phival) *
              sum(phival^(0:(gapvec[t-1]-1)) * lambdaval^((cumvec[t]-1):cumvec[t-1]))
          }
          ## Rescale all the pents to add to 1: note that only rounded values are displayed and
          ## returned. Simulation code will calculate the *unrounded* values directly from lambda
          ## and phi.
          valpent <- format(round(valpent/sum(valpent), digits=4), nsmall=4)
        }  ## End of more than one ticked survey
        else valpent <- as.character(valpent)
        
        ## Reset to blank any pents that correspond to no survey:
        valpent[!timeoptl] <- ""
        
      }  ## End of any ticked surveys
      else valpent <- rep("", valTimeN)   ## Case for no ticked surveys.
      
      ## All pent boxes are disabled on the matrix for lambda sims:
      pentdisabled <- rep(T, valTimeN)
    }
    ## ------------------------------------------------------------------------------------------------
    else if(input$simtype=="singlephisim" | input$simtype=="fullsim"){
      ## EasyFill is checked for entry probabilities:
      if(input$simtype=="singlephisim")
        valpent = try(efeval(input$efpentsingle, valTimeN), silent = TRUE)
      else if(input$simtype=="fullsim")
        valpent = try(efeval(input$efpentfull, valTimeN), silent = TRUE)
      ## EasyFill gives an error:
      if(class(valpent) == "try-error") valpent = "error"
      
      ## The pents are disabled if the p's are disabled, and also the pent corresponding to the first survey
      ## must be disabled:
      pentdisabled <- pdisabled
      ## Replace any disabled pents with blanks:
      valpent_temp <- rep("", valTimeN)
      valpent_temp[timeoptl] <- valpent[1:sum(timeoptl)]
      valpent <- valpent_temp
    }
    
    ## -----------------------------------------------------------------------------------------------------
    ## DERIVE pentScaled and Nt VALUES
    ## -----------------------------------------------------------------------------------------------------
    ## These are calculated in the same way for all models, using the pent and phi values.
    ## N1 = pent[1] * Ns
    ## Nt = phi * N[t-1] + pent[t] * Ns
    ## If there are survey gaps, we only report Nt for the same times as we report pent.
    ## Survival can carry on throughout the gaps, but only applies to the animals alive at the
    ## start of the gap.  The pent parameters in the lambda model accommodate new
    ## births from throughout the gap (and omit any births that died during the gap).
    ## Thus if we have, say, N[t], gap, gap, N[t+3]
    ## then we want N[t+3] = phi^3 N[t] + pent[t+3]*Ns.
    
    valNt <- rep(0, valTimeN)
    pentScaled <- rep("", valTimeN)
    if(any(timeoptl)) {
      survtimes <- which(timeoptl)
      firstsurvtime <- min(survtimes)
      lastsurvtime <- max(survtimes)
      ## Scale the pent's to sum to 1 in case they have been entered differently.  (Leave them unchanged
      ## for lambda models.)
      ## Note that pentScaled has NA's in non-survey times or times without a numeric valpent.
      if(input$simtype=="lambdasim") pentScaled <- suppressWarnings(as.numeric(valpent))
      else{
        pentvals <- suppressWarnings(as.numeric(valpent))
        pentScaled <- pentvals/sum(pentvals[survtimes])
      }
      ## Nt for the first survey is just Ns * pent[firstsurv]:
      Nsval <- suppressWarnings(as.numeric(input$superpopn))
      valNt[firstsurvtime] <- Nsval * pentScaled[firstsurvtime]
      
      ## If there are any more surveys, fill in the valNt values:
      if(lastsurvtime > firstsurvtime){
        test.phi <- suppressWarnings(as.numeric(valphi[firstsurvtime:(lastsurvtime-1)]))
        if(any(is.na(test.phi))) valNt <- rep(NA, valTimeN)
        else{
          ## Use all.phi to get the indices correct: it covers all time periods,
          ## not just firstsurv to lastsurv-1:
          all.phi <- suppressWarnings(as.numeric(valphi))
          for(t in firstsurvtime:(lastsurvtime-1)){
            valNt[t+1] <- valNt[t] * all.phi[t]
            ## If there is a survey at time t+1, add Ns * pent[t+1]:
            if(timeoptl[t+1])
              valNt[t+1] <- valNt[t+1] + Nsval * pentScaled[t+1]
          }
        }
        ## If there are any Nt missing, make them all missing; otherwise round to nearest integer:
        if(any(is.na(valNt))) valNt <- rep("", valTimeN)
        else valNt <- as.character(round(valNt))
        ## If there are any pentScaled missing in survey times, make them all missing;
        ## otherwise round to nearest 3dp:
        if(any(is.na(pentScaled[survtimes]))) pentScaled <- rep("", valTimeN)
        else pentScaled <- format(round(pentScaled, digits=4), nsmall=4)
      }
      ## Set valNt and pentScaled to blank whenever there is no survey, so we don't confuse
      ## the survey gaps by displaying deaths but not births:
      valNt[!timeoptl] <- ""
      pentScaled[!timeoptl] <- ""
    }  ## case where there are any surveys selected
    else{
      valNt <- rep("", valTimeN)
    }
    
    ## All Nt boxes are disabled: they give reported values only.  pentScaled will use the same disabling.
    Ntdisabled <- rep(T, valTimeN)
    
    ## -----------------------------------------------------------------------------------------------------
    # Simulating with existing data 
    ## -----------------------------------------------------------------------------------------------------
    
    # If model fit to existing dataset selected, find and add relevant parameter
    # values to be included as disabled inputs at top of parameter matrix.
    
    # Assuming, for now, that in the model fit to existing data, lambda and phi
    # are either estimated or held constant, but all other parameters are
    # estimated.
    
    # fitselected is renderUI input so have to check not NULL first.
    if(!(is.null(input$fitselected) || input$fitselected == "None")) {
      # Get fit results and model
      chosenFit <- capow_list()$fit_list()[[input$fitselected]]
      data_estimates <- chosenFit[["fit"]]
      chosenModel <- capow_list()$model_list()[[chosenFit[["model"]]]]
      data_surv_inds <- chosenModel$chosentimes
      
      # Find lambda and as either estimates or constants in fitted model
      if(chosenModel$lambdaparam == "lambda") {
        chosenLambda <- data_estimates["lambda"]
      } else {
        chosenLambda <- as.numeric(chosenModel$lambdaparam)
      }
      if("phi1" %in% names(data_estimates)) {
        chosenPhi <- data_estimates["phi1"]
      } else {
        chosenPhi <- as.numeric(chosenModel$paramdf$survrate[1])
      }
      
      # Get number of time periods covered by existing data
      data_TimeN <- nrow(chosenModel$paramdf)
      
      # Survey occasions and number of time periods
      timeoptl <- c(chosenModel$paramdf$timeopt, timeoptl)
      valTimeN <- valTimeN + data_TimeN
      
      # Time labels
      first_time_label <- as.numeric(chosenModel$paramdf$timelabels[1])
      valTimeLabels <- first_time_label:(first_time_label + valTimeN - 1)
      
      # Survival probabilities
      
      # The first one in the existing vector will be blank so just remake whole
      # thing here
      valphi <- rep("", valTimeN)
      surv_inds <- which(timeoptl)
      valphi[surv_inds[1] : (tail(surv_inds, 1) - 1)] <- format(
        round(chosenPhi, digits = 4), 
        nsmall = 4
      )
      
      # Capture probabilities
      
      # I should allow for these also to have been estimated or held constant -
      # should do for all eventually.  Use match paste "p" data_surv_inds too
      valp <- c(rep("", data_TimeN), valp)
      first_var_index <- grep("var", names(data_estimates))[1]
      valp[data_surv_inds] <- format(
        round(
          data_estimates[(first_var_index - length(data_surv_inds)):(first_var_index - 1)], 
          digits=4
        ), 
        nsmall=4
      )
      
      # Entry proportions and population size
      
      # Use Ns, phi, and pent/lambda, from model fitted to existing data, to get
      # N_1.
      
      # nalive.calc.func comes from Rfunc.R in Rachel's Maui work.  It returns
      # all Nts, but finds pent accounting for survey gaps, so not worth more
      # code.
      nalive <- nalive.calc.func(
        survs = valTimeLabels[data_surv_inds],
        Ns = data_estimates[1], ## Assuming for now that Ns was estimated 
        lambda = chosenLambda,
        phi = chosenPhi
      )
      
      # Find Ns including new surveys for sim.  
      # Should set input to this value and disable it when I get time?
      # For now, printing it might help?
      Ns_val <- Ns.calc.func(
        nalive.in = nalive[1, 2], 
        year.in = nalive[1, 1], 
        lambda = chosenLambda, 
        phi = chosenPhi, 
        years.out = valTimeLabels[timeoptl]
      )
      # Leave this printing so you can put the right value in for the sim.
      # Remember the param matrix is being built based on the estimated Ns for
      # the data, but the sim builder still uses the value input so you have to
      # enter it for now.  Definitely need to make it update automatically
      # though, right?  And definitely disable them (phi n lambda too) since it
      # really doesn't make sense to simulate from existing data with param
      # values that haven't been derived from it, either by direct estimation,
      # or by informing other values by being held constant in a model fit.
      # print("Ns_val")
      # print(Ns_val)
      # print("after Nsval")
      
      # Get all Nts and pents
      nalive <- nalive.calc.func(
        survs = valTimeLabels[timeoptl],
        Ns = Ns_val$Ns,
        lambda = chosenLambda,
        phi = chosenPhi
      )
      
      # Place into display vectors at survey occasions
      valNt <- valpent <- rep("", valTimeN)
      # I dunno how this was working before, maybe I just didn't check???
      # valNt[timeoptl] <- round(nalive[timeoptl, 2])
      # valpent[timeoptl] <- format(round(nalive[timeoptl, 3], digits = 4), nsmall = 4)
      valNt[timeoptl] <- round(nalive[, 2])
      valpent[timeoptl] <- format(round(nalive[, 3], digits = 4), nsmall = 4)
      pentScaled <- valpent
      # print("after Nts")
      # Make all parameter inputs for existing data disabled
      data_disabled <- rep(T, data_TimeN)
      TLdisabled <- c(data_disabled, TLdisabled)
      phidisabled <- c(data_disabled, phidisabled)
      pdisabled <- c(data_disabled, pdisabled)
      pentdisabled <- c(data_disabled, pentdisabled)
      Ntdisabled <- c(data_disabled, Ntdisabled)
    }
    
    ## -----------------------------------------------------------------------------------------------------
    ## OUTPUT TABLE
    ## -----------------------------------------------------------------------------------------------------
    ## colArgs gives the output table:
    colArgs = list(
      list(type = "text", style = "width:3em", value = valTimeLabels, disabled=TLdisabled),
      list(type = "checkbox", checked = timeoptl),
      list(type = "text", style = "width:4em", value = valphi, disabled = phidisabled),
      list(type = "text", style = "width:4em", value = valp, disabled = pdisabled),
      list(type = "text", style = "width:4em", value = valpent, disabled = pentdisabled),
      list(type = "text", style = "width:4em", value = pentScaled, disabled = Ntdisabled),
      list(type = "text", style = "width:3em", value = valNt, disabled = Ntdisabled)
    )
    
    # Defines namespace for Sim Builder module to use for output ids below
    ns <- NS("SimUI")
    
    ## Creates a matrix layout that includes Shiny objects for the final output displayed:
    matLayAc(colTypes = rep("extInput", 7),
             colIDs = c(ns("timelabels"), ns("timeopt"), ns("survrate"), ns("capturepr"), ns("prentry"), ns("pentscaled"), ns("Nt")),
             colArgs = colArgs,
             colNames = c("Time", "", "Survival &Phi;", "Capture p",
                          "Relative p<sub>ent</sub>", "p<sub>ent</sub>", "E(N<sub>t</sub>)"),
             nrow = valTimeN,
             print.rownames=T)
  })
  
  # Try to save sim when button clicked
  savemessage <- reactiveVal(c("", ""))
  observeEvent(input$saveSim, savemessage({ 
    save.msg <- c("", "")
    
    ## input$TimeN is the number of time periods.  This is given value valTimeN and set to 1 if missing:
    # Also set to 1 if < 1
    valTimeN = suppressWarnings(as.numeric(input$TimeN))
    if(is.na(valTimeN) || valTimeN < 1) valTimeN = 1
    
    # Add the number of time periods of selected fit results
    if(!(is.null(input$fitselected) || input$fitselected == "None")) {
      chosenFit <- capow_list()$fit_list()[[input$fitselected]]
      chosenModel <- capow_list()$model_list()[[chosenFit[["model"]]]]
      valTimeN <- valTimeN + nrow(chosenModel$paramdf)
    }
    
    ##----------------------------------------
    ## Generate "out"
    ##----------------------------------------
    ## This line is needed for opening up the initial sim when first starting the interface.
    ## tempSim will be overwritten later, but this is needed to establish the opening sim.
    if(exists("tempSim", envir = CPenv, inherits = FALSE))
      tempSim = get("tempSim", envir = CPenv)
    
    
    ## This takes all the components of "input" defined in ui.R and puts them into a list, called "out":
    out = reactiveValuesToList(input)
    outnames = names(out)
    # print("input changed in sim func")
    # print(valTimeN)
    ## Collect matLay values into data.frame:
    ## ndigits is the number of digits in the number of time periods,
    ## e.g. if there are 10 time periods then ndigits=2,
    ## if there are 100 time periods then ndigits=3:
    ndigits = nchar(valTimeN)
    
    ## makeregexp is a function of column ID (name of the column), and current row (currow):
    ## e.g. if the number of time periods is in double digits (10 to 99: so ndigits=2),
    ## then makeregexp(colIDs[1], 1) = makeregexp("timelabels", 1) = "^timelabels(01)$"
    ## This is then used to check whether the corresponding name appears in outnames.
    ## The ^ and $ are just placemarkers in grep's regular expression matching:
    ## ^ means this is the beginning of the string, and $ means it is the end.  E.g.
    ## grep("hello", "hellogoodbye") = 1
    ## grep("^hello", "hellogoodbye") = 1 because the beginning of the strings match;
    ## grep("^hello$", "hellogoodbye") = integer(0) because there is no match for the entire string
    ## from beginning to end.
    makeregexp = function(ID, currow){
      paste0("^", ID, "(", sprintf(paste0("%0", ndigits, "d"), currow), ")$")
    }
    outdf = NULL
    colIDs = c("timelabels", "timeopt",  "survrate", "capturepr", "prentry", "pentscaled", "Nt")
    
    ## Check if UI has finished loading:
    ## the line below checks whether there is an entry present in "out" with name "timelabels01"
    ## or equivalent:
    # Changed to check for Nt to ensure reloaded after switching from model tab.  This won't work if
    # there are other tabs with "Nt" columns.  Also model tab might not work if before sim tab?
    # Actually interface in conditional panel not working...
    if(length(out[grep(makeregexp(colIDs[7], 1), outnames)]) > 0){
      ## UI has finished loading if we get inside here, so continue:
      ## Loop through each row (time period), and *remake* the output data frame:
      for(i in 1:valTimeN){
        outrow = NULL
        ## Loop through each column and grab value to append to the current row, outrow:
        for(curID in colIDs)
          outrow = c(outrow, out[grep(makeregexp(curID, i), outnames)])
        if(length(outrow) > 0){
          names(outrow) = colIDs
          ## Bind row values to data.frame outdf:
          outdf = rbind(outdf, as.data.frame(outrow, stringsAsFactors=F))
        }
      }
      
      ## We've now rebuilt the output data frame and called it outdf.
      
      ## Collect remaining and do some computation
      chosentimes = which(outdf$timeopt)
      ## Only report a lambda value if it's a lambda-type sim:
      if(out$simtype=="lambdasim") lambdareport <- out$lambdaparam
      else lambdareport <- ""
      
      # Changed input names to have Sim prefix
      tempSim = list(simname = out$SimName,
                     description = out$Description,
                     fit = input$fitselected,
                     chosentimes = chosentimes,
                     gapvec = diff(chosentimes),
                     superpopn = out$superpopn,
                     simtype = out$simtype, ## All of these above here come from data frame out
                     lambdaparam = lambdareport,
                     paramdf = outdf)   ## paramdf comes from "outdf" which has just been rebuilt
      assign("tempSim", tempSim, envir = CPenv)   ## Whole lot is now assigned to CPenv
      
      ## --------------------------------------------------------------------------------------------------
      ## SAVING SIM:
      ## --------------------------------------------------------------------------------------------------
      ## ----------------------------------------------------------------------------------------
      ## Check sim:
      ## ----------------------------------------------------------------------------------------
      ## checkSim.func checks the sim format. Further checks for
      ## the sim already existing follow below.
      ## Applying checkSim.func returns T or F for "checkOK", and the appropriate
      ## message to print if an error has been found:
      checkResult <- checkSim.func(sim=tempSim)
      
      simCheckOK <- checkResult$checkOK
      save.msg <- checkResult$msg
      ## If the sim is correct, proceed to the next step: check whether there is already a saved sim
      ## with that name.
      ## If there is, prompt whether to continue.
      ## If there isn't, continue straight on to save the sim.
      ##
      ## If the sim is not correct, this block of code below will be skipped and the error message
      ## will be returned at the end of this function and printed by the output$saveMessage routine.
      if(simCheckOK){
        ## Check whether there is already a sim saved with this name in SimList:
        currentSimList <- get("SimList", envir=CPenv)
        if(any(names(currentSimList)==input$SimName)){
          ## --------------------------------------------------------------------------------------------
          ## SIM ALREADY EXISTS:
          ## --------------------------------------------------------------------------------------------
          ## If there is already a sim saved with the same name,
          ## retrieve the current random-number code to confirm save.
          ## If it doesn't exist, select a new random number:
          if(exists("randomNumber", envir=CPenv)) randno <-
              get("randomNumber", envir=CPenv)
          else{
            randno <- sample(1:100, 1)
            ## Put the new random number into CPenv,
            ## or else this will go into an endless loop:
            assign("randomNumber", randno, envir=CPenv)
          }
          ## Check whether the sim is also part of an existing project:
          currentProjectList <- get("ProjectList", envir=CPenv)
          if(length(currentProjectList)>0){
            projUsingSim <- NULL
            for (proj in names(currentProjectList))
              ## Add any projects that include this sim to the vector projUsingSim:
              if(currentProjectList[[proj]]["simset"]==input$SimName)
                projUsingSim <- c(projUsingSim, proj)
          }
          else projUsingSim <- NULL
          
          # Render "continueSave" box
          output$savesimclicked <- renderUI({
            ns <- NS("SimUI")
            numericInput(ns("continueSave"), "Confirm number:", NA)
          })
          # If input from "continueSave" box has not been recognised yet
          if(is.null(input$continueSave)) confirm <- F
          else {
            ## If there is no entry in the "continueSave" box, do not continue:
            if(is.na(input$continueSave)) confirm <- F
            ## If there is an entry in the "continueSave" box,
            ## verify that it is the correct number code:
            else confirm <- (input$continueSave==randno)
          }
          ## If the correct number code has been entered, confirm=T, otherwise confirm=F.
          if(confirm){
            ## The number matches: continue to save the sim.
            if(!is.null(projUsingSim)){
              ## The sim is part of existing projects. These must be removed,
              ## and all their results also removed.
              for(pr in projUsingSim) {
                resultname <- paste("result.", pr, sep="")
                ## Remove results:
                if(exists(resultname, envir=CPenv)) rm(list=resultname, envir=CPenv)
                ## Remove project from project list:
                currentProjectList[[pr]] <- NULL
              }
              ## If all projects have been removed, replace currentProjectList with list():
              if(length(currentProjectList)==0) currentProjectList <- list()
              ## Replace the Project List in CPenv with the reduced one:
              assign("ProjectList", currentProjectList, envir=CPenv)
              capow_list()$project_list(currentProjectList)
              
              ## Report back:
              save.msg <- c(paste("Project(s) ", paste0(projUsingSim, collapse=", "), " have been removed with all their results, and the sim has been saved.", sep=""), "")
              
            }
            else{
              ## The sim is not part of existing projects.
              save.msg <- c("SimSet has been saved.", "")
            }
            
            ## Save the sim:
            currentSimList[[input$SimName]] <- tempSim
            assign("SimList", currentSimList, envir=CPenv)
            capow_list()$sim_list(currentSimList)
            
            ## Create a new random number that will be used for the next save:
            new.randno <- sample(1:100, 1)
            ## Make sure it isn't the same as the previous number.  If it is, arbitrarily add 3.
            if(new.randno==randno) new.randno <- new.randno + 3
            ## Put the new random number in CPenv for retrieval when it is next needed:
            assign("randomNumber", new.randno, envir=CPenv)
          }
          else{
            ## The confirm number doesn't match.
            ## This is either because it hasn't yet been entered
            ## for this save iteration; or because the wrong
            ## number was entered for this save iteration.
            ## Display the message showing which number must be displayed to continue,
            ## or alternatively escape from this screen by unticking the Save Sim box:
            if(!is.null(projUsingSim))
              ## The sim is part of existing projects:
              save.msg <- c(paste("A sim with this name already exists, and is used by project(s) "
                                  , paste0(projUsingSim, collapse=", "), ". If you continue, these projects and any results will be removed. If you are sure you want to overwrite the existing sim and remove the projects, enter the number ", randno, " in the box below and click the Save Sim button again to confirm.", sep=""), "Alternatively, make changes or save the sim under a different name.")
            
            ## The sim is not part of existing projects:
            else save.msg <- c(paste("A sim with this name already exists. If you want to overwrite the existing sim, enter the number", randno, "in the box below and click the Save Sim button again to confirm."), "Alternatively, make changes or save the sim under a different name.")
          }
        } ## End of case where there is a sim with the same name already saved.
        
        else{
          ## --------------------------------------------------------------------------------------------
          ## SIM DOES NOT ALREADY EXIST - SAVE IT
          ## --------------------------------------------------------------------------------------------
          ## Sim does not exist previously and there are no errors: go ahead and save.
          currentSimList[[input$SimName]] <- tempSim
          assign("SimList", currentSimList, envir=CPenv)
          capow_list()$sim_list(currentSimList)
          save.msg <- c("SimSet has been saved.", "")
        }
      }  ## End of if(simCheckOK) : the sim checked successfully
    }  ## End of if(UI has finished loading)
    save.msg
  }))
  output$simSaveMessage <- renderText({
    savemessagevec <- savemessage()
    paste("<strong><font color=\"#CC0033\">",
          savemessagevec[1], "</font><font color=\"#339933\">",
          savemessagevec[2], "</font></strong>")
  })
  
  ## Ns CALCULATOR
  output$NsCalc <- renderText({
    baseN <- suppressWarnings(as.numeric(input$baseNt))
    baseT <- suppressWarnings(as.numeric(input$baseTime))
    lam <- suppressWarnings(as.numeric(input$lambdaDesired))
    ph <- suppressWarnings(as.numeric(input$phiDesired))
    start <- suppressWarnings(as.numeric(input$startTimeNs))
    end <- suppressWarnings(as.numeric(input$endTimeNs))
    
    ## If end is less than start, return nothing:
    if(end<start) return("")
    
    ## If anything is missing or non-numeric, return nothing:
    if(any(is.na(c(baseN, baseT, lam, ph, start, end)))) return("")
    
    ## Determine the survey years from the entry:
    if(input$survTimesNs=="ALL" | input$survTimesNs=="all" | input$survTimesNs=="All" |
       input$survTimesNs=="")
      surveytimes <- start:end
    else{
      surveyt <- input$survTimesNs
      ## If there are any dashes, colons, spaces, or commas, make sure they are
      ## not the first or last character entered:
      surveytcheck <- unlist(strsplit(surveyt, split=""))
      if(surveytcheck[1]=="-" | surveytcheck[1]=="," | surveytcheck[1]==":" | surveytcheck[1]==" ")
        return("")
      if(rev(surveytcheck)[1]=="-" | rev(surveytcheck)[1]=="," | rev(surveytcheck)[1]==":" |
         rev(surveytcheck)[1]==" ") return("")
      
      ## Replace any occurrences of "-" with ":"
      surveyt <- gsub("-", ":", surveyt, fixed=T)
      
      survtimestry <- try(parse(text=paste("c(", surveyt, ")")))
      ## If survtimes doesn't make sense, return nothing:
      if(class(survtimestry) == "try-error") return("")
      ## Otherwise create the survey times:
      surveytimes <- eval(survtimestry)
      ## If surveytimes is not monotonically increasing, return nothing:
      if(any(diff(surveytimes)<0)) return("")
    }
    
    ## Calculate Ns and the Nt table:
    Nsres <- Ns.calc.func(nalive.in=baseN, year.in=baseT, lambda=lam, phi=ph, years.out=surveytimes)
    
    Ns <- suppressWarnings(as.numeric(Nsres$Ns))
    if(is.na(Ns)) return("")
    
    ## If Ns has been calculated successfully, start building the output text:
    outtxt <- paste("<strong>Superpopulation size over the designated period: </strong>",
                    round(Ns, 4))
    
    ## Create the output table:
    Ntab <- Nsres$yeartable
    Ntab[, "ENt"] <- format(round(Ntab[, "ENt"], 1), nsmall=2)
    Ntab[, "pent"] <- format(round(Ntab[, "pent"], 3), nsmall=3)
    Ntab[, "cumulativeNs"] <- format(round(Ntab[, "cumulativeNs"], 0), nsmall=0)
    
    ## Top row: headers
    tabstr <- "<p><table style=\"border:solid; border-width:1px; border-color:#BFBFBF\">"
    tabstr <- paste(tabstr,
                    "<tr><th style=\"text-align:center; padding:5px; width:6em;\">Survey year</th>",
                    "<th style=\"text-align:center; padding:5px; width:8em;\">Cumulative N<sub>s</sub></th>",
                    "<th style=\"text-align:center; padding:5px; width:5em;\">p<sub>ent</sub></th>",
                    "<th style=\"text-align:center; padding:5px; width:5em;\">E(N<sub>t</sub>)</th></tr>")
    
    ## Table body:
    for(i in 1:nrow(Ntab)){
      rowstart <- paste("<tr><td style=\"text-align:center; padding:5px;\">")
      rowbody <- paste(Ntab[i,], collapse="</td>  <td style=\"text-align:center; padding:5px;\">")
      rowend <- paste("</td> </tr>")
      rowstr <- paste(rowstart, rowbody, rowend, collapse=" ")
      tabstr <- paste(tabstr, rowstr, collapse=" ")
    }
    tabstr <- paste(tabstr, "</table></p><p style=\"margin-bottom:1.4cm\"></p>", collapse=" ")
    
    outtxt <- paste(outtxt, "</p><p>Table of results at survey times:")
    outtxt <- paste(outtxt, tabstr)
    outtxt
  })
  
  # Detailed reactive display of existing sims
  output$detailedsimlist <- reactive({
    DisplSim.func(
      names(capow_list()$sim_list()),
      capow_list()$sim_list()
    )
  })
  
  # Return reactive list of objects
  capow_list
}