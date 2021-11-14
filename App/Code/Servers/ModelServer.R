ModelServer <- function(input, output, session, capow_list) {
  # Wanna make a reactive for display.matrix that only updates when it needs to and especially not for
  # "survrate", "capturepr", and "prentry", which are updated by renderUI when capture occasions change.
  # This isn't working though, it updates twice when capture occasions change and I don't know why :(
  # Some of these functions communicate through tempModel in CPenv.
  makedisplaymatrix <- function() isolate({
    ## input$TimeN is the number of time periods.  This is given value valTimeN and set to 1 if missing:
    # Changed to ModelTimeN
    valTimeN = as.numeric(input$TimeN)
    if(is.na(valTimeN)) valTimeN = 1
    
    ## This line is needed for opening up the initial model when first starting the interface.
    ## tempModel will be overwritten later, but this is needed to establish the opening model.
    # This is only overwritten with values read from the UI once it has been loaded and had these
    # values written in and possibly updated.
    if(exists("tempModel", envir = CPenv, inherits = FALSE))
      tempModel = get("tempModel", envir = CPenv)
    
    ## -----------------------------------------------------------------------------------------------------
    ## CREATE THE MATRIX PANEL DISPLAY
    ## -----------------------------------------------------------------------------------------------------
    
    ## timeoptl = vector of True / False saying whether each time period is selected for a survey:
    ## First try to get it from tempModel:
    timeoptl = try(tempModel$paramdf$timeopt, silent = TRUE)
    ## If that doesn't work or is the wrong length, replace it with FALSE everywhere:
    if(class(timeoptl) == "try-error" || length(timeoptl) != valTimeN) timeoptl = rep(FALSE, valTimeN)
    
    ## -----------------------------------------------------------------------------------------------------
    ## TIME LABELS
    ## -----------------------------------------------------------------------------------------------------
    ## Go through the model types: lambda-POPAN, standard-POPAN with single phi, and full POPAN:
    if(input$modeltype=="lambdamodel"){
      ## Time labels have to start at startTime and finish at startTime + valTimeN - 1:
      if(input$startlambda=="" | is.na(as.numeric(input$startlambda)))
        valTimeLabels <- seq(1,  valTimeN)
      else
        valTimeLabels <- seq(as.numeric(input$startlambda),
                             as.numeric(input$startlambda) + valTimeN - 1)
      ## All time labels boxes are disabled on the matrix for lambda models:
      TLdisabled <- rep(T, valTimeN)
    }
    ## ------------------------------------------------------------------------------------------------
    else if(input$modeltype=="singlephimodel"){
      ## Time labels have to start at startTime and finish at startTime + valTimeN - 1:
      if(input$startsingle =="" | is.na(as.numeric(input$startsingle)))
        valTimeLabels <- seq(1,  valTimeN)
      else
        valTimeLabels <- seq(as.numeric(input$startsingle),
                             as.numeric(input$startsingle) + valTimeN - 1)
      ## All time labels boxes are disabled on the matrix for single-phi models:
      TLdisabled <- rep(T, valTimeN)
    }
    ## ------------------------------------------------------------------------------------------------
    else if(input$modeltype=="fullmodel"){
      ## - If EasyFill is ticked, try to use that.
      ## - Otherwise try to load previous values.
      ## - If that fails (or length is not right), use failsafe defaults.
      # eftick = try(input$efTimeLabelstick)
      eftick = T
      ## eftick = easy-fill ticked.  Here it's seeing whether the Time Labels checkbox on EasyFill is ticked.
      if(class(eftick) == "try-error") eftick = FALSE
      if(eftick){
        ## Time Labels checkbox on EasyFill is ticked: use EasyFill Time Labels if they are OK:
        valTimeLabels = try(efeval(input$efTimeLabels, valTimeN), silent = TRUE)
        if(class(valTimeLabels) == "try-error") valTimeLabels = "error"
        
      } else{
        ## Time Labels checkbox on EasyFill is not ticked.
        ## Try to get the values from tempModel$paramdf:
        valTimeLabels = try(tempModel$paramdf$timelabels, silent = TRUE)
        ## If tempModel$paramdf gave wrong or incompatible values,
        ## revert to defaults 2000 + (1:#times):
        ## efeval is Jimmy's function to evaluate EasyFill expressions as advertised:
        if(class(valTimeLabels) == "try-error" || length(valTimeLabels) != valTimeN)
          valTimeLabels = efeval("2000 + %t", valTimeN)
      }
      
      ## No time labels boxes are disabled on the matrix for full models:
      TLdisabled <- rep(F, valTimeN)
    }
    
    ## -----------------------------------------------------------------------------------------------------
    ## SURVIVAL
    ## -----------------------------------------------------------------------------------------------------
    ## Go through the model types: lambda-POPAN, standard-POPAN with single phi, and full POPAN:
    
    if(input$modeltype=="lambdamodel"){
      ## Survival rate has to be survratelambda:
      valphi <- rep(input$survratelambda, valTimeN)
      ## Any phi's strictly before the first ticked survey are blank:
      if(any(timeoptl)) if(min(which(timeoptl))>1) valphi[1:(min(which(timeoptl))-1)] <- ""
      ## All phi's from the last ticked survey to the end, inclusive, are blank:
      ## in particular, the phi *at* the last ticked survey is blank:
      if(any(timeoptl)) valphi[max(which(timeoptl)):valTimeN] <- ""
      ## All survival-rate boxes are disabled on the matrix for lambda models:
      phidisabled <- rep(T, valTimeN)
    }
    ## ------------------------------------------------------------------------------------------------
    else if(input$modeltype=="singlephimodel"){
      ## Survival rate has to be survratesingle:
      valphi <- rep(input$survratesingle, valTimeN)
      ## Any phi's strictly before the first ticked survey are blank:
      if(any(timeoptl)) if(min(which(timeoptl))>1) valphi[1:(min(which(timeoptl))-1)] <- ""
      ## All phi's from the last ticked survey to the end, inclusive, are blank:
      ## in particular, the phi *at* the last ticked survey is blank:
      if(any(timeoptl)) valphi[max(which(timeoptl)):valTimeN] <- ""
      ## All survival-rate boxes are disabled on the matrix for single-phi models:
      phidisabled <- rep(T, valTimeN)
    }
    ## ------------------------------------------------------------------------------------------------
    else if(input$modeltype=="fullmodel"){
      
      # eftick = try(input$efsurvratetick)
      # if(class(eftick) == "try-error") eftick = FALSE
      eftick = T
      if(eftick){
        ## EasyFill is checked for survival:
        valphi = try(efeval(input$efsurvrate, valTimeN), silent = TRUE)
        ## EasyFill gives an error:
        if(class(valphi) == "try-error") valphi = "error"
      } else{
        ## EasyFill not checked for survival: use tempModel:
        valphi = try(tempModel$paramdf$survrate, silent = TRUE)
        ## tempModel gives an error or is the wrong length: revert to default of "phi" everywhere:
        if(class(valphi) == "try-error" || length(valphi) != valTimeN)
          valphi = efeval("phi", valTimeN)
      }
      ## Any phi's strictly before the first ticked survey are blank:
      if(any(timeoptl)) if(min(which(timeoptl))>1) valphi[1:(min(which(timeoptl))-1)] <- ""
      ## All values of phi from the last ticked survey to the end (inclusive) must be blank:
      ## in particular, the phi *at* the last ticked survey is blank:
      if(any(timeoptl)) valphi[max(which(timeoptl)):valTimeN] <- ""
      ## Disable the values before the first ticked survey,
      ## and from the last ticked survey to the end (inclusive):
      phidisabled <- rep(F, valTimeN)
      if(any(timeoptl)){
        if(min(which(timeoptl))>1) phidisabled[1:(min(which(timeoptl))-1)] <- T
        phidisabled[max(which(timeoptl)):valTimeN] <- T
      }
    }
    
    ## -----------------------------------------------------------------------------------------------------
    ## CAPTURE PROBABILITIES
    ## -----------------------------------------------------------------------------------------------------
    ## Find whether the relevant conditional-panel EasyFill is ticked:
    if(input$modeltype=="lambdamodel") eftick <- try(input$efpticklambda)
    # else if(input$modeltype=="singlephimodel") eftick <- try(input$efpticksingle)
    # else if(input$modeltype=="fullmodel") eftick <- try(input$efptickfull)
    # if(class(eftick) == "try-error") eftick = FALSE
    eftick = T
    
    if(eftick){
      ## If EasyFill is checked for capture probabilities, try to use it:
      ## pick it out from the relevant conditional-panel EasyFill:
      if(input$modeltype=="lambdamodel")
        valp = try(efeval(input$efplambda, valTimeN), silent = TRUE)
      else if(input$modeltype=="singlephimodel")
        valp = try(efeval(input$efpsingle, valTimeN), silent = TRUE)
      else if(input$modeltype=="fullmodel")
        valp = try(efeval(input$efpfull, valTimeN), silent = TRUE)
      ## If the relevant one failed, return an error:
      if(class(valp) == "try-error") valp = "error"
      valp <- as.character(valp)
    } else{
      ## If EasyFill not checked for capture probabilities, try to use the tempModel values,
      ## but fill in the defaults for anything that is blank in the tempModel - if these are still
      ## meant to be disabled they will be overwritten below.
      valp = try(tempModel$paramdf$capturepr, silent = TRUE)
      ## If the tempModel values don't work or are the wrong length, use defaults p1, ..., pk:
      if(class(valp) == "try-error" || length(valp) != valTimeN)
        valp = efeval("p(t)", valTimeN)
      ## Anything in valp that is blank is replaced by defaults: this is included in case
      ## the survey box has only just been enabled.
      valp <- as.character(valp)
      blankp.which <- which(valp=="")
      if(length(blankp.which)>0) valp[blankp.which] <- paste("p", blankp.which, sep="")
    }
    
    ## Work out which p boxes are disabled and set them to blank:
    
    ## pdisabled is the opposite of timeoptl: it is true when there is NO survey, false when there IS a survey:
    ## pdisabled <-  as.logical(abs(timeoptl - 1))
    pdisabled <-  (!timeoptl)
    ## Reset disabled capture probabilities to blank:
    valp[pdisabled] <- ""
    
    ## -----------------------------------------------------------------------------------------------------
    ## ENTRY PROBABILITIES
    ## -----------------------------------------------------------------------------------------------------
    ## Go through the model types: lambda-POPAN, standard-POPAN with single phi, and full POPAN:
    
    if(input$modeltype=="lambdamodel"){
      ## If it's a lambda survey, the pent values are "calculated" to indicate they are
      ## calculated from lambda and phi:
      valpent <- rep("calculated", valTimeN)
      ## If any surveys are ticked, then ONLY these surveys should have a pent="calculated" entry.
      ## This matches the behaviour for phi: all entries display "phi" if there are no surveys ticked,
      ## but as soon as any surveys are ticked only the relevant entries are displayed.
      if(any(timeoptl)) valpent[!timeoptl] <- ""
      ## All pent boxes are disabled on the matrix for lambda models:
      pentdisabled <- rep(T, valTimeN)
    }
    ## ------------------------------------------------------------------------------------------------
    else if(input$modeltype=="singlephimodel" | input$modeltype=="fullmodel"){
      ## For the pent-based models, find whether EasyFill pent is ticked:
      # if(input$modeltype=="singlephimodel") eftick <- try(input$efpentticksingle)
      # else if(input$modeltype=="fullmodel") eftick <- try(input$efpenttickfull)
      # if(class(eftick) == "try-error") eftick = FALSE
      eftick = T
      
      if(eftick){
        ## EasyFill is checked for entry probabilities:
        if(input$modeltype=="singlephimodel")
          valpent = try(efeval(input$efpentsingle, valTimeN), silent = TRUE)
        else if(input$modeltype=="fullmodel")
          valpent = try(efeval(input$efpentfull, valTimeN), silent = TRUE)
        ## EasyFill gives an error:
        if(class(valpent) == "try-error") valpent = "error"
        
        valpent <- as.character(valpent)
      } else{
        ## EasyFill is not checked for entry probabilities: try to get their values from tempModel:
        valpent = try(tempModel$paramdf$prentry, silent = TRUE)
        ## If that fails or is the wrong length, revert to defaults of pent1, ..., pentk:
        if(class(valpent) == "try-error" || length(valpent) != valTimeN)
          valpent = efeval("pent(t)", valTimeN)
        ## Anything in valpent that is blank or "calculated" is replaced by defaults: this is included
        ## in case the survey box has only just been enabled or switched from a
        ## lambda-POPAN model:
        valpent <- as.character(valpent)
        blankpent.which <- which(valpent=="" | valpent=="calculated")
        if(length(blankpent.which)>0) valpent[blankpent.which] <-
          paste("pent", blankpent.which, sep="")
      }
      
      ## The pents are disabled if the p's are disabled, and also the pent corresponding to the first survey
      ## must be disabled:
      pentdisabled <- pdisabled
      ## Replace any disabled pents with blanks:
      valpent[pentdisabled] <- ""
      ## Find the first survey:
      if(any(timeoptl)) {
        pentdisabled[min(which(timeoptl))] <- T
        ## Replace the first-survey value of pent with "calculated":
        valpent[min(which(timeoptl))] <- "calculated"
      }
    }
    
    ## -----------------------------------------------------------------------------------------------------
    ## OUTPUT TABLE
    ## -----------------------------------------------------------------------------------------------------
    ## colArgs gives the output table:
    colArgs = list(
      list(type = "text", style = "width:5em", value = valTimeLabels, disabled=TLdisabled),
      list(type = "checkbox", checked = timeoptl),
      list(type = "text", style = "width:5em", value = valphi, disabled = phidisabled),
      list(type = "text", style = "width:5em", value = valp, disabled = pdisabled),
      list(type = "text", style = "width:5em", value = valpent, disabled = pentdisabled)
    )
    # Defines namespace for Model Builder module to use for output ids below
    ns <- NS("ModelUI")
    ## Creates a matrix layout that includes Shiny objects for the final output displayed:
    matLayAc(colTypes = c("extInput", "extInput", "extInput", "extInput", "extInput"),
             colIDs = c(ns("timelabels"), ns("timeopt"), ns("survrate"), ns("capturepr"), ns("prentry")),
             colArgs = colArgs,
             colNames = c("Time Labels", "", "Survival (&phi;)", "Capture Pr (p)",
                          "Pr Entry (p<sub>ent</sub>)"),
             nrow = valTimeN,
             print.rownames=T)
  })
  displaymatrix <- reactiveVal(makedisplaymatrix())
  dismatupdateinputs <- reactive({
    allinputs <- reactiveValuesToList(input)
    allnames <- names(allinputs)
    dismatupdatenames <- allnames[-c(grep("survrate", allnames), grep("capturepr", allnames),
                                     grep("prentry", allnames), grep("timelabels", allnames))]
    # print(dismatupdatenames)
    allinputs[dismatupdatenames]
  })
  readdisplaymatrix <- function() isolate({
    ## input$TimeN is the number of time periods.  This is given value valTimeN and set to 1 if missing:
    # Changed to ModelTimeN
    valTimeN = as.numeric(input$TimeN)
    if(is.na(valTimeN)) valTimeN = 1
    
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
    colIDs = c("timelabels", "timeopt", "survrate", "capturepr", "prentry")
    
    ## Check if UI has finished loading:
    ## the line below checks whether there is an entry present in "out" with name "timelabels01"
    ## or equivalent:
    if(length(out[grep(makeregexp(colIDs[1], 1), outnames)]) > 0){
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
      if(out$modeltype=="lambdamodel") lambdareport <- out$lambdaparam
      else lambdareport <- ""
      
      tempModel = list(
        modelname = out$ModelName,
        description = out$Description,
        chosentimes = chosentimes, # These are the indices of timeopt (a logical), not timeopt itself
        gapvec = diff(chosentimes),
        superpopn = out$superpopn,
        modeltype = out$modeltype, ## All of these above here come from data frame out
        lambdaparam = lambdareport,
        paramdf = outdf   ## paramdf comes from "outdf" which has just been rebuilt
      )
      assign("tempModel", tempModel, envir = CPenv)   ## Whole lot is now assigned to CPenv
    }
  })
  observeEvent(dismatupdateinputs(), {
    # print("inputs changed")  
    readdisplaymatrix()
    displaymatrix(makedisplaymatrix())
  })
  output$modelParamUI <- renderUI(displaymatrix())
  
  savemessage <- reactiveVal(c("", ""))
  observeEvent(input$saveModel, savemessage({    
    save.msg <- c("", "")
    
    ## input$TimeN is the number of time periods.  This is given value valTimeN and set to 1 if missing:
    # Changed to ModelTimeN
    valTimeN = as.numeric(input$TimeN)
    if(is.na(valTimeN)) valTimeN = 1
    
    ##----------------------------------------
    ## Generate "out"
    ##----------------------------------------
    ## This line is needed for opening up the initial model when first starting the interface.
    ## tempModel will be overwritten later, but this is needed to establish the opening model.
    # This is only overwritten with values read from the UI once it has been loaded and had these
    # values written in and possibly updated.
    if(exists("tempModel", envir = CPenv, inherits = FALSE))
      tempModel = get("tempModel", envir = CPenv)
    
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
    colIDs = c("timelabels", "timeopt", "survrate", "capturepr", "prentry")
    
    ## Check if UI has finished loading:
    ## the line below checks whether there is an entry present in "out" with name "timelabels01"
    ## or equivalent:
    if(length(out[grep(makeregexp(colIDs[1], 1), outnames)]) > 0){
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
      if(out$modeltype=="lambdamodel") lambdareport <- out$lambdaparam
      else lambdareport <- ""
      
      tempModel = list(modelname = out$ModelName,
                       description = out$Description,
                       chosentimes = chosentimes,
                       gapvec = diff(chosentimes),
                       superpopn = out$superpopn,
                       modeltype = out$modeltype, ## All of these above here come from data frame out
                       lambdaparam = lambdareport,
                       paramdf = outdf)   ## paramdf comes from "outdf" which has just been rebuilt
      assign("tempModel", tempModel, envir = CPenv)   ## Whole lot is now assigned to CPenv
      
      ## --------------------------------------------------------------------------------------------------
      ## SAVING MODEL:
      ## --------------------------------------------------------------------------------------------------
      
      ## ----------------------------------------------------------------------------------------
      ## Check model:
      ## ----------------------------------------------------------------------------------------
      ## checkModel.func checks the model format. Further checks for identifiability and
      ## the model already existing follow below.
      ## Applying checkModel.func returns T or F for "checkOK", and the appropriate
      ## message to print if an error has been found:
      checkResult <- checkModel.func(model=tempModel)
      
      modelCheckOK <- checkResult$checkOK
      save.msg <- checkResult$msg
      
      ## If the model is correct, proceed to the next step: check for identifiability.
      ## If the model is non-identifiable, check whether to continue.
      ## If continuing, check whether there is already a saved model with that name.
      ## If there is, prompt whether to continue.
      ## If there isn't, continue straight on to save the model.
      ##
      ## If the model is not correct, this block of code below will be skipped and the error message
      ## will be returned at the end of this function and printed by the output$saveMessage routine.
      
      if(modelCheckOK){
        ## -----------------------------------------------------------------------------
        ## IDENTIFIABILITY CHECK:
        ## -----------------------------------------------------------------------------
        ## Identifiability issues can only arise with single phi or full models:
        ## not with lambda models.
        if(input$modeltype=="lambdamodel") identOK <- T
        else if(exists("tempIdentOK", envir=CPenv)) {
          ## If tempIdentOK exists in CPenv, it is a one-off ticket to say that the model is
          ## known to be non-identifiable and the user wants to continue anyway.
          ## It means identOK should be set to True (the only value that tempIdentOK ever
          ##  takes) and then the tempIdentOK ticket should be immediately removed:
          identOK <- get("tempIdentOK", envir=CPenv)
          rm("tempIdentOK", envir=CPenv)
        }
        else identOK <- F
        ## For fullmodel type not already confirmed to ignore non-identifiability.
        
        if(!identOK){
          ## No free ticket for ignoring identifiability. Go ahead and do the checks.
          
          ## ------------------------------------------------------------------------
          ## Check confounding between p1 and pent1:
          ## ------------------------------------------------------------------------
          ## p1 and pent1 are ordinarily confounded. See the Mark Manual, page 12-7,
          ## table 12-2.
          ## Check whether the first ticked survey has a unique non-numeric p and a unique
          ## non-numeric pent.
          firstsurv <- min(which(tempModel$paramdf$timeopt))
          ## Check whether p.firstsurv is the only occurrence: e.g. if this is p1, does p1 appear
          ## anywhere else?  Only applicable if p.firstsurv is not a number.
          p.firstsurv <- tempModel$paramdf$capturepr[firstsurv]
          p.firstsurv.not.number <- is.na(suppressWarnings(as.numeric(p.firstsurv)))
          p.unique <- (length(tempModel$paramdf$capturepr[
            tempModel$paramdf$capturepr==p.firstsurv])==1)
          p1.maybe.nonidentifiable <- (p.firstsurv.not.number & p.unique)
          ## Check pent1:
          ## pent.firstsurv is going to be "calculated", but it might still appear in other
          ## pent parameters:
          ## Note that the inability to allow pent.firstsurv to be a number is a deficiency
          ## of the design.
          ## Include the code for pent.firstsurv.not.number anyway, in case this design
          ## changes.
          pent.firstsurv <- tempModel$paramdf$prentry[firstsurv]
          pent.firstsurv.not.number <- is.na(suppressWarnings(as.numeric(pent.firstsurv)))
          ## Unlike p, this pent being unique means that its length in the paramdf data frame
          ##  is 0, rather than 1, because its own entry is "calculated".
          pent.firstsurv.trueval <- paste("pent", firstsurv, sep="")
          ## Additionally, if ALL the pents are supplied as numbers, then there is no concern
          ## about identifiability.  Check this by seeing if any of the entries in
          ## tempModel$paramdf$prentry match the string "pent", indicating that they contain
          ## a pent parameter to be estimated.
          pent.any.est <- (length(grep("pent", tempModel$paramdf$prentry))>0)
          pent.unique <- (length(tempModel$paramdf$prentry[
            tempModel$paramdf$prentry ==
              pent.firstsurv.trueval])==0)
          pent.maybe.nonidentifiable <- (pent.firstsurv.not.number & pent.unique &
                                           pent.any.est)
          
          
          ## ------------------------------------------------------------------------
          ## Check confounding between p_k and phi_{k-1}:
          ## ------------------------------------------------------------------------
          ## p_k and phi_{k-1} are ordinarily confounded. See the Mark Manual, page
          ## 12-7, table 12-2.
          ## Check whether the first ticked survey has a unique non-numeric p and a unique
          ## non-numeric pent:
          
          lastsurv <- max(which(tempModel$paramdf$timeopt))
          ## Check whether p.lastsurv is the only occurrence: e.g. if this is p8, does p8 appear
          ## anywhere else?  Only applicable if p.lastsurv is not a number.
          p.lastsurv <- tempModel$paramdf$capturepr[lastsurv]
          p.lastsurv.not.number <- is.na(suppressWarnings(as.numeric(p.lastsurv)))
          plast.unique <- (length(tempModel$paramdf$capturepr[
            tempModel$paramdf$capturepr==p.lastsurv])==1)
          plast.maybe.nonidentifiable <- (p.lastsurv.not.number & plast.unique)
          
          ## For the penultimate phi, we need to find the phi corresponding to the
          ## penultimate *survey* (not just time period), and see if this phi occurs any time
          ## BEFORE the penultimate *survey*.  For example, this isn't good enough:
          ## time   survey  phi
          ## 6           yes     phi6
          ## 7           no      phi6
          ## 8           yes        -
          ## - the penultimate survey is at time 6, and phi6 occurs twice, but it never has any
          ## information to unconfound from p8.  We need phi6 to occur *earlier* than time 6
          ## for it to be separable from p8.
          
          ## Find the penultimate survey:
          penultsurv <- max(which(tempModel$paramdf$timeopt[-lastsurv]))
          phi.last <- tempModel$paramdf$survrate[penultsurv]
          phi.last.not.number <- is.na(suppressWarnings(as.numeric(phi.last)))
          
          if(penultsurv > 1){
            phivals.before.penult <- tempModel$paramdf$survrate[1:(penultsurv-1)]
            philast.unique <- (length(phivals.before.penult[phivals.before.penult==
                                                              phi.last])==0)
          }
          else philast.unique <- T
          phi.maybe.nonidentifiable <- (phi.last.not.number & philast.unique)
          
          ## ------------------------------------------------------------------------
          ## Overall non-identifiability:
          ## --------------------------------------------------------------------------------------
          ## nonident.detected if there is confounding in p1 and pent1, or in plast and phi-last:
          nonident.detected <- ((p1.maybe.nonidentifiable & pent.maybe.nonidentifiable) |
                                  (plast.maybe.nonidentifiable & phi.maybe.nonidentifiable))
          ## --------------------------------------------------------------------------------------
          ## Non-identifiability detected: issue warning
          ## --------------------------------------------------------------------------------------
          ## If non-identifiability is suspected, issue the non-identifiability warning and
          ## prompt for whether the user wants to continue:
          if(nonident.detected){
            
            ## Retrieve the current random-number code to confirm save.
            ## If it doesn't exist, select a new random number:
            if(exists("randomNumber", envir=CPenv))
              randno <- get("randomNumber", envir=CPenv)
            else{
              randno <- sample(1:100, 1)
              ## Put the new random number into CPenv, or else this will go into an
              ## endless loop:
              assign("randomNumber", randno, envir=CPenv)
            }
            
            # Render "continueSave" box
            output$savemodelclicked <- renderUI({
              ns <- NS("ModelUI")
              numericInput(ns("continueSave"), "Confirm number:", NA)
            })
            # If input from "continueSave" box has not been recognised yet
            if(is.null(input$continueSave)) confirm.id <- F
            else {
              ## If there is no entry in the "continueSave" box, do not continue:
              if(is.na(input$continueSave)) confirm.id <- F
              ## If there is an entry in the "continueSave" box,
              ## verify that it is the correct number code:
              else confirm.id <- (input$continueSave==randno)
            }
            ## If the correct number code has been entered, confirm.id=T, otherwise
            ## confirm.id=F.
            ## If confirmed, we reset the random number for the next check, and set
            ## identOK=T
            ## and copy the free ticket "don't check identifiability next time" to tempIdentOK
            ## in environment CPenv.
            ## This ticket enables us to ignore the random number check
            ## for identifiability when the code loops round again, so that the
            ## new random number is used to check whether the user wants to overwrite
            ## an existing model.
            ## After that, we move on to the next code block to check whether the
            ## model already exists.
            ## If not confirmed, set identOK=F, so the next code block is skipped and the
            ## model is not saved.
            if(confirm.id){
              ## Assign the free ticket to ignoring identifiability for the next check:
              assign("tempIdentOK", TRUE, envir=CPenv)
              identOK <- T
              ## Create a new random number that will be used for the next save:
              new.randno <- sample(1:100, 1)
              ## Make sure it isn't the same as the previous number.
              ## If it is, arbitrarily add 3.
              if(new.randno==randno) new.randno <- new.randno + 3
              ## Put the new random number in CPenv for retrieval when it is next
              ## needed:
              assign("randomNumber", new.randno, envir=CPenv)
              
            }
            else{
              ## The confirm number doesn't match.
              ## This is either because it hasn't yet been entered for this save iteration;
              ## or because the wrong number was entered for this save iteration.
              ## Display the message showing which number must be displayed to
              ## continue, or alternatively escape from this screen by unticking
              ## the Save Model box:
              save.msg <- c(paste("Warning: your model might be non-identifiable. In a time-varying model, confounding occurs between the first p and pent, and between the last p and phi.  If you want to save the model anyway, enter the number", randno, "in the box below to confirm."), "Alternatively, untick the Save Model box to make changes.")
            }
            
          }  ## End of if(non-identifiability detected)
          else {
            ## --------------------------------------------------------------------------------------
            ## No problem of non-identifiability detected:
            ## --------------------------------------------------------------------------------------
            ## Set identOK=T:
            identOK <- T
          }
        }  ## End if(!identOK)
        ## --------------------------------------------------------------------------------------------
        ## Identifiability result obtained. Continue to try and save if identOK=T.
        ## --------------------------------------------------------------------------------------------
        
        ## Check whether there is already a model saved with this name in ModelList:
        currentModelList <- get("ModelList", envir=CPenv)
        if(identOK) if(any(names(currentModelList)==input$ModelName)){
          ## --------------------------------------------------------------------------------------------
          ## MODEL ALREADY EXISTS:
          ## --------------------------------------------------------------------------------------------
          ## If there is already a model saved with the same name,
          ## retrieve the current random-number code to confirm save.
          ## If it doesn't exist, select a new random number:
          if(exists("randomNumber", envir=CPenv)) randno <- get("randomNumber", envir=CPenv)
          else{
            randno <- sample(1:100, 1)
            ## Put the new random number into CPenv,
            ## or else this will go into an endless loop:
            assign("randomNumber", randno, envir=CPenv)
          }
          
          ## Check whether the model is also part of an existing project:
          currentProjectList <- get("ProjectList", envir=CPenv)
          if(length(currentProjectList)>0){
            projUsingModel <- NULL
            for (proj in names(currentProjectList))
              ## Add any projects that include this model to the vector projUsingModel:
              if(currentProjectList[[proj]]["model"]==input$ModelName)
                projUsingModel <- c(projUsingModel, proj)
          }
          else projUsingModel <- NULL
          
          # Render "continueSave" box
          output$savemodelclicked <- renderUI({
            ns <- NS("ModelUI")
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
            ## The number matches: continue to save the model.
            if(!is.null(projUsingModel)){
              ## The model is part of existing projects. These must be removed,
              ## and all their results also removed.
              for(pr in projUsingModel) {
                resultname <- paste("result.", pr, sep="")
                ## Remove results:
                if(exists(resultname, envir=CPenv))
                  rm(list=resultname, envir=CPenv)
                ## Remove project from project list:
                currentProjectList[[pr]] <- NULL
              }
              ## If all projects have been removed, replace currentProjectList with list():
              if(length(currentProjectList)==0) currentProjectList <- list()
              ## Replace the Project List in CPenv with the reduced one:
              assign("ProjectList", currentProjectList, envir=CPenv)
              capow_list()$project_list(currentProjectList)
              ## Report back:
              save.msg <- c(paste("Project(s) ", paste0(projUsingModel, collapse=", "), 
                                  " have been removed with all their results, and the model has been saved.", sep=""), "")
            }
            else{
              ## The model is not part of existing projects.
              save.msg <- c("Model has been saved.", "")
            }
            
            ## Save the model:
            currentModelList[[input$ModelName]] <- tempModel
            assign("ModelList", currentModelList, envir=CPenv)
            capow_list()$model_list(currentModelList)
            
            ## Create a new random number that will be used for the next save:
            new.randno <- sample(1:100, 1)
            ## Make sure it isn't the same as the previous number.  If it is, arbitrarily add 3.
            if(new.randno==randno) new.randno <- new.randno + 3
            ## Put the new random number in CPenv for retrieval when it is next needed:
            assign("randomNumber", new.randno, envir=CPenv)
          }
          else{
            ## The confirm number doesn't match.
            ## This is either because it hasn't yet been entered for this save iteration;
            ## or because the wrong number was entered for this save
            ## iteration.  Display the message showing which number must be displayed to
            ## continue, or alternatively escape from this screen by unticking the
            ## Save Model box:
            if(!is.null(projUsingModel))
              ## The model is part of existing projects:
              save.msg <- c(paste("A model with this name already exists, and is used by project(s) "
                                  , paste0(projUsingModel, collapse=", "), ". If you continue, these projects and any results will be removed. If you are sure you want to overwrite the existing model and remove the projects, enter the number ", randno, " in the box below and click the Save Model button again to confirm.", sep=""), "Alternatively, make changes or save the model under a different name.")
            
            ## The model is not part of existing projects:
            else save.msg <- c(paste("A model with this name already exists. If you want to overwrite the existing model, enter the number", randno, "in the box below and click the Save Model button again to confirm."), "Alternatively, make changes or save the model under a different name.")
          }
        } ## End of case where there is a model with the same name already saved.
        
        else{
          ## --------------------------------------------------------------------------------------------
          ## MODEL DOES NOT ALREADY EXIST - SAVE IT
          ## --------------------------------------------------------------------------------------------
          ## Model does not exist previously and there are no errors: go ahead and save.
          currentModelList[[input$ModelName]] <- tempModel
          assign("ModelList", currentModelList, envir=CPenv)
          capow_list()$model_list(currentModelList)
          
          save.msg <- c("Model has been saved.", "")
        }
      }  ## End of if(modelCheckOK) : the model checked successfully
    }  ## End of if(UI has finished loading)
    save.msg
  }))
  output$modelSaveMessage <- renderText({
    savemessagevec <- savemessage()
    paste("<strong><font color=\"#CC0033\">", savemessagevec[1], "</font><font color=\"#339933\">",
          savemessagevec[2], "</font></strong>")
  })
  
  # Detailed reactive display of existing models
  output$detailedmodellist <- reactive(
    DisplModel.func(
      names(capow_list()$model_list()), 
      capow_list()$model_list()
    )
  )
  
  # Return reactive list of objects
  capow_list
}