Power <- function(){
        ## Power launches the PowerUI:
        on.exit({
                save(CPenv, file = "CaPow.dat")
        })
        runApp("./PowerUI/")

}

#############################################################################


Plot <- function(){
        ## Plot launches the PlotUI:
        on.exit({
                save(CPenv, file = "CaPow.dat")
        })
        runApp("./PlotUI/")

}

#############################################################################

Run <- function(){
        ## Run launches the RunUI:
        on.exit({
                save(CPenv, file = "CaPow.dat")
        })
        runApp("./RunUI/")

}

#############################################################################
ProjectBuilder <- function(){
        ## ProjectBuilder launches the ProjectUI:
        on.exit({
                save(CPenv, file = "CaPow.dat")
        })
        runApp("./ProjectUI/")

}

#############################################################################
RemoveItems <- function(){
        ## RemoveItems launches the RemoveItemsUI:
        on.exit({
                save(CPenv, file = "CaPow.dat")
        })
        runApp("./RemoveItemsUI/")

}

#############################################################################

SimBuilder <-  function(edit = NULL, copy=NULL, reset=F){
        ## SimBuilder for running SimUI:
        ##
        ## This function is almost identical to ModelBuilder, but with Sim replacing Model, SimList replacing ModelList,
        ## etc.  The only other difference is that no check needs to be done on exit for removing the identifiability flag
        ## from CPenv.  It would be possible to code these two functions together, with a type="Model" or type="Sim"
        ## argument.  For the moment they are coded separately for readability.
        ##
        ## edit (character) is the name of an existing Sim to edit, loaded from the CaPow environment.
        ## It might be either a saved sim (living in SimList) or a work-in-progress (living in the temporary
        ## storage tempSim).
        ## If the sim is available in both places, and differs between the two, the user is prompted to say
        ## which one they want to use.
        ##
        ## If copy is supplied, the GUI will open with a copy of the named sim: e.g. copy="S1" will open
        ## a copy of sim S1, but the name will be set to blank.  Only saved sims can be copied.
        ##
        ## Only one of "edit" and "copy" can be supplied.
        ## Similarly, can't have reset=T if either edit or copy are supplied.
        ##
        ## If edit and copy are both NULL, and reset=F, the GUI will open with the latest work-in-progress,
        ## taken from tempSim, as long as this sim is not identical to one already saved.  If reset=T,
        ## or the work-in-progress is identical to an already-saved sim, the GUI will open with fresh defaults.
        if(!is.null(edit) & !is.null(copy)) stop("\n\nOnly one of 'edit' and 'copy' can be supplied.  \nUse SimBuilder(\"S1\") to edit an existing sim S1. \nUse SimBuilder(copy=\"S1\") to create a new sim copied from sim S1.")

        if(reset & ( !is.null(edit) | !is.null(copy))) stop("\n\nYou can't specify reset=T as well as giving a sim to edit or copy.\nYou can use either SimBuilder(reset=T), or something like SimBuilder(\"S1\") or SimBuilder(copy=\"S1\"), but they can't be used together.")

        SimList <- get("SimList", envir = CPenv)
        if(exists("tempSim", envir = CPenv))
                tempSim <- get("tempSim", envir = CPenv)
        else{
                tempSim <- list()
                ## Make up a dummy sim name that is definitely different from any name that is supplied (even if NULL)
                ## so we can leave the code intact for the case that tempSim doesn't exist:
                tempSim$simname <- paste("blah", edit, "blah", sep="")
        }

        ## -----------------------------------------------------------------------------------------------------------------
        ## IF EDIT IS SUPPLIED:
        ## -----------------------------------------------------------------------------------------------------------------
        if(!is.null(edit)){
                ## If a sim is specified, try to load it.
                ## Check if it exists in SimList:
                in.SL <- any(names(SimList) == edit)
                ## Check if it exists in tempSim:
                in.temp <- (tempSim$simname == edit)

                ## ----------------------------------------------------------------------
                ## If it is in SimList and not in tempSim, assign it to tempSim so the GUI
                ## will open with it:
                if(in.SL & !in.temp)
                        assign("tempSim", SimList[[edit]], envir = CPenv)

                ## ----------------------------------------------------------------------
                ## If it is in tempSim and not in SimList, nothing needs to be done because it is already
                ## there in tempSim ready to open.

                ## ----------------------------------------------------------------------
                ## If it is in both tempSim and SimList, find whether they are different and if so, prompt
                ## the user for which one they want.
                else if(in.SL & in.temp) {
                        sim.SL <- SimList[[edit]]
                        sim.temp <- tempSim
                        ## We don't want to compare whether the save message differs between the sims, so make
                        ## it the same for both:
                        sim.SL$savemsg <- ""
                        sim.temp$savemsg <- ""
                        any.differ <- rep(F, length(sim.SL))
                        ## Following line suppresses a warning about comparing different length vectors:
                        if(length(sim.SL$chosentimes)!=length(sim.temp$chosentimes))
                                any.differ[1] <- T
                        else
                                for(elt in 1:length(sim.SL)) any.differ[elt] <- any( sim.SL[[elt]] != sim.temp[[elt]])

                        ## If any of any.differ is true, then prompt the user to ask which one they want:
                        if(any(any.differ)){
                                prpt <- readline(paste("\n\nSim", edit, "is available as a saved version and a draft version in progress. \nWhich version do you want?\nType 'saved' for the saved version, 'draft' for the draft version:\n"))
                                chosenSim <- switch(prpt, "saved"=sim.SL, "draft"=sim.temp,
                                                      stop("I don't understand your answer."))
                                assign("tempSim", chosenSim, envir = CPenv)
                        }
                        ## Otherwise, there is no difference between the two copies so we don't need to do anything:
                        ## the correct sim is already in tempSim.
                }

                ## ----------------------------------------------------------------------
                ## If the sim doesn't exist in either SimList or tempSim, open with fresh defaults:
                else if(!in.SL & !in.temp){
                        ## If the named sim doesn't exist, give an error and load defaults:
                        warning("Sim ", edit, " cannot be found. Loading defaults...",
                                call. = FALSE, immediate. = TRUE)
                        if(exists("tempSim", envir = CPenv, inherits = FALSE))
                                rm("tempSim", envir = CPenv)
                }
        }

        ## -----------------------------------------------------------------------------------------------------------------
        ## IF COPY IS SUPPLIED:
        ## -----------------------------------------------------------------------------------------------------------------

        if(!is.null(copy)){
                ## If a copy-sim is specified, try to find it:
                ## Check if it exists in SimList:
                in.SL <- any(names(SimList) == copy)
                ## Check if it exists in tempSim:
                in.temp <- (tempSim$simname == copy)

                ## ----------------------------------------------------------------------
                ## If it is in SimList and not in tempSim, pull it out and call it copySim:
                if(in.SL & !in.temp)
                        copySim <- SimList[[copy]]

                ## ----------------------------------------------------------------------
                ## If it is in tempSim and not in SimList, dump with an error that the copied sim has not been
                ## saved yet and the draft will be over-written:
                else if(in.temp & !in.SL)
                        stop(paste("Sim ", copy, " has not been saved. \nProceeding will overwrite the draft. \nTo save, open with SimBuilder(\"", copy, "\")\n", sep=""))

                ## ----------------------------------------------------------------------
                ## If it is in both tempSim and SimList, find whether they are different and if so, check that the user wants
                ## to continue and overwrite the draft. If they are the same, proceed to copy the sim.
                else if(in.SL & in.temp) {
                        sim.SL <- SimList[[copy]]
                        sim.temp <- tempSim
                        ## We don't want to compare whether the save message differs between the sims, so make
                        ## it the same for both:
                        sim.SL$savemsg <- ""
                        sim.temp$savemsg <- ""
                        any.differ <- rep(F, length(sim.SL))
                        ## Following line suppresses a warning about comparing different length vectors:
                        if(length(sim.SL$chosentimes)!=length(sim.temp$chosentimes))
                                any.differ[1] <- T
                        else
                                for(elt in 1:length(sim.SL)) any.differ[elt] <- any( sim.SL[[elt]] != sim.temp[[elt]])

                        ## If any of any.differ is true, then prompt the user to ask which one they want:
                        if(any(any.differ)){
                                prpt <- readline(paste("\n\nSim", copy, "is available as a saved version and a draft version.\nOnly the saved version will be copied. \nThe draft will be overwritten if you continue. \nDo you want to continue? (y/n)\n"))
                                if(prpt!="y" & prpt!="yes") stop(paste("Cancelling copy-sim.  To save draft, use SimBuilder(\"", copy, "\") and tick the save box.\n", sep=""))
                                else copySim <- SimList[[copy]]
                        }
                        else copySim <- SimList[[copy]]
                }
                ## ----------------------------------------------------------------------
                ## If the sim doesn't exist in either SimList or tempSim, open with fresh defaults:
                else if(!in.SL & !in.temp){
                        ## If the named sim doesn't exist, give an error and load defaults:
                        stop(paste("Sim", copy, "cannot be found. Did you forget to save it?"))
                }

                ## -------------------------------------------------------------------------
                ## If we've got to here, we've successfully got a sim copySim.  Change its name to blank
                ## and assign it to tempSim, so it will be opened by the GUI.
                copySim$simname <- ""
                assign("tempSim", copySim, envir = CPenv)
        }

        ## -----------------------------------------------------------------------------------------------------------------
        ## IF NEITHER EDIT NOR COPY IS SUPPLIED, RESET CAN BE TRUE OR FALSE:
        ## -----------------------------------------------------------------------------------------------------------------
        if(is.null(edit) & is.null(copy)){
                if(reset) {
                        ## If resetting, clear any existing tempSim so the GUI will open with defaults:
                        if(exists("tempSim", envir = CPenv, inherits = FALSE))
                                rm("tempSim", envir = CPenv)
                }
                else{
                        ## If not resetting, first check whether tempSim exists and if so whether it conflicts with
                        ## a previously-saved sim.  If it does, prompt for required action: saved, draft, or reset.
                        ## If tempSim exists but does not conflict with a previously-saved sim, open it without prompting.
                        ## If tempSim does not exist, open with defaults.
                        if(exists("tempSim", envir = CPenv, inherits = FALSE)){
                                tempSim <- get("tempSim", envir = CPenv)

                                ## Check if it exists in SimList:
                                in.SL <- any(names(SimList) == tempSim$simname)

                                ## ----------------------------------------------------------------------
                                ## If it is in SimList, find whether they are different and if so, check what the user wants
                                ## to do.
                                if(in.SL) {
                                        sim.SL <- SimList[[tempSim$simname]]
                                        sim.temp <- tempSim
                                        ## We don't want to compare whether the save message differs between the sims, so make
                                        ## it the same for both:
                                        sim.SL$savemsg <- ""
                                        sim.temp$savemsg <- ""
                                        any.differ <- rep(F, length(sim.SL))
                                        ## Following line suppresses a warning about comparing different length vectors:
                                        if(length(sim.SL$chosentimes)!=length(sim.temp$chosentimes))
                                                any.differ[1] <- T
                                        else
                                                for(elt in 1:length(sim.SL)) any.differ[elt] <- any( sim.SL[[elt]] != sim.temp[[elt]])
                                        ## If any of any.differ is true, then prompt the user to ask which one they want:
                                        if(any(any.differ)){
                                                prpt <- readline(paste("\n\nThe most recently edited sim, ", tempSim$simname, ", is available as a saved version and a draft version.\nType:\n 'draft' to open with the draft version; \n'saved' to open with the saved version; \n'reset' or Enter to delete the draft and open with defaults.\n", sep=""))
                                                ## If user wants to open the saved version, do so:
                                                if(prpt=="saved") tempSim <- sim.SL
                                                ## Otherwise, anything except 'draft' will reset to the defaults:
                                                else if(prpt!="draft") tempSim <- NULL
                                        }
                                        ## Put the selected sim into tempSim in CPenv,
                                        ## or if resetting, remove tempSim from CPenv:
                                        if(!is.null(tempSim)) assign("tempSim", tempSim, envir = CPenv)
                                        else rm("tempSim", envir=CPenv)
                                } ## End of if(in.SL)

                                ## Otherwise, tempSim is the only copy with that simname and it stays unchanged in CPenv.

                        }  ## End of if(exists(tempSim))
                        ## Otherwise, tempSim does not exist, so no action needs to be taken: defaults are the only option.

                } ## End of no arguments supplied: no edit and no copy, and reset=F

        }  ## End of no edit and no copy.

        on.exit({
                ## Identifiability check is not needed for simsets, so this line is removed:
                ##  if(exists("tempIdentOK", envir=CPenv)) rm("tempIdentOK", envir=CPenv)
                save(CPenv, file = "CaPow.dat")
        })
        runApp("./SimUI/")
}





#############################################################################


ModelBuilder <-  function(edit = NULL, copy=NULL, reset=F){
        ## ModelBuilder for running ModelUI:
        ## modified from CPbackendModelUI.
        ##
        ## edit (character) is the name of an existing Model to edit, loaded from the CaPow environment.
        ## It might be either a saved model (living in ModelList) or a work-in-progress (living in the temporary
        ## storage tempModel).
        ## If the model is available in both places, and differs between the two, the user is prompted to say
        ## which one they want to use.
        ##
        ## If copy is supplied, the GUI will open with a copy of the named model: e.g. copy="M1" will open
        ## a copy of model M1, but the name will be set to blank.  Only saved models can be copied.
        ##
        ## Only one of "edit" and "copy" can be supplied.
        ## Similarly, can't have reset=T if either edit or copy are supplied.
        ##
        ## If edit and copy are both NULL, and reset=F, the GUI will open with the latest work-in-progress,
        ## taken from tempModel, as long as this model is not identical to one already saved.  If reset=T,
        ## or the work-in-progress is identical to an already-saved model, the GUI will open with fresh defaults.
        if(!is.null(edit) & !is.null(copy)) stop("\n\nOnly one of 'edit' and 'copy' can be supplied.  \nUse ModelBuilder(\"M1\") to edit an existing model M1. \nUse ModelBuilder(copy=\"M1\") to create a new model copied from model M1.")

        if(reset & ( !is.null(edit) | !is.null(copy))) stop("\n\nYou can't specify reset=T as well as giving a model to edit or copy.\nYou can use either ModelBuilder(reset=T), or something like ModelBuilder(\"M1\") or ModelBuilder(copy=\"M1\"), but they can't be used together.")

        ModelList <- get("ModelList", envir = CPenv)
        if(exists("tempModel", envir = CPenv))
                tempModel <- get("tempModel", envir = CPenv)
        else{
                tempModel <- list()
                ## Make up a dummy model name that is definitely different from any name that is supplied (even if NULL)
                ## so we can leave the code intact for the case that tempModel doesn't exist:
                tempModel$modelname <- paste("blah", edit, "blah", sep="")
        }

        ## -----------------------------------------------------------------------------------------------------------------
        ## IF EDIT IS SUPPLIED:
        ## -----------------------------------------------------------------------------------------------------------------
        if(!is.null(edit)){
                ## If a model is specified, try to load it.
                ## Check if it exists in ModelList:
                in.ML <- any(names(ModelList) == edit)
                ## Check if it exists in tempModel:
                in.temp <- (tempModel$modelname == edit)

                ## ----------------------------------------------------------------------
                ## If it is in ModelList and not in tempModel, assign it to tempModel so the GUI
                ## will open with it:
                if(in.ML & !in.temp)
                        assign("tempModel", ModelList[[edit]], envir = CPenv)

                ## ----------------------------------------------------------------------
                ## If it is in tempModel and not in ModelList, nothing needs to be done because it is already
                ## there in tempModel ready to open.

                ## ----------------------------------------------------------------------
                ## If it is in both tempModel and ModelList, find whether they are different and if so, prompt
                ## the user for which one they want.
                else if(in.ML & in.temp) {
                        model.ML <- ModelList[[edit]]
                        model.temp <- tempModel
                        ## We don't want to compare whether the save message differs between the models, so make
                        ## it the same for both:
                        model.ML$savemsg <- ""
                        model.temp$savemsg <- ""
                        any.differ <- rep(F, length(model.ML))
                        ## Following line suppresses a warning about comparing different length vectors:
                        if(length(model.ML$chosentimes)!=length(model.temp$chosentimes))
                                any.differ[1] <- T
                        else
                                for(elt in 1:length(model.ML)) any.differ[elt] <- any( model.ML[[elt]] != model.temp[[elt]])

                        ## If any of any.differ is true, then prompt the user to ask which one they want:
                        if(any(any.differ)){
                                prpt <- readline(paste("\n\nModel", edit, "is available as a saved version and a draft version in progress. \nWhich version do you want?\nType 'saved' for the saved version, 'draft' for the draft version:\n"))
                                chosenModel <- switch(prpt, "saved"=model.ML, "draft"=model.temp,
                                                      stop("I don't understand your answer."))
                                assign("tempModel", chosenModel, envir = CPenv)
                        }
                        ## Otherwise, there is no difference between the two copies so we don't need to do anything:
                        ## the correct model is already in tempModel.
                }

                ## ----------------------------------------------------------------------
                ## If the model doesn't exist in either ModelList or tempModel, open with fresh defaults:
                else if(!in.ML & !in.temp){
                        ## If the named model doesn't exist, give an error and load defaults:
                        warning("Model ", edit, " cannot be found. Loading defaults...",
                                call. = FALSE, immediate. = TRUE)
                        if(exists("tempModel", envir = CPenv, inherits = FALSE))
                                rm("tempModel", envir = CPenv)
                }
        }

        ## -----------------------------------------------------------------------------------------------------------------
        ## IF COPY IS SUPPLIED:
        ## -----------------------------------------------------------------------------------------------------------------

        if(!is.null(copy)){
                ## If a copy-model is specified, try to find it:
                ## Check if it exists in ModelList:
                in.ML <- any(names(ModelList) == copy)
                ## Check if it exists in tempModel:
                in.temp <- (tempModel$modelname == copy)

                ## ----------------------------------------------------------------------
                ## If it is in ModelList and not in tempModel, pull it out and call it copyModel:
                if(in.ML & !in.temp)
                        copyModel <- ModelList[[copy]]

                ## ----------------------------------------------------------------------
                ## If it is in tempModel and not in ModelList, dump with an error that the copied model has not been
                ## saved yet and the draft will be over-written:
                else if(in.temp & !in.ML)
                        stop(paste("Model ", copy, " has not been saved. \nProceeding will overwrite the draft. \nTo save, open with ModelBuilder(\"", copy, "\")\n", sep=""))

                ## ----------------------------------------------------------------------
                ## If it is in both tempModel and ModelList, find whether they are different and if so, check that the user wants
                ## to continue and overwrite the draft. If they are the same, proceed to copy the model.
                else if(in.ML & in.temp) {
                        model.ML <- ModelList[[copy]]
                        model.temp <- tempModel
                        ## We don't want to compare whether the save message differs between the models, so make
                        ## it the same for both:
                        model.ML$savemsg <- ""
                        model.temp$savemsg <- ""
                        any.differ <- rep(F, length(model.ML))
                        ## Following line suppresses a warning about comparing different length vectors:
                        if(length(model.ML$chosentimes)!=length(model.temp$chosentimes))
                                any.differ[1] <- T
                        else
                                for(elt in 1:length(model.ML)) any.differ[elt] <- any( model.ML[[elt]] != model.temp[[elt]])

                        ## If any of any.differ is true, then prompt the user to ask which one they want:
                        if(any(any.differ)){
                                prpt <- readline(paste("\n\nModel", copy, "is available as a saved version and a draft version.\nOnly the saved version will be copied. \nThe draft will be overwritten if you continue. \nDo you want to continue? (y/n)\n"))
                                if(prpt!="y" & prpt!="yes") stop(paste("Cancelling copy-model.  To save draft, use ModelBuilder(\"", copy, "\") and tick the save box.\n", sep=""))
                                else copyModel <- ModelList[[copy]]
                        }
                        else copyModel <- ModelList[[copy]]
                }
                ## ----------------------------------------------------------------------
                ## If the model doesn't exist in either ModelList or tempModel, open with fresh defaults:
                else if(!in.ML & !in.temp){
                        ## If the named model doesn't exist, give an error and load defaults:
                        stop(paste("Model", copy, "cannot be found. Did you forget to save it?"))
                }

                ## -------------------------------------------------------------------------
                ## If we've got to here, we've successfully got a model copyModel.  Change its name to blank
                ## and assign it to tempModel, so it will be opened by the GUI.
                copyModel$modelname <- ""
                assign("tempModel", copyModel, envir = CPenv)
        }

        ## -----------------------------------------------------------------------------------------------------------------
        ## IF NEITHER EDIT NOR COPY IS SUPPLIED, RESET CAN BE TRUE OR FALSE:
        ## -----------------------------------------------------------------------------------------------------------------
        if(is.null(edit) & is.null(copy)){
                if(reset) {
                        ## If resetting, clear any existing tempModel so the GUI will open with defaults:
                        if(exists("tempModel", envir = CPenv, inherits = FALSE))
                                rm("tempModel", envir = CPenv)
                }
                else{
                        ## If not resetting, first check whether tempModel exists and if so whether it conflicts with
                        ## a previously-saved model.  If it does, prompt for required action: saved, draft, or reset.
                        ## If tempModel exists but does not conflict with a previously-saved model, open it without prompting.
                        ## If tempModel does not exist, open with defaults.
                        if(exists("tempModel", envir = CPenv, inherits = FALSE)){
                                tempModel <- get("tempModel", envir = CPenv)

                                ## Check if it exists in ModelList:
                                in.ML <- any(names(ModelList) == tempModel$modelname)

                                ## ----------------------------------------------------------------------
                                ## If it is in ModelList, find whether they are different and if so, check what the user wants
                                ## to do.
                                if(in.ML) {
                                        model.ML <- ModelList[[tempModel$modelname]]
                                        model.temp <- tempModel
                                        ## We don't want to compare whether the save message differs between the models, so make
                                        ## it the same for both:
                                        model.ML$savemsg <- ""
                                        model.temp$savemsg <- ""
                                        any.differ <- rep(F, length(model.ML))
                                        ## Following line suppresses a warning about comparing different length vectors:
                                        if(length(model.ML$chosentimes)!=length(model.temp$chosentimes))
                                                any.differ[1] <- T
                                        else
                                                for(elt in 1:length(model.ML)) any.differ[elt] <- any( model.ML[[elt]] != model.temp[[elt]])
                                        ## If any of any.differ is true, then prompt the user to ask which one they want:
                                        if(any(any.differ)){
                                                prpt <- readline(paste("\n\nThe most recently edited model, ", tempModel$modelname, ", is available as a saved version and a draft version.\nType:\n 'draft' to open with the draft version; \n'saved' to open with the saved version; \n'reset' or Enter to delete the draft and open with defaults.\n", sep=""))
                                                ## If user wants to open the saved version, do so:
                                                if(prpt=="saved") tempModel <- model.ML
                                                ## Otherwise, anything except 'draft' will reset to the defaults:
                                                else if(prpt!="draft") tempModel <- NULL
                                        }
                                        ## Put the selected model into tempModel in CPenv,
                                        ## or if resetting, remove tempModel from CPenv:
                                        if(!is.null(tempModel)) assign("tempModel", tempModel, envir = CPenv)
                                        else rm("tempModel", envir=CPenv)
                                } ## End of if(in.ML)

                                ## Otherwise, tempModel is the only copy with that modelname and it stays unchanged in CPenv.

                        }  ## End of if(exists(tempModel))
                        ## Otherwise, tempModel does not exist, so no action needs to be taken: defaults are the only option.

                } ## End of no arguments supplied: no edit and no copy, and reset=F

        }  ## End of no edit and no copy.

        on.exit({
                if(exists("tempIdentOK", envir=CPenv)) rm("tempIdentOK", envir=CPenv)
                save(CPenv, file = "CaPow.dat")
        })
        runApp("./ModelUI/")
}





#############################################################################

CPinit  <- function(CPload = NULL){
        ## Args:
        ## --CPload (character)--
        ## Name of data file to load a CaPow environment
        ## Default: NULL - attempts to find a "CaPow.dat"
        ##   if found, will use that as CPload.
        ##   Otherwise creates a new CaPow environment

        ## If CPload is NULL (default), look for "CaPow.dat"
        if(is.null(CPload)){
                wdfiles = list.files()
                if(any(wdfiles == "CaPow.dat"))
                        CPload = "CaPow.dat"
        }

        ## If CPload is a character vector, load it
        if(mode(CPload) == "character"){
                cat(paste0("Loading from file \"", CPload, "\"...\n"))
                load(CPload, envir = .GlobalEnv)
                # load(CPload, envir = parent.env(parent.frame()))

        } else{
                ## Else, initialise a new CaPow environment
                
                # Changed here to try to make CPenv default to nest inside the server
                # environment for each session 
                # But this is irrelevant because it only happens if there's no capow.dat
                # above
                assign("CPenv", new.env(parent = .GlobalEnv), envir = .GlobalEnv)
                # assign("CPenv", new.env())
                # print("parent.frame()")
                # print(parent.frame())
          
                assign("ModelList", list(), envir = CPenv)
                assign("SimList", list(), envir = CPenv)
                assign("ProjectList", list(), envir = CPenv)
        }
}

CPsave = function(filename = "CaPow.dat"){
        ## Args:
        ## --filename (character)--
        ## Name of data file to save current CaPow environment
        ## Default: "CaPow.dat"
        ## Check if file already exists, give warning
        wdfiles = list.files()
        if(any(wdfiles == filename))
                inp = readline(paste0("A \"", filename, "\" already exists. Overwrite? (yes/no)\n"))
        else
                inp = "yes"

        ## Save the CaPow environment to file
        if(inp == "yes" || inp == "y"){
                cat(paste0("Saving to file \"", filename, "\"...\n"))
                save(CPenv, file = filename)
        } else
        cat("Save cancelled.\n")
}

CPmodelUI =
  ## Launch the Model Specification UI
  ## Args:
  ## --loadModel (character)--
  ## Name of an existing Model to load from CaPow environment
  ## --loadSim (character)--
  ## Name of an existing Simset to load from CaPow environment
  ## Default: Both NULL - Do not load and use fresh defaults
  ## If both loadModel and loadSim are specified, the latter is ignored.
  function(loadModel = NULL, loadSim = NULL){
    if(!is.null(loadModel)){
      ModelName = loadModel
      LoadList = "ModelList"
    } else if(!is.null(loadSim)){
      ModelName = loadSim
      LoadList = "SimList"
    } else{
      ModelName = NULL
      LoadList = NULL
    }

    assign("ModelUIType", "Model", envir = CPenv)
    CPbackendModelUI(ModelName, LoadList, SaveList = "ModelList")
  }

CPsimsetUI =
  ## Launch the Simset Specification UI
  ## Args:
  ## --loadSim (character)--
  ## Name of an existing Simset to load from CaPow environment
  ## --loadModel (character)--
  ## Name of an existing Model to load from CaPow environment
  ## Default: Both NULL - Do not load and use fresh defaults
  ## If both loadSim and loadModel are specified, the latter is ignored.
  function(loadSim = NULL, loadModel = NULL){
    if(!is.null(loadSim)){
      ModelName = loadSim
      LoadList = "SimList"
    } else if(!is.null(loadModel)){
      ModelName = loadModel
      LoadList = "ModelList"
    } else{
      ModelName = NULL
      LoadList = NULL
    }

    assign("ModelUIType", "Simset", envir = CPenv)
    CPbackendModelUI(ModelName, LoadList, SaveList = "SimList")
  }

CPbackendModelUI =
  ## THIS FUNCTION IS NOW A BACKEND
  ## MAKE CALLS TO CPmodelUI OR CPsimsetUI
  ## Args:
  ## --ModelName (character)--
  ## Name of an existing Model to load from CaPow environment
  ## Default: NULL - Do not load and use fresh defaults
  ## --LoadList (character)--
  ## The list in the CaPow environment to load ModelName from
  ## --SaveList (character)--
  ## The list in the CaPow environment to save to.
  ## Passed to CPbackendModelSave()
  function(ModelName = NULL, LoadList = NULL, SaveList = NULL){
    if(!is.null(ModelName)){
      ## If a model is specified, need to load this on start-up
      ## Check it exists and load.
      ## If it doesn't exist give an error
      ModelList = get(LoadList, envir = CPenv)
      if(any(names(ModelList) == ModelName))
        assign("tempModel", ModelList[[ModelName]], envir = CPenv)
      else{
        warning("\"", ModelName, "\" cannot be found, loading defaults...",
                call. = FALSE, immediate. = TRUE)
        if(exists("tempModel", envir = CPenv, inherits = FALSE))
          rm("tempModel", envir = CPenv)
      }
    } else{
      ## Else, clear any existing tempModel
      if(exists("tempModel", envir = CPenv, inherits = FALSE))
        rm("tempModel", envir = CPenv)
    }
    on.exit(CPbackendModelSave(SaveList))
    runApp("./ModelUI/")
  }

CPbackendModelSave =
  ## Sub-function for saving models
  ## Should generally only be called as part of CPbackendModelUI()
  ## Args:
  ## --SaveList (character)--
  ## The list in the CaPow environment to save to
  function(SaveList){
    ## Grab required var
    tempModel = get("tempModel", envir = CPenv)
    ModelList = get(SaveList, envir = CPenv)
    ModelExists = any(names(ModelList) == tempModel$modelname)
    ModelUIType = get("ModelUIType", envir = CPenv)

    ## Ask if user wants to save the model
    prompttext = paste0("Save ", ModelUIType, " of name \"", tempModel$modelname, "\"? (yes/no)\n")
    ## Let user know if model of same name already exists
    if(ModelExists)
      prompttext = paste0(prompttext, "A ", ModelUIType, " of this name already exists ",
        "and will be overwritten.\nEnter (rename) to rename new ", ModelUIType, ".\n")
    inp = readline(prompttext)

    if(inp == "rename"){
      inp = readline("Enter a new name for the ", ModelUIType, ":\n")
      tempModel$modelname = inp
      assign("tempModel", tempModel, envir = CPenv)
      CPbackendModelSave()
    } else if(inp == "yes" || inp == "y"){
      ## If Model of same name exists, update that
      if(ModelExists){
        cat(paste0("Updating ", ModelUIType, " of name \"", tempModel$modelname, "\"...\n"))
        ModelList[[tempModel$modelname]] = tempModel
      } else{
        ## Else, add a new model
        cat(paste0("Adding new ", ModelUIType, " of name \"", tempModel$modelname, "\"...\n"))
        newModel = list(tempModel)
        names(newModel) = tempModel$modelname
        ModelList = c(ModelList, newModel)
      }
      assign(SaveList, ModelList, envir = CPenv)
      cat(paste0(tempModel$modelname, " has been saved.\n",
                 "Don't forget to call CPsave() to write changes to disk.\n"))
    }
  }

CPlistModels =
  ## Trivial function for listing all current models
  function()
        naturalsort(names(get("ModelList", envir = CPenv)))

CPlistSimsets =
  ## Trivial function for listing all current simsets
  function()
        naturalsort(names(get("SimList", envir = CPenv)))

CPremoveModel =
  function(ModelName = NULL)
    CPbackendRemoveModel(ModelName, "ModelList", "Model")

CPremoveSimset =
  function(SimName = NULL)
    CPbackendRemoveModel(SimName, "SimList", "Simset")

CPbackendRemoveModel =
  ## Args:
  ## --ModelName (character)--
  ## Name of an existing Model to delete from CaPow environment
  function(ModelName, DelList, ModelType){
    ModelList = get(DelList, envir = CPenv)
    if(any(names(ModelList) == ModelName)){
      inp = readline(paste0("Really remove ", ModelUIType, " of name \"",
                            ModelName, "\"? (yes/no)\n"))
      if(inp == "yes" || inp == "y"){
        ModelList = ModelList[names(ModelList) != ModelName]
        assign(DelList, ModelList, envir = CPenv)
      }
    } else cat(paste0("Error. No ", ModelUIType, " of name \"", ModelName, "\" found.\n"))
  }

CPcreateProject =
  ##
  ## Args:
  ## --SaveList (character)--
  ## The list in the CaPow environment to save to
  local({
    makeMatrix =
      ##
      ## Args:
      ## --SaveList (character)--
      ## The list in the CaPow environment to save to
      function(inlist){
        outmat = NULL
        for(i in 1:length(inlist))
          outmat = rbind(outmat, c(inlist[[i]]$modelname, inlist[[i]]$description))
        colnames(outmat) = c("Name", "Description")
        as.data.frame(outmat)
      }
  function(SaveList){
    ## Grab required var
    ModelList = get("ModelList", envir = CPenv)
    SimList = get("SimList", envir = CPenv)
    ProjList = get("ProjList", envir = CPenv)

    if(length(ModelList) < 1 || length(SimList) < 1)
      stop("Need at least 1 Model and at least 1 Simset", call. = FALSE)

    cat("Choose the Model:\n")
    print(makeMatrix(ModelList))
    selModel = as.numeric(readline("Enter the Model Number, e.g. 1\n"))

    cat("Choose the Simulation:\n")
    print(makeMatrix(SimList))
    selSim = as.numeric(readline("Enter the Simulation Number, e.g. 1\n"))

    ProjNameCheck = TRUE
    ProjExists = FALSE
    while(ProjNameCheck){
      ProjName = readline("Enter the Project's Name:\n")
      ## Check if project of same name already exists
      if(any(names(ProjList) == ProjName)){
        inp = readline(paste0("A project of this name already exists.\n",
                       "Overwrite? (overwrite) or (o) to overwrite, otherwise rename.\n"))
        if(inp == "overwrite" || inp == "o"){
          ProjExists = TRUE
          ProjNameCheck = FALSE
        }
      } else ProjNameCheck = FALSE
    }

    ProjDescrip = readline("Enter a short description for the project:\n")

    tempModel = list(name = ProjName, descrip = ProjDescrip, model = ModelList[[selModel]], sim = SimList[[selSim]])

    ## If Project of same name exists, update that
    if(ProjExists){
      cat(paste0("Updating project of name \"", ProjName, "\"...\n"))
      ProjList[[ProjName]] = tempModel
    } else{
      ## Else, add a new project
      cat(paste0("Adding new project of name \"", ProjName, "\"...\n"))
      newProj = list(tempModel)
      names(newProj) = ProjName
      ProjList = c(ProjList, newProj)
    }
    assign("ProjList", ProjList, envir = CPenv)
    cat(paste0(ProjName, " has been saved.\n",
               "Don't forget to call CPsave() to write changes to disk.\n"))
  }})

CPprojectUI =
  ## Launch the Project UI for running simulations
  function(){
    on.exit(CPrunsim())
    ## TEMP CODE TO FAKE EXISTENCE OF
    ## ProjList
    ## ProjHaveResults
    assign("ProjList",
       list(ProjA = list(modelname = "ProjA", description = "And the silken sad uncertain rustling of each purple curtain"),
            ProjB = list(modelname = "ProjB", description = "Thrilled me- filled me with fantastic terrors never felt before;"),
            ProjC = list(modelname = "ProjC", description = "So that now, to still the beating of my heart, I stood repeating,"),
            ProjD = list(modelname = "ProjD", description = "\"'Tis some visitor entreating entrance at my chamber door-"),
            ProjE = list(modelname = "ProjE", description = "Some late visitor entreating entrance at my chamber door;-"),
            ProjF = list(modelname = "ProjF", description = "This it is, and nothing more.\"")
            ),
       envir = CPenv)
    assign("ProjHaveResults", as.logical(rbinom(6, 1, 0.5)), CPenv)
    ## END TEMP

    runApp("./ProjectUI/")
  }

CPrunsim =
  ## Sub-function for running simulations based on
  ## CPprojectUI results.
  ## To be done by Rachel?
  function(){
    cat("RACHEL TO DO:\n",
        "Tie CPprojectUI code to actual Project List\n",
        "  and set ProjHaveResults with however it should be set\n",
        "Also, the output is saved to Projout, use:\n",
        "  get(\"Projout\", envir = CPenv)\n",
        "to retrieve it and use.\n")
  }
