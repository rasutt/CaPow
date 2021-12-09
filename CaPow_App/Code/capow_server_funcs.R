##############################################################

popan.mean.plot <- function(res.raw, plotwhat=c("phi", "p", "pent", "lambda", "N"),
                            view.phi="auto", view.p="auto", view.pent="auto", view.N="auto", view.lambda="auto",
                            autorange=10,
                            gridspace.phi=0.1, gridspace.p=0.1, gridspace.pent=0.1, gridspace.N=50,
                            gridspace.lambda=0.1,
                            include.constlevel="auto",
                            cex.pt=2, cex.main=1.8, cex.text=1.8, cex.axis=1.5,
                            threshold.negvar=1e-6){
  ## popan.mean.plot : 2/11/15
  ## Schematic chart to display mean estimates for each parameter type, colour-coded and distinguished
  ## according to whether the parameter is treated as a fixed constant (lines drawn to upper level); estimated
  ## either in its own timeslot or a different one (black lines drawn to lower level); or derived from other
  ## parameters in "calc" mode (orange lines drawn to lower level).
  ##
  ## Takes argument res.raw: results directly out of popan.sim.wrap, not yet cleaned.
  ##
  ## The view parameters can be "auto", or a vector depicting xlim, like view.p=c(0, 1).
  ## If "auto", the plot uses R's automatic range plus or minus autorange % of the central value
  ## to give a zoomed-in view.
  ##
  ## gridlines are marked on every plot with spacing given by the corresponding gridspace: if gridlines
  ## aren't wanted for a plot, leave the corresponding entry NA.
  ##
  ## If include.constlevel="auto", the plot will only include the levels for constant (non-estimated) parameters
  ## if there are any in the model setup.  Otherwise, include.constlevel should be "yes".
  ## This covers all allowed cases: we don't allow constlevel NOT to be plotted if there are constant parameters
  ## in the model setup.
  ##
  ## Any results with any variances < (-threshold.negvar) are removed entirely before any plotting is done.
  
  ## Housekeeping:
  ## Clean results, removing any rows that are non-converged (code not in (1, 2, 3) or flag=1),
  ## or any rows that have any negative variances more negative than threshold.negvar; negative
  ## variances within threshold.negvar are reset to 0:
  res <- clean.result.func(resdf=res.raw, threshold.negvar=threshold.negvar)
  nres <- nrow(res)
  setup <- attributes(res)$setup
  
  ## ------------------------------------------------------------------------------------------------
  ## Add columns to res corresponding to "calculated" parameters pent.  If this is a lambda-model,
  ## all pent parameters need to be calculated using phi and lambda results.  If not, there will still be
  ## one pent parameter that is 1 minus the sum of the others (which could include non-estimated or
  ## piggy-back estimated ones).  The function pent.calc.func takes care of all cases.
  res <- pent.calc.func(res.clean=res, res.setup=setup)
  
  ## ------------------------------------------------------------------------------------------------
  ## Now that res has all the necessary columns, create the column names:
  allColMeans <- colMeans(res)
  allColNames <- names(allColMeans)
  
  ## ------------------------------------------------------------------------------------------------
  ## Set colours: can be changed into parameters later if required:
  est.colour <- "black"  ## Colour used for plotting estimated means
  const.colour <- "brown"  ## Colour for plotting to values of fixed parameters
  calc.colour <- "darkorange2"  ## Colour for plotting to values of calculated parameters
  trueline.colour <- "blue"  ## Colour for the line in the true level
  estline.colour <- "red2"  ## Colour for the line in the constant level
  
  ## If lambda has been requested, but it isn't a lambda-model, just ignore lambda by deleting it from plotwhat:
  if(("lambda" %in% plotwhat) & (!setup$lambdamodel)) plotwhat <- plotwhat[plotwhat!="lambda"]
  
  ## include.constlevel should be "auto" or "yes":
  if(!(include.constlevel %in% c("auto", "yes"))) stop("include.constlevel should be either \'auto\' or \'yes\'.")
  ## If include.constlevel = "auto" find what it should be:
  if(include.constlevel=="auto"){
    include.constlevel <- suppressWarnings(any(!is.na(as.numeric(setup$allparvec))))
  }
  else include.constlevel <- T
  
  ## --------------------------------------------------------------------------------------------------------------
  ## FIGURE LAYOUT:
  ## Layout in one row with the required number of plots (use equal spacing each):
  nplot <- length(plotwhat)
  layout(matrix(1:nplot, ncol=nplot), widths=rep(1, nplot))#widths=c(1.2, 1.2, 1.2, 0.4))
  
  ## Set the vertical levels for plotting truevals, fixed (constant, not estimated), and estimated
  ## parameters.  Only include the "const" level if include.constlevel=T.
  if(include.constlevel){
    ## If plotting the constant bar:
    y.trueval <- 1.5
    y.const <- 2.5
    y.est <- 0.5
    y.top <- 2.6   ## top of the plot
  }
  else{
    ## If not plotting the constant bar:
    y.trueval <- 1.5
    y.est <- 0.5
    y.top <- 2   ## top of the plot
  }
  
  ## ------------------------------------------------------------------------------------------------
  ## Set up the title and axis labels for each parameter:
  titlevec <- c(p=expression(bold("Capture probability, "*p[t])),
                phi=expression(bold("Annual survival, "*phi)),
                pent=expression(bold("Entry proportion, "*p[ent(t)])),
                N=expression(bold("Superpopulation, "*N[s])),
                lambda=expression(bold("Annual growth rate, "*lambda)))
  labelvec <- c(p=expression("Capture probability, "*p), phi=expression("Survival probability, "*phi),
                pent=expression("Entry proportion, "*p[ent]), N=expression("Superpopulation size, "*N[s]),
                lambda=expression("Annual growth rate, "*lambda))
  
  ## ------------------------------------------------------------------------------------------------
  ## FUNCTION TO PLOT A SINGLE PANEL: PARAMETER 'PAR'
  ## ------------------------------------------------------------------------------------------------
  onepar.plot <- function(par, firstplot=T){
    ## par can be "p", "phi", "pent", "lambda", or "N".
    
    ## parnames includes all par-slot parameters, whether they are estimated or constant:
    ## For example, if there are surveys at times 1, 2, 4, 5, and we use the following model
    ## setup for capture probabilities: p1, 0.2, p1, p5, then parnames = c("p1", "p2", "p4", "p5").
    if(par=="lambda") parnames <- "lambda"
    else if(par=="N") parnames <- "N"
    else parnames <- eval(parse(text=paste0("setup[[\'", par, "names\']]")))
    par.simvec <- setup$truevals[parnames]
    
    ## The possibilities for modelled par's are:
    ## - estimated (either via its own time-slot or via a different one): plot these in black;
    ## - fixed as constant; plot these in const.colour.
    ## The allparvec results show how each parameter is dealt with in the model, which
    ## we translate to vector par.details.
    ## In the example above, par.details would be:
    ##    p1    p2    p4    p5
    ##  "p1" "0.2"  "p1"  "p5"
    par.details <- setup$allparvec[parnames]
    
    ## Find which parameters are fixed (not estimated):
    ## par.const converts all the parameter values to numbers where possible:
    par.const <- suppressWarnings(as.numeric(par.details))
    names(par.const) <- parnames
    which.par.const <- which(!is.na(par.const))
    which.par.est <- which(is.na(par.const))
    
    ## If view = "auto", we need to work out the data range to plot:
    view.par <- eval(parse(text=paste0("view.", par)))
    if(is.numeric(view.par)) par.xlim <- view.par
    else{
      ## If view.par is anything other than a numeric range, assume that it's requesting "auto"
      ## and find the corresponding xlim as the range of the values within.
      par.all <- c(par.simvec, allColMeans[allColNames %in% parnames], par.const)
      par.xlim <- range(par.all[!is.na(par.all)])
      ## Set the range to be R's range +/- autorange% of the central value:
      par.xlim <- par.xlim+c(-1, 1)*autorange/100*mean(par.xlim)
    }
    ## Use the function pretty(vals, n=10) to give a prettier range: this is what is
    ## used in the R function plot.window, see code at the end of help(plot.window) to confirm.
    par.xlim <- range(pretty(par.xlim, n=10))
    
    ## ------------------------------------------------------------
    ## Set up the plot: note that the second component of mar (corresponding to the left margin)
    ## is high for the first plot, because it includes the text.  Default mar is (5, 4, 4, 2)
    ## in order c(bottom, left, top, right).
    if(firstplot) par(mar=c(4.1, 6, 3, 1), mgp=c(2.8, 0.8, 0))
    else par(mar=c(4.1, 3, 3, 1), mgp=c(2.8, 0.8, 0))
    plot(1, xlim=par.xlim, ylim=c(0, y.top), type="n", ylab="", bty="n", yaxt="n", cex.axis=cex.axis,
         cex.lab=cex.axis, xlab=labelvec[par],
         main=titlevec[par], cex.main=cex.main)
    gridspace.par <- eval(parse(text=paste0("gridspace.", par)))
    if(!is.na(gridspace.par)){
      abline(v=seq(min(par.xlim)-min(par.xlim)%%gridspace.par,
                   max(par.xlim)+gridspace.par, by=gridspace.par), col="grey70", lty=3)
    }
    lines(par.xlim, rep(y.trueval,2), col=trueline.colour, lwd=3)
    lines(par.xlim, rep(y.est, 2), col=estline.colour, lwd=3)
    ## Add explanatory text if it's the first plot:
    if(firstplot){
      mtext(text="True",side=2, outer=F, at=y.trueval-0.15, adj=0.5, las=1,
            col=trueline.colour, cex=cex.text)
      mtext(text=paste0("Mean Estimate (n=", nres, ")"),
            side=2, outer=F, at=y.est-0.15, adj=0.2, las=1,
            col=estline.colour, cex=cex.text)
    }
    ## Plot the constant bar if required:
    if(include.constlevel){
      lines(par.xlim, rep(y.const, 2), col=const.colour, lwd=3)
      if(firstplot)
        mtext(text="Fixed", side=2, outer=F, at=y.const-0.15, adj=0.5, las=1, col=const.colour,
              cex=cex.text)
    }
    ## Plot the simulated values in the middle line (y-coordinate y.trueval):
    points(par.simvec, rep(y.trueval, length(par.simvec)), pch=16, cex=cex.pt)
    
    ## Plot the values of fixed parameters above the sim values (y-coordinate y.const):
    if(length(which.par.const)>0){
      parnames.const <- parnames[which.par.const]
      for(parnam in parnames.const){
        points(par.const[parnam], y.const, col=const.colour, pch=16, cex=cex.pt)
        lines(c(par.simvec[parnam], par.const[parnam]), c(y.trueval, y.const), col=const.colour)
      }
    }
    ## Plot the values of estimated parameters below the sim values (y-coordinate y.est):
    if(length(which.par.est)>0){
      parnames.est <- parnames[which.par.est]
      ## In the example above, parnames.est includes the time-slot parameter p4, which is
      ## estimated via parameter p1.  So we need to link the simulated p4-value to the
      ## estimated p1-value.  In this case, parnam="p4" and parplot="p1".
      for(parnam in parnames.est){
        parplot <- par.details[parnam]
        ## If parplot="calc", the parameter has already been calculated and the calculation
        ## has already been entered into res under name parnam;
        ## however the "calc" flag notifies us that we want to use a different colour to plot it.
        if(parplot=="calc"){
          ## Extract the column of res that has already been specified to have the same
          ## name as the "calc" parameter:
          parplot.mean.est <- mean(res[[parnam]])
          colour.parnam <- calc.colour
        }
        else{
          ## If parplot is not "calc", then parplot is the name of the parameter used
          ## in the parameter slot given by parnam.  (For example, we might have
          ## parameter slot parnam=phi3, but it might be estimated by parameter phi2.)
          ## Extract the column of res given by parplot and use black colour to plot:
          parplot.mean.est <- mean(res[[parplot]])
          colour.parnam <- est.colour
        }
        ## cat(c("Time slot:", parnam, "  Plotting: ", parplot, "  Mean est = ", parplot.mean.est, "\n"))
        lines(c(par.simvec[parnam], parplot.mean.est), c(y.trueval, y.est), col=colour.parnam)
        points(parplot.mean.est, y.est, col=colour.parnam, pch=16, cex=cex.pt)
        
      }
      ## Overplot the central points for aesthetics:
      points(par.simvec, rep(y.trueval, length(par.simvec)), pch=16, cex=cex.pt)
    }
    
  }  ## End of onepar.plot
  ## ------------------------------------------------------------------------------------------------
  
  ## ------------------------------------------------------------------------------------------------
  ## CREATE PLOTS FOR EACH PARAMETER:
  sapply(1:nplot, function(i) onepar.plot(plotwhat[i], firstplot=(i==1)))
  NULL
}

##############################################################

process.onepar.func <- function(res.clean, res.setup, partype, conf, collapse.single, impose.01limits){
  ## process.onepar.func 27/11/15
  ## Internal function to process results for one parameter type: finds how each parameter has been
  ## treated (estimated, piggy-back, fixed, derived), finds confidence intervals where appropriate.
  ## Called from within popan.summary.func, popan.boxplot.
  ## Only deals with clean results that have been put through clean.result.func.
  ##
  ## NOTE: If partype = "pent", then columns for any "calc" parameters should have been added to res.clean
  ## BEFORE sending it to this function.
  ##
  ## partype can be "p", "phi", "pent", "lambda", or "N".
  ##
  ## In the output summary:
  ## relativeBias is (mean(estimates) - simulated value) / simulated value;
  ## rootMeanSqError is sqrt( mean((estimates - simulated value)^2) )
  ## empiricalSD is the standard deviation of estimated values;
  ## empiricalCV is the standard deviation of estimated values divided by the mean of estimated values;
  ## meanEstCV is the mean of estimated CVs.
  
  ## ----------------------------------------------------------------------------------------------------------------
  ## Function starts here.
  ## parnames includes all par-slot parameters, whether they are estimated or constant:
  ## For example, if there are surveys at times 1, 2, 4, 5, and we use the following model
  ## setup for capture probabilities: p1, 0.2, p1, p5, then parnames = c("p1", "p2", "p4", "p5").
  if(partype=="lambda") parnames <- "lambda"
  else if(partype=="N") parnames <- "N"
  else parnames <- eval(parse(text=paste0("res.setup[[\'", partype, "names\']]")))
  
  nres <- nrow(res.clean)
  
  ## Find the true (simulated) values:
  par.simvec <- res.setup$truevals[parnames]
  
  ## The possibilities for modelled par's are:
  ## - estimated in its own time-slot: type "Estimated"
  ## - estimated in a time-slot other than its own: known colloquially as piggy-back, formally as
  ##   something like "Estimated via p1" if it's piggybacking off parameter p1.
  ## - fixed as constant: type "Fixed"
  ## - derived from other parameters; e.g. pents derived from lambdas, or pent[1]=1-sum of other pents,
  ##   known as "Derived"
  ## The allparvec results show how each parameter is dealt with in the model, which
  ## we translate to vector par.details.
  ## For example, par.details could be:
  ##    p1    p2    p4    p5
  ##  "p1" "0.2"  "p1"  "p5"
  par.details <- res.setup$allparvec[parnames]
  
  ## If collapse.single=T, determine whether the descriptions can be collapsed to a single one
  ## for this parameter type.  This only applies if all par.details are the same AND if all par.simvec
  ## are the same.
  # Might get an error here if try to boxplot lambda for non-lambda model, have to fix that.
  if(collapse.single & all(par.simvec==par.simvec[1]) & all(par.details==par.details[1])){
    ## if(length(parnames)>1) catline("Collapsing to single row for parameter ", partype)
    par.simvec <- par.simvec[1]
    par.details <- par.details[1]
    parnames <- parnames[1]
  }
  
  ## Find which parameters are fixed (not estimated):
  ## par.const converts all the parameter values to numbers where possible:
  par.const <- suppressWarnings(as.numeric(par.details))
  names(par.const) <- parnames
  which.par.const <- which(!is.na(par.const))
  
  ## Find the parameters that are estimated in their own time-slots: these are the ones
  ## for which par.details == names(par.details):
  which.par.est <- which(par.details==names(par.details))
  
  ## Find which parameters are derived from other parameters ("calc"):
  which.par.calc <- which(par.details=="calc")
  
  ## Find piggy-back parameters: these have par.details != names(par.details),
  ## par.details != "calc", and is.na(as.numeric(par.details)):
  which.par.piggy <- which( (par.details !=names(par.details)) & (par.details != "calc") &
                              is.na(suppressWarnings(as.numeric(par.details))))
  
  ## -----------------------------------------------------------------------------------------------------------------
  ## Enter the details of how each parameter is treated into the vector par.treatment:
  par.treatment <- rep("", length(parnames))
  names(par.treatment) <- parnames
  par.treatment[which.par.est] <- "Estimated"
  par.treatment[which.par.piggy] <- paste0("Estimated via ", par.details[which.par.piggy])
  par.treatment[which.par.const] <- "Fixed"
  par.treatment[which.par.calc] <- "Derived"
  
  ## Compile a data frame containing the details for every parameter:
  par.out <- data.frame(parname=parnames, details=par.details, treatment=par.treatment,
                        simval=par.simvec)
  par.out$CIavail <- rep(0, nrow(par.out))
  par.out$CIavail[c(which.par.est, which.par.piggy)] <- 1
  
  parnames.for.ci <- c(parnames[which.par.est], parnames[which.par.piggy])
  varnames <- paste0("var.", parnames)
  varnames.for.ci <- c(varnames[which.par.est], varnames[which.par.piggy])
  
  ## -----------------------------------------------------------------------------------------------------------------
  ## CONSTRUCT DATA FRAME par.est.df FOR ALL RESULTS:
  ## Everything will be added to par.est.df: all estimated parameters and their variance columns;
  ## identical columns for all piggy-back parameters and their variances; a column of identical constants
  ## for fixed parameters; the derived values for calc parameters.  (The latter two have no variance columns).
  ## NOTE: calc parameters should have been added to res.clean BEFORE sending it to this function.
  
  ## Find the data frame of estimated values, and also append any variance columns for which.par.est:
  if(length(which.par.est) + length(which.par.calc)>0){
    par.est.df <- res.clean[, parnames[c(which.par.est, which.par.calc)], drop=F]
    ## If some of these values are estimated (not calc), append the variances as well:
    if(length(which.par.est)>0)
      par.est.df <- data.frame(par.est.df, res.clean[, paste0("var.", parnames[which.par.est]), drop=F])
  }
  else par.est.df <- list()
  
  ## Add to par.est.df the constant parameters, if any:
  if(length(which.par.const)>0){
    for(cp in which.par.const){
      par.est.df[[parnames[cp]]] <- rep(as.numeric(par.const[cp]), nres)
    }
  }
  ## In case par.est.df isn't already a data frame (because it contains only constant elements so far),
  ## coerce it into a data frame now:
  par.est.df <- as.data.frame(par.est.df, stringsAsFactors=F)
  ## Add to par.est.df the piggy-back parameters, if any, and the associated variances:
  if(length(which.par.piggy)>0){
    for(cp in which.par.piggy){
      par.est.df[parnames[cp]] <- par.est.df[par.details[cp]]
      ## Append the variances:
      par.est.df[paste0("var.", parnames[cp])] <- par.est.df[paste0("var.", par.details[cp])]
    }
  }
  
  ## -----------------------------------------------------------------------------------------------------------------
  ## SUMMARY ESTIMATE PERFORMANCE:
  ## relativeBias is (mean(estimates) - simulated value) / simulated value.
  ## rootMeanSqError is sqrt( mean((estimates - simulated value)^2) )
  ## empiricalSD is the standard deviation of estimated values.
  ## empiricalCV is the standard deviation of estimated values divided by the mean of estimated values.
  
  par.out$rootMeanSqError <- par.out$empiricalCV <- par.out$empiricalSD <-
    par.out$relativeBias <- par.out$meanEst <-  rep(NA, length(parnames))
  
  for(onepar in parnames){
    ests.par <- par.est.df[, onepar]
    sim.par <- par.out$simval[par.out$parname==onepar]
    par.out$meanEst[par.out$parname==onepar] <- mean(ests.par)
    par.out$relativeBias[par.out$parname==onepar] <- (mean(ests.par) - sim.par)/sim.par
    par.out$rootMeanSqError[par.out$parname==onepar] <- sqrt(mean((ests.par - sim.par)^2))
    par.out$empiricalCV[par.out$parname==onepar] <- sqrt(var(ests.par))/mean(ests.par)
    par.out$empiricalSD[par.out$parname==onepar] <- sqrt(var(ests.par))
  }
  
  ## -----------------------------------------------------------------------------------------------------------------
  ## CONFIDENCE INTERVALS AND PERFORMANCE OF VARIANCE ESTIMATION:
  
  par.out$CIcover <- par.out$medCIwidth <- par.out$meanEstCV <- par.out$meanSE <-
    rep(NA, length(parnames))
  
  ## Compute CIs only for parameters that are estimated or piggy-back estimated:
  if((length(which.par.est) + length(which.par.piggy)>0)){
    est.var.in <- par.est.df[, c(parnames.for.ci, varnames.for.ci)]
    ## Find the CI results.  Use lognormal CIs for N and normal for all other parameters.
    ## ci.out contains 4 columns for each parameter in parnames.for.ci: e.g. for parameter phi1,
    ## we have cilow.phi1, cihi.phi1 giving the low and high confidence limits;
    ## cover.phi1 = 1 if the interval covers the true value and 0 otherwise;
    ## cicode.phi1 which is one of "cic" (interval covers true value); "cib" (interval lies wholly below
    ## true value); "cia" (interval lies wholly above true value).
    ci.out <- ci.func(est.var.df=est.var.in, par.names=parnames.for.ci, conf=conf,
                      lognormal=rep(partype=="N", length(parnames.for.ci)),
                      query.vals=par.simvec[parnames.for.ci], impose.01limits=impose.01limits)
    
    ## Summarize results from the CIs for one parameter at a time:
    ## meanEstCV is the mean of estimated CVs.
    for(cipar in parnames.for.ci){
      ## CI coverage:
      par.out$CIcover[par.out$parname==cipar] <- sum(ci.out[, paste0("cover.", cipar)]) / attributes(res.clean)$n_datasets
      ## median CI width:
      par.out$medCIwidth[par.out$parname==cipar] <- median(ci.out[, paste0("cihi.", cipar)] -
                                                             ci.out[, paste0("cilow.", cipar)])
      par.out$meanEstCV[par.out$parname==cipar] <-
        mean(sqrt(est.var.in[, paste0("var.", cipar)])/est.var.in[, cipar])
      par.out$meanSE[par.out$parname==cipar] <-
        mean(sqrt(est.var.in[, paste0("var.", cipar)]))
    }
    
    
  }  ## End of any parameters available for CI
  else ci.out <- NULL
  
  return(list(par.est.df=par.est.df, ci.out=ci.out, parnames=parnames, simvals=par.simvec,
              which.est=which.par.est,
              which.estvia=which.par.piggy, which.fixed=which.par.const, which.derived=which.par.calc,
              parnames.for.ci=parnames.for.ci, varnames.for.ci=varnames.for.ci,
              summary.table=par.out))
  
}

##############################################################

popan.summary.func <- function(res.raw, plotwhat=c("lambda", "N", "phi", "p", "pent"), conf=0.95,
                               collapse.single=T, threshold.negvar=1e-6, impose.01limits=T) {
  ## popan.summary.func 27/12/15
  ## Creates an output summary table for all parameters specified in plotwhat.
  ## Any results with any variances < (-threshold.negvar) are removed entirely before summarizing.
  ##
  ## Takes argument res.raw: results directly out of popan.sim.wrap, not yet cleaned.
  ##
  ## If collapse.single=T, then parameter types that are fitted with a single parameter AND that
  ## are simulated with a single value are collapsed to a single row.  For example, if both
  ## the simulation model and the fitted model are lambdamodels or constant-phi models ,
  ## then there is only one value of phi simulated and one value fitted, so collapse.single=T will
  ## display a single row for all phi values.  If collapse.single=F then a separate (identical) row
  ## will be included for every phi-slot in this case.
  
  ## Housekeeping:
  ## Clean results, removing any rows that are non-converged (code not in (1, 2, 3) or flag=1),
  ## or any rows that have any negative variances more negative than threshold.negvar; negative
  ## variances within threshold.negvar are reset to 0:
  res <- clean.result.func(resdf=res.raw, threshold.negvar=threshold.negvar)
  nres <- nrow(res)
  setup <- attributes(res)$setup
  
  ## If lambda has been requested, but it isn't a lambda-model, just ignore lambda by deleting it from plotwhat:
  if(("lambda" %in% plotwhat) & (!setup$lambdamodel)) plotwhat <- plotwhat[plotwhat!="lambda"]
  npartype <- length(plotwhat)
  
  ## ------------------------------------------------------------------------------------------------
  ## Add columns to res corresponding to "calculated" parameters pent.  If this is a lambda-model,
  ## all pent parameters need to be calculated using phi and lambda results.  If not, there will still be
  ## one pent parameter that is 1 minus the sum of the others (which could include non-estimated or
  ## piggy-back estimated ones).  The function pent.calc.func takes care of all cases.
  res <- pent.calc.func(res.clean=res, res.setup=setup)
  
  ## ------------------------------------------------------------------------------------------------
  ## CREATE SUMMARIES FOR EACH PARAMETER TYPE:
  summary.out <- data.frame()
  for(i in 1:length(plotwhat)) summary.out <- rbind(summary.out,
                                                    process.onepar.func(res.clean=res, res.setup=setup, partype=plotwhat[i],
                                                                        conf=conf, collapse.single=collapse.single,
                                                                        impose.01limits=impose.01limits)$summary.table)
  ## Beautify the titles:
  rownames(summary.out) <- 1:nrow(summary.out)
  names(summary.out)[names(summary.out)=="parname"] <- "Parameter"
  names(summary.out)[names(summary.out)=="treatment"] <- "Treatment"
  names(summary.out)[names(summary.out)=="simval"] <- "TrueValue"
  names(summary.out)[names(summary.out)=="rootMeanSqError"] <- "RMSE"  ## to shorten it
  summary.out
}

##############################################################

ci.func <- function(est.var.df, par.names, conf=0.95, lognormal=(par.names=="N"), query.vals=NULL,
                    include.estcolumns=F, impose.01limits=T){
  ## ci.func 19/11/15
  ## Finds lower and upper 100*conf CIs for each parameter named in par.names.
  ## est.var.df must contain a column "par" of estimates for each name "par" in par.names, and
  ## a column var.par of estimated variances for this parameter.
  ##
  ## This function doesn't remove any results due to negative variances, non-convergence, etc.
  ## It's assumed this has all been done beforehand.
  ##
  ## lognormal must be a vector of length par.names.  It must either be supplied with names
  ## matching par.names, or if it has no names it is assumed to be in the same order as par.names.
  ## By default, lognormal is set to lognormal=T for parameter N and lognormal=F for all other parameters.
  ##
  ## Normal CIs:  est +/-  1.959964*sqrt(varhat(est))
  ## Lognormal CIs: est/C to est*C, where C = exp(1.959964 * sqrt(log(1 + varhat(est) / est^2)))
  ## or if conf != 0.95, then 1.959964 is replaced by qnorm(1-(1-conf)/2).
  ##
  ## If impose.01limits=T, any confidence limits of probabilities that are <0 are reset to 0 and any that are >1
  ## are reset to 1 (that means p, pent, phi);
  ## additionally, any confidence limits of lambda or N that are <0 are reset to 0.
  ##
  ## If query.vals is supplied, the function returns CI coverage statistics as well.  Query.vals needs to have the
  ## same names as par.names.  Any parameter not included in query.vals will be ignored for coverage statistics.
  ## If the aim of using this function is to investigate CI coverage of the true value, use query.vals=truevals
  ## as in popan.ci.plot. If the aim is to investigate power to detect that a parameter is not equal to X,
  ## then use query.vals=X: for example if we want to detect lambda != 1 then query.vals=c(lambda=1).
  ##
  ## If include.estcolumns=T, columns containing the parameter estimates are included with the returned
  ## data frame of CI results.  Might not want include.estcolumns if the CI information is to be appended to
  ## the results data frame, because then the estimate columns would be duplicated.
  
  ## First check that all the par.names and their variances are to be found in est.var.df:
  if(!(all(par.names %in% names(est.var.df)))) {
    cat("par.names", par.names, "\n") ## Giving NA
    cat("names(est.var.df)", names(est.var.df), "\n") ## Giving nothing?
    stop("All parameter names in par.names must be named columns of est.var.df.")
  }
  if(!(all(paste0("var.", par.names) %in% names(est.var.df))))
    stop("All parameter names in par.names must have a variance column in est.var.df.")
  ## Deal with lognormal vector: it must be the same length as par.names, and if it has no names
  ## already it's given the same names as par.names.  If it does have names already, everything in
  ## par.names must be there.
  if(length(lognormal)!=length(par.names)) stop("lognormal T/F has a different length from par.names.")
  if(is.null(names(lognormal))) names(lognormal) <- par.names
  if(!all(par.names %in% names(lognormal)))
    stop("Parameters specified in par.names are not given names in the lognormal T/F argument.")
  
  ## Extract estimates and var.estimates as data frames:
  est.df <- est.var.df[, par.names, drop=F]
  var.df <- est.var.df[, paste0("var.", par.names), drop=F]
  
  ## Find the zalpha value:
  zalpha <- qnorm(1 - (1-conf)/2)
  
  ## Find the lower and upper CIs for the whole matrix in one go:
  ci.lower.df <- ci.upper.df <- est.df
  lognorm.names <- par.names[lognormal]
  norm.names <- par.names[!lognormal]
  ## mult is needed for lognormal; margin for normal
  if(any(lognormal)){
    mult <- exp(zalpha * sqrt(log(1 + var.df / est.df^2)))
    names(mult) <- names(est.df)
    ci.lower.df[, lognorm.names] <- est.df[, lognorm.names] / mult[, lognorm.names]
    ci.upper.df[, lognorm.names] <- est.df[, lognorm.names] * mult[, lognorm.names]
  }
  if(any(!lognormal)) {
    margin <- zalpha * sqrt(var.df)
    names(margin) <- names(est.df)
    ci.lower.df[, norm.names] <- est.df[, norm.names] - margin[, norm.names]
    ci.upper.df[, norm.names] <- est.df[, norm.names] + margin[, norm.names]
  }
  
  names(ci.lower.df) <- paste0("cilow.", par.names)
  names(ci.upper.df) <- paste0("cihi.", par.names)
  ci.out <- data.frame(ci.lower.df, ci.upper.df)
  ## Reorder the columns of ci.out to cluster each parameter together:
  ci.out <- ci.out[paste0(rep(c("cilow.", "cihi."), times=length(par.names)), rep(par.names, each=2))]
  
  ## Impose 0-1 limits if desired:
  if(impose.01limits){
    ## All parameters are positive, so any elements of ci.out that are <0 should be reset to 0:
    ci.out[ci.out<0] <- 0
    ## Parameters p, phi, and pent are probabilities, so any elements of ci.out in these columns
    ## that are >1 should be reset to 1.  Really lazy way of doing this, but just grep "p" in the
    ## column names:
    probcol.names <- grep("p", names(ci.out))
    if(length(probcol.names)>0) ci.out[probcol.names][ci.out[probcol.names]>1] <- 1
  }
  
  ## Run through each parameter in par.names and find coverage if applicable:
  for(parnm in par.names) if(!is.null(query.vals[parnm])){
    trueval.parnm <- query.vals[parnm]
    cover.parnm <- rep(0, nrow(ci.out))
    cilow.parnm <- ci.out[paste0("cilow.", parnm)]
    cihi.parnm <- ci.out[paste0("cihi.", parnm)]
    cover.parnm[ (cilow.parnm<=trueval.parnm) & (trueval.parnm<=cihi.parnm) ] <- 1
    ## CI code contains a code for each replicate:
    ## "cib" means the entire CI lies below the true value;
    ## "cic" means the CI contains the true value;
    ## "cia" means the entire CI lies above the true value.
    ci.code.parnm <- rep("cib", nrow(ci.out))
    ci.code.parnm[cihi.parnm >= trueval.parnm] <- "cic"
    ci.code.parnm[cilow.parnm > trueval.parnm] <- "cia"
    ## Append the two new columns to the data frame: cover.parnm, and cicode.parnm:
    ci.out[paste0("cover.", parnm)] <- cover.parnm
    ci.out[paste0("cicode.", parnm)] <- ci.code.parnm
  }
  ## If include.estcolumns, also append a column containing the estimated value:
  if(include.estcolumns) ci.out[par.names] <- est.df[par.names]
  
  return(ci.out)
  
}

##############################################################
clean.result.func <- function(resdf, threshold.negvar){
  ## clean.result.func 24/11/15
  ## Cleans the fitted results in resdf, such that results are removed if they have code not in (1, 2, 3),
  ## or flag=1. If there are negative variances they are converted to 0 if they are within threshold.negvar
  ## of 0 (e.g. threshold.negvar=1e-6), or otherwise if there are still negative variances
  ## the whole row is removed as an assumed non-MLE convergence.
  ## resdf needs to have columns code and flag, and any column marked "var.*" is assumed to be the
  ## variance estimates of a parameter.
  
  # Save total number of datasets
  attributes(resdf)$n_datasets = nrow(resdf)

  ## Remove any results in res that didn't converge or that had final parameters out of range:
  resdf <- resdf[resdf$code %in% c(1, 2, 3),]
  resdf <- resdf[resdf$flag==0,]
  ## Replace any variance results with -threshold.negvar < var < 0 by 0, and then remove any rows that
  ## still have any negative variances in them or where the variances are NA:
  var.cols <- grep("var.", names(resdf))
  if(length(var.cols)>0){
    if(any(is.na(resdf[var.cols]))){   ## Missing variances
      which.ok <- which( apply(resdf[var.cols], 1, function(x) all(!is.na(x))) )
      resdf <- resdf[ which.ok, ]
    }
    if(any(resdf[var.cols]<0)){  ## Negative variances
      resdf[var.cols][  (-threshold.negvar < resdf[var.cols]) & (resdf[var.cols]<0) ] <- 0
      ## Any remaining rows with negative variances should be removed:
      which.pos <- which( apply(resdf[var.cols], 1, function(x) all(x>=0)) )
      resdf <- resdf[ which.pos, ]
    }
  }
  
  return(resdf)
}

##############################################################

popan.boxplot <- function(res.raw, plotwhat=c("phi", "p", "pent", "lambda", "N"), conf=0.95, collapse.single=T,
                          n.plotcol=3, time.axis=T,
                          gridspace.phi=0.05, gridspace.p=0.2, gridspace.pent=0.2, gridspace.N=100,
                          gridspace.lambda=0.1,
                          plot.limits=T, cex.main=1.8, cex.axis=1.5, cex.text=1.2, cex.limit=1,
                          citext=T, bottomline.text="medCIwidth", textspace=(if(citext) c(0.6, 0.6) else c(0.3, 0.3)),
                          threshold.negvar=1e-6, impose.01limits=T){
  ## popan.boxplot 19/11/15
  ## Takes argument res.raw: results directly out of popan.sim.wrap, not yet cleaned.
  ##
  ## If collapse.single=T, then parameter types that are fitted with a single parameter AND that
  ## are simulated with a single value are collapsed to a single boxplot.  For example, if both
  ## the simulation model and the fitted model are lambdamodels or constant-phi models ,
  ## then there is only one value of phi simulated and one value fitted, so collapse.single=T will plot
  ## just a single boxplot for all phi values.  If collapse.single=F then a separate (identical) boxplot
  ## will be plotted for every phi-slot in this case.
  ##
  ## Any results with any variances < (-threshold.negvar) are removed entirely before any plotting is done.
  ##
  ## n.plotcol is the number of columns desired for the plot.
  ##
  ## If time.axis=T, a time axis is plotted using the time labels specified in ModelBuilder / SimBuilder.
  ## If plot.limits=T, limits like 0, 1 for probabilities are plotted if they don't interfere with the text.
  ##
  ## gridlines are marked on every plot with spacing given by the corresponding gridspace: if gridlines
  ## aren't wanted for a plot, leave the corresponding entry NA.
  ##
  ## textspace is the amount of space at bottom, top of boxplot for plotting text
  ##
  ## citext specifies whether text regarding estimator performance is to be plotted on the plots.
  ## If so, then this function plots the following:
  ## - For parameters with a single box, e.g. N, lambda: mean estimate and true value on the top line;
  ## - For all parameters, CI coverage % on the next line above the boxplots;
  ## - User choice of text below the boxplots, determined by argument bottomline.text which can be one of:
  ##   "", "empiricalSD", "empiricalCV", "relativeBias", "rootMeanSqError", "meanEst", "meanSE",
  ##     "meanEstCV", "medCIwidth"
  ##   where: "" plots nothing; empiricalSD plots SD of the estimates; empiricalCV plots %CV of the estimates;
  ##   relativeBias plots (mean est - true val)/true val * 100;
  ##   rootMeanSqError plots   sqrt( mean( (ests - true val)^2) ); meanEst plots the mean estimate;
  ##   meanEstCV plots the mean of estimated %CVs; and medCIwidth plots the median CI width
  ##   across estimates.
  
  ## Housekeeping:
  ## Clean results, removing any rows that are non-converged (code not in (1, 2, 3) or flag=1),
  ## or any rows that have any negative variances more negative than threshold.negvar; negative
  ## variances within threshold.negvar are reset to 0:
  res <- clean.result.func(resdf=res.raw, threshold.negvar=threshold.negvar)
  nres <- nrow(res)
  setup <- attributes(res)$setup
  modelTable <- setup$ModelTable[setup$ModelTable$survey,]
  
  ## ------------------------------------------------------------------------------------------------
  ## Add columns to res corresponding to "calculated" parameters pent.  If this is a lambda-model,
  ## all pent parameters need to be calculated using phi and lambda results.  If not, there will still be
  ## one pent parameter that is 1 minus the sum of the others (which could include non-estimated or
  ## piggy-back estimated ones).  The function pent.calc.func takes care of all cases.
  res <- pent.calc.func(res.clean=res, res.setup=setup)
  
  ## ------------------------------------------------------------------------------------------------
  ## Now that res has all the necessary columns, create the column names:
  allColMeans <- colMeans(res)
  allColNames <- names(allColMeans)
  
  ## ------------------------------------------------------------------------------------------------
  ## Limit calculations for case where phi or lambda is constant in a lambda-model:
  ##
  ## This is a special case for lambda-models: if either phi or lambda are specified as constants in the
  ## model, they (1) act as bounds for estimates of the other parameter, and (2) need to be extracted
  ## as res.lambda and/or res.phi before calculating pent.  Define the limits globally for convenience,
  ## because the plotting function only takes one type of parameter at a time so it doesn't easily deal with
  ## interactions between parameters.
  phi.limit <- lambda.limit <- res.lambda <- res.phi <- NA
  if(setup$lambdamodel){
    lambda.limit <- suppressWarnings(as.numeric(setup$allparvec["lambda"]))
    phi.limit <- suppressWarnings(as.numeric(setup$allparvec["phi1"]))
    if(!is.na(lambda.limit))
      ## Lambda is estimated as a constant: we need to insert the column "lambda" into res:
      res.lambda <- rep(lambda.limit, nres)
    if(!is.na(phi.limit))
      ## phi is estimated as a constant: we need to insert the column "phi1" into res:
      res.phi <- rep(phi.limit, nres)
  }
  ## If both phi and lambda are constants, we don't want them to be marked on each other's graphs:
  if((!is.na(phi.limit)) & (!is.na(lambda.limit))) phi.limit <- lambda.limit <- NA
  ## If lambda.limit > 1, replace it with 1, because phi can't go above 1 anyway:
  if(!is.na(lambda.limit))
    if(lambda.limit>1) lambda.limit <- 1
  
  ## ------------------------------------------------------------------------------------------------
  ## Set colours: can be changed into parameters later if required:
  true.colour <- "red2"
  est.colour <- "lightblue"
  calc.colour <- "peachpuff"
  piggyback.colour <- "snow2"  ## parameters estimated but not in their native slots
  const.colour <- "brown4"
  mean.colour <- "black"
  timeaxis.colour <- "rosybrown"
  limit.colour <- "blue"
  
  ## If lambda has been requested, but it isn't a lambda-model, just ignore lambda by deleting it from plotwhat:
  if(("lambda" %in% plotwhat) & (!setup$lambdamodel)) plotwhat <- plotwhat[plotwhat!="lambda"]
  
  ## --------------------------------------------------------------------------------------------------------------
  ## FIGURE LAYOUT:
  ## Layout in n.plotcol columns with the required number of plots (use equal spacing each), and plot across
  ## rows:
  nplot <- length(plotwhat)
  nslot <- ceiling(nplot/n.plotcol)*n.plotcol
  layout(matrix(1:nslot, nrow=ceiling(nplot/n.plotcol), ncol=n.plotcol, byrow=T), widths=rep(1, nslot))
  
  ## ------------------------------------------------------------------------------------------------
  ## Set up the title and axis labels for each parameter:
  titlevec <- c(p=expression(bold("Capture probability, "*p[t])),
                phi=expression(bold("Annual survival, "*phi)),
                pent=expression(bold("Entry proportion, "*p[ent(t)])),
                N=expression(bold("Superpopulation, "*N[s])),
                lambda=expression(bold("Annual growth rate, "*lambda)))
  labelvec <- c(p=expression("Capture probability, "*p), phi=expression("Survival probability, "*phi),
                pent=expression("Entry proportion, "*p[ent]), N=expression("Superpopulation size, "*N[s]),
                lambda=expression("Annual growth rate, "*lambda))
  
  ## Set up bottomline text: options correspond to column names returned from
  ## process.onepar.func()$summary.table:
  bottomline.options <- c("", "empiricalSD", "empiricalCV", "relativeBias", "rootMeanSqError",
                          "meanEst", "meanSE", "meanEstCV", "medCIwidth")
  bottomline.descrip <- c("", "empirical SD :", "empirical %CV :", "% relative bias :", "RMSE :",
                          "mean estimate :", "mean SD :", "mean %CV :", "median CI width :")
  names(bottomline.descrip) <- bottomline.options
  if(!(bottomline.text %in% bottomline.options)) bottomline.text <- ""
  
  ## ------------------------------------------------------------
  ## Set up the plot. Default mar is (5, 4, 4, 2) in order c(bottom, left, top, right).
  par(mar=c(5, 3, 1, 1), mgp=c(3, 1, 0))
  
  ## ------------------------------------------------------------------------------------------------
  ## FUNCTION TO PLOT A SINGLE PANEL: PARAMETER 'PAR'
  ## ------------------------------------------------------------------------------------------------
  onepar.boxplot <- function(par){
    ## The parameter type par can be "p", "phi", "pent", "lambda", or "N".
    
    ## Process results for this parameter type: the following command creates summaries of how
    ## each parameter in the type is treated, estimate summaries, and confidence intervals where
    ## available:
    par.processed <- process.onepar.func(res.clean=res, res.setup=setup, partype=par, conf=conf,
                                         collapse.single=collapse.single, impose.01limits=impose.01limits)
    
    ## Extract components of par.processed:
    parnames <- par.processed$parnames  ## all names of parameters with the type specified by par
    par.simvec <- par.processed$simvals  ## true (simulated) values
    par.est.df <- par.processed$par.est.df  ## data frame of all estimates, and their variances where available
    ci.out <- par.processed$ci.out ## data frame of all available CIs and coverage results
    par.out <- par.processed$summary.table  ## summary table of estimator performance for this par-type
    any.ci <- sum(par.out$CIavail)  ## any.ci is 0 if there are no CI details to plot, >0 otherwise.
    
    ## The possibilities for modelled par's are:
    ## - estimated via its own time-slot: plot these in est.colour;
    ## - estimated via a different time-slot: plot these in piggyback.colour;
    ## - fixed as constant; plot these in const.colour;
    ## - derived from other parameters; plot these in calc.colour, e.g. pents derived from lambdas,
    ## or pent[1] = 1 - sum of other pents.
    ## Components of par.processed specify which are which:
    ##     -  par.processed$summary.table$treatment gives codes "Estimated", "Estimated via ...",
    ##       "Fixed", and "Derived";
    ##     - par.processed$which.est, par.processed$which.estvia, par.processed$which.fixed, and
    ##       par.processed$which.derived give the explicit members of par.processed$parnames
    ##       that have each treatment.  For example, we could have par.processed$which.estvia as below:
    ##              phi2  phi3
    ##                2       3
    ##       meaning that parameters phi2 and phi3, in positions 2 and 3 of parnames, are estimated via
    ##       another parameter - in other words they are piggy-back parameters.
    
    ## ---------------------------------------------------------------------------------------------
    ## Sort out the colours for the different boxes on the plot and the lines representing mean estimates:
    nboxes <- length(parnames)  ## Number of boxes to plot on this boxplot panel.
    box.colours <- rep("", nboxes)
    box.colours[par.processed$which.est] <- est.colour
    box.colours[par.processed$which.estvia] <- piggyback.colour
    box.colours[par.processed$which.derived] <- calc.colour
    box.colours[par.processed$which.fixed] <- const.colour
    mean.colours <- rep(mean.colour, nboxes)
    mean.colours[par.processed$which.fixed] <-  const.colour
    mean.lwd <- rep(2, nboxes)
    mean.lwd[par.processed$which.fixed] <- 3
    
    ## ---------------------------------------------------------------------------------------------
    ## Set up the plotting space:
    allvals <- c(unlist(par.est.df[parnames]), par.simvec)
    y.lo <- min(allvals) - textspace[1]*diff(range(allvals))
    y.hi <- max(allvals) + textspace[2]*diff(range(allvals))
    ## Special case: if allvals contains just a single value:
    if(y.lo==y.hi){
      y.tmp <- range(pretty(c(y.lo, y.hi), n=10))
      y.lo <- y.tmp[1]
      y.hi <- y.tmp[2]
    }
    par.ylim <- c(y.lo, y.hi)
    
    ## ------------------------------------------------------------------------------------------
    ## Set vertical levels for text:
    incr <- (y.hi - y.lo)/15
    y.title <- y.hi-0.5*incr
    if(citext){
      y.mean <- y.hi - 2*incr
      y.CIcov <- y.hi - 3*incr
      y.bottomline <-  y.lo + 1.1*incr
      y.limit <- y.lo + 0.1*incr
      ## Set limits for drawing the gridlines so they don't obscure the text:
      y.gridlim.hi <- max(c(y.CIcov-incr, max(allvals)))
      y.gridlim.lo <- min(c(y.bottomline+incr, min(allvals)))
    }
    else{
      y.gridlim.hi <- min(c(y.title - incr, max(allvals)))
      y.gridlim.lo <- max(c(y.lo + incr, min(allvals)))
    }
    
    ## ------------------------------------------------------------------------------------------------
    ## Draw the boxplot: start with a blank plot, then put gridlines on if wanted, then
    ## finally plot the boxplot over the top.
    ## Blank plot:
    plot(-1, xaxt="n", ylim=par.ylim, xlim=c(0.4, nboxes+0.6), xlab="", ylab="", cex.axis=cex.axis,
         type="n")
    ## Gridlines if wanted:
    gridspace.par <- eval(parse(text=paste0("gridspace.", par)))
    if(!is.na(gridspace.par)){
      grid.hvals <- seq(y.gridlim.lo-y.gridlim.lo%%gridspace.par, y.gridlim.hi, by=gridspace.par)
      grid.hvals <- grid.hvals[(grid.hvals >= y.gridlim.lo ) & (grid.hvals <= y.gridlim.hi)]
      if(length(grid.hvals)>0) abline(h=grid.hvals, col="grey70", lty=3)
    }
    ## Plot the boxes:
    bp <- boxplot(par.est.df[parnames], names = parnames, ylim = par.ylim,
                  pars=list(medlty="blank", boxfill=box.colours, outwex = 0.3, boxwex = 0.6,
                            outpch=NA, outlty="solid", staplewex=0.6, staplelwd=1, whisklty=1, whisklwd=0.4,
                            outlwd=0.4), xaxt="n", yaxt="n", add=T)
    #, cex.axis=cex.axis, main=titlevec[par], cex.main=cex.main)
    if(nboxes>1)
      axis(side=1, at=1:nboxes, labels=parnames, cex.axis=cex.axis, line=0, tick=T)
    
    ## Mark the mean values on the plot:
    for(i in 1:nboxes){
      lines(c(i - 0.4, i + 0.4), rep(mean(par.est.df[[parnames[i]]]), 2), lwd = mean.lwd[i],
            col=mean.colours[i])
      lines(c(i - 0.5, i + 0.5), rep(par.simvec[parnames[i]], 2), lwd = 1, col=true.colour)
    }
    
    ## ------------------------------------------------------------------------------------------
    ## Mark the time axis if wanted:
    if(time.axis & (nboxes>1)){
      if(par=="phi") axis(side=1, at=seq(0.5, nboxes+0.5),
                          labels=setup$ModelTable$time, line=3, col=timeaxis.colour,
                          col.axis=timeaxis.colour, cex.axis=cex.axis)
      else axis(side=1, at=1:nboxes, labels=modelTable$time, line=3, col=timeaxis.colour,
                col.axis=timeaxis.colour, cex.axis=cex.axis)
    }
    
    ## --------------------------------------------------------------------------------
    ## Put in titles for all plots inside the plots:
    text((1+nboxes)/2, y.title, labels=titlevec[par], cex=cex.main)
    
    ## ----------------------------------------------------------------------------------------------
    ## Put in limits where they apply:
    if(plot.limits){
      ## Phi, p, pent limits:
      if(par %in% c("phi", "p", "pent")){
        ## Mark in the lower limit 0 if it doesn't interfere with the text:
        if(y.gridlim.lo<0) abline(h=0, col=limit.colour)
        ## If it's a lambda-model, and lambda is constant (not estimated), and the constant
        ## lambda is <1, then phi is bounded above by lambda for all sims;
        ## otherwise, (and for p's), mark the upper limit 1 if it doesn't interfere with the text:
        if(par=="phi" & (!is.na(lambda.limit))) {
          abline(h=lambda.limit, col=limit.colour, lwd=2, lty=2)
          if(citext) text(1, y.limit, bquote("upper limit = "*lambda*" ="~.(lamconstval),
                                             list('lamconstval'=format(lambda.limit))))
        }
        else if(y.gridlim.hi > 1)
          abline(h=1, lwd=2, col=limit.colour)
      }
      ## Lambda limits:
      if(par=="lambda"){
        ## Mark in the lower limit 0 if it doesn't interfere with the text:
        if(y.gridlim.lo < 0) abline(h=0, col=limit.colour, lwd=2)
        ## If phi is constant, lambda is bounded below by phi for all sims;
        if(!is.na(phi.limit)) if((y.gridlim.hi > phi.limit) & citext){
          abline(h=phi.limit, col=limit.colour, lwd=2, lty=2)
          text(1, y.limit, bquote("lower limit = "*phi*" ="~.(phiconstval),
                                  list('phiconstval'=format(phi.limit))), cex=cex.limit)
        } ## End if phi is constant so lambda limits apply
      }  ## End par=="lambda"
    }  ## End if(plot.limits)
    
    ## -----------------------------------------------------------------------------------------------------------------
    ## CONFIDENCE INTERVALS:
    ## Do these only for parameters that are estimated or piggy-back estimated, and only if citext=T:
    if(citext & (any.ci>0)){
      ## Text behaves differently according to whether nboxes=1 or nboxes>1:
      ## if nboxes=1, include a line for mean vs true estimate, and write more text.
      if(nboxes==1){
        ## Extract the desired summary statistics for the single parameter for which CIs are available:
        mean.par <- par.out$meanEst[par.out$CIavail==1]
        true.par <- par.out$simval[par.out$CIavail==1]
        ## CI coverage is rounded to the nearest integer percentage:
        cicover.par <- round(100*par.out$CIcover[par.out$CIavail==1])
        
        ## Determine formatting length.
        ## Write true.par in scientific format and find the exponent e.
        ## If e<2 (including negative values), round to 2-e decimal places, UNLESS
        ## the true parameter is between 1 and 2, in which case we add an extra decimal place
        ## to give lambda values the same precision as phi values.
        truepar.sci <- strsplit(format(true.par, scientific=T), split="e")
        truepar.eval <- as.numeric(truepar.sci[[1]][2])
        if((true.par>=1) & (true.par<=2)) round.val <- 2 - truepar.eval + 1
        else if(truepar.eval<1) round.val <- 2 - truepar.eval
        else round.val <- 0
        text(1, y.mean, paste0("mean=", round(mean.par, round.val),
                               "  true=", format(round(true.par, round.val), nsmall=round.val)), cex=cex.text)
        ## CI coverage text:
        text(1, y.CIcov, paste0("CI coverage: ", cicover.par, "%"), cex=cex.text)
        
        ## Bottomline text, if desired:
        if(bottomline.text!=""){
          blout.par <- par.out[[bottomline.text]][par.out$CIavail==1]
          ## If bottomline.text is one of relativeBias, empiricalCV, meanEstCV, multiply by 100
          ## to get a percentage and round to 1 dp:
          if(bottomline.text %in% c("relativeBias", "empiricalCV", "meanEstCV")){
            blout.par <- blout.par*100
            round.bottomline <- 1
          }
          else round.bottomline <- round.val
          
          ## Add the bottomline text:
          text(1, y.bottomline,
               paste(bottomline.descrip[bottomline.text], round(blout.par, round.bottomline)),
               cex=cex.text)
        }  ## End of bottomline text
        
      }  ## End of nboxes=1
      else{
        ## If nboxes>1, run through the CI-available parameters one by one and
        ## plot the CI coverage above and the bottomline text below the corresponding box:
        for(cipar in par.out$parname[par.out$CIavail==1]){
          ## Find which box this parameter corresponds to:
          xcoord.par <- which(parnames==cipar)
          ## CI coverage text: to nearest integer percent:
          cicover.par <- round(100*par.out$CIcover[par.out$parname==cipar])
          text(xcoord.par, y.CIcov, paste0(cicover.par, "%"), cex=cex.text)
          ## CI width text: the many-par boxes are probabilities (phi, p, or pent) so quote the
          ## bottom-line text to 1 dp if it's a percent, and 2 dp otherwise, as generic choices:
          
          if(bottomline.text!=""){
            blout.par <- par.out[[bottomline.text]][par.out$parname==cipar]
            ## If bottomline.text is one of relativeBias, empiricalCV, meanEstCV, multiply by 100
            ## to get a percentage and round to 1 dp:
            if(bottomline.text %in% c("relativeBias", "empiricalCV", "meanEstCV")){
              blout.par <- blout.par*100
              round.bottomline <- 1
            }
            else round.bottomline <- 2
            text(xcoord.par, y.bottomline,
                 format(round(blout.par, round.bottomline), nsmall=round.bottomline),
                 cex=cex.text)
          } ## End of bottomline text
        }  ## End of parameter cipar
      } ## End of nboxes>1
      
    } ## End of citext=T and some CI-available parameters to print CI text for.
    
  }  ## End of onepar.boxplot
  
  ## ------------------------------------------------------------------------------------------------
  ## CREATE PLOTS FOR EACH PARAMETER:
  sapply(1:nplot, function(i) onepar.boxplot(plotwhat[i]))
  return(res)
}

##############################################################

popan.ci.plot <- function(res.raw, plotwhat=c("phi", "lambda", "N"), conf=0.95, collapse.single=T,
                          gridspace.phi=0.05, gridspace.p=0.2, gridspace.pent=0.2, gridspace.N=100,
                          gridspace.lambda=0.05,
                          n.plotcol=2, cex.main=1.8, cex.axis=1, cex.points=1, textspace=c(0.1, 0.3),
                          threshold.negvar=1e-6, impose.01limits=T){
  ## popan.ci.plot 24/11/15
  ## Takes argument res.raw: results directly out of popan.sim.wrap, not yet cleaned.
  ##
  ## If collapse.single=T, then parameter types that are fitted with a single parameter AND that
  ## are simulated with a single value are collapsed to a single plot.  For example, if both
  ## the simulation model and the fitted model are lambdamodels or constant-phi models ,
  ## then there is only one value of phi simulated and one value fitted, so collapse.single=T will plot
  ## just a single boxplot for all phi values.  If collapse.single=F then a separate (identical) plot
  ## will be plotted for every phi-slot in this case.
  ##
  ## Any results with any variances < (-threshold.negvar) are removed entirely before any plotting is done.
  ##
  ## n.plotcol is the number of columns desired for the plot.
  ##
  ## textspace is the amount of space at bottom, top of each panel for plotting the title text.
  
  ## Housekeeping:
  ## Clean results, removing any rows that are non-converged (code not in (1, 2, 3) or flag=1),
  ## or any rows that have any negative variances more negative than threshold.negvar; negative
  ## variances within threshold.negvar are reset to 0:
  res <- clean.result.func(resdf=res.raw, threshold.negvar=threshold.negvar)
  nres <- nrow(res)
  setup <- attributes(res)$setup
  
  ## Set colours: can be changed into parameters later if required:
  true.colour <- "red2"
  est.colour <- "grey"
  ci.single.colour <- "slateblue"  ## CI endpoints for parameters that are estimated as singles.
  ci.tied.colour <- "darkgreen" ## CI endpoints for parameters that are either piggyback or piggybacked-off.
  out.colour <- "black"  ## CI endpoints for CIs that don't contain the true value.
  
  ## --------------------------------------------------------------------------------------------------------------
  ## Find parameters to plot: we plot a panel for every parameter that is estimated or piggy-back
  ## estimated. Piggy-back parameters have the corresponding columns added to res before plotting.
  ## We ignore any requests for parameters that don't have a corresponding column var.*
  ## in res: this means constant or calc parameters.
  
  ## If lambda has been requested, but it isn't a lambda-model, ignore lambda by deleting it from plotwhat:
  ## this prevents errors later:
  if(("lambda" %in% plotwhat) & (!setup$lambdamodel)) plotwhat <- plotwhat[plotwhat!="lambda"]
  
  ## parnames.plot contains the names of all panels to plot, and colvec.plot gives the colour for each panel.
  ## partypes.plot gives the types of all parameters - e.g. the parameter with name phi1 has type phi.
  parnames.plot <- colvec.plot <- partypes.plot <- NULL
  for(partype in plotwhat){
    if(partype=="lambda") parnames <- "lambda"
    else if(partype=="N") parnames <- "N"
    else parnames <- eval(parse(text=paste0("setup[[\'", partype, "names\']]")))
    ## Find the true (simulated) values:
    par.simvec <- setup$truevals[parnames]
    
    ## The possibilities for modelled par's are:
    ## - estimated via its own time-slot: plot these in est.colour;
    ## - estimated via a different time-slot: plot these in piggyback.colour;
    ## - fixed as constant; plot these in const.colour;
    ## - derived from other parameters; plot these in calc.colour, e.g. pents derived from lambdas,
    ## or pent[1] = 1 - sum of other pents.
    ## The allparvec results show how each parameter is dealt with in the model, which
    ## we translate to vector par.details.
    ## In the example above, par.details would be:
    ##    p1    p2    p4    p5
    ##  "p1" "0.2"  "p1"  "p5"
    par.details <- setup$allparvec[parnames]
    
    ## If collapse.single=T, determine whether the boxplots can be collapsed to a single boxplot
    ## for this parameter type.  This only applies if all par.details are the same AND if all par.simvec
    ## are the same.
    if(collapse.single & all(par.simvec==par.simvec[1]) & all(par.details==par.details[1])){
      ## if(length(parnames)>1) catline("Collapsing to single plot for parameter ", partype)
      par.simvec <- par.simvec[1]
      par.details <- par.details[1]
      parnames <- parnames[1]
    }
    
    ## Find the parameters that are estimated in their own time-slots: these are the ones
    ## for which par.details == names(par.details):
    which.par.est <- which(par.details==names(par.details))
    
    ## Find piggy-back parameters: these have par.details != names(par.details),
    ## par.details != "calc", and is.na(as.numeric(par.details)):
    which.par.piggy <- which( (par.details !=names(par.details)) & (par.details != "calc") &
                                is.na(suppressWarnings(as.numeric(par.details))))
    
    ## If there are any piggy-back parameters, we want to use colour ci.tied.colour
    ## for both the piggy parameters and the est parameters that they are piggybacking from.
    ## We also need to add a column and var.column to res for ease of plotting all parnames.plot
    ## in the same way.
    colvec.type <- rep(ci.single.colour, length(parnames))
    names(colvec.type) <- parnames
    if(length(which.par.piggy)>0){
      ## Sort out colours for both the piggy-back and the piggybacked-from parameter:
      colvec.type[which.par.piggy] <- ci.tied.colour
      colvec.type[par.details[which.par.piggy]] <- ci.tied.colour
      ## Add the necessary columns to res: the parameter estimates and the variance estimates:
      for(i in which.par.piggy){
        res[parnames[i]] <- res[[par.details[parnames[i]]]]
        res[paste0("var.", parnames[i])] <- res[[paste0("var.", par.details[parnames[i]])]]
      }
    }
    ## Add sorted vector of est, piggy parameters to parnames.plot:
    parnames.keep <- parnames[c(which.par.est, which.par.piggy)]
    parnames.order <- naturalorder(parnames.keep)
    parnames.plot <- c(parnames.plot, parnames.keep[parnames.order])
    colvec.plot <- c(colvec.plot, colvec.type[parnames.keep][parnames.order])
    partypes.plot <- c(partypes.plot, rep(partype, length(parnames.order)))
  }  ## End partype
  if(length(parnames.plot)==0) return(NULL)
  
  ## --------------------------------------------------------------------------------------------------------------
  ## FIGURE LAYOUT:
  ## Layout in n.plotcol columns with the required number of rows: plot one row at a time.
  nplot <- length(parnames.plot)
  nslot <- ceiling(nplot/n.plotcol)*n.plotcol
  layout(matrix(1:nslot, nrow=ceiling(nplot/n.plotcol), ncol=n.plotcol, byrow=T), widths=rep(1, nslot))
  ## Set up the plot. Default mar is (5, 4, 4, 2) in order c(bottom, left, top, right).
  par(mar=c(3, 2, 2, 2), mgp=c(2, 0.8, 0))
  
  ## --------------------------------------------------------------------------------------------------------------
  ## FIND THE CONFIDENCE INTERVALS:
  ## Find all CIs for parnames.plot all in one go:
  lognorm.plot <- (parnames.plot=="N")
  names(lognorm.plot) <- parnames.plot
  truevals <- setup$truevals[parnames.plot]
  ci.res <- ci.func(res, par.names=parnames.plot, conf=conf, lognormal=lognorm.plot,
                    query.vals=truevals, impose.01limits=impose.01limits)
  ## Combine all estimates, CIs, and other information into a single data frame called allres:
  allres <- data.frame(res, ci.res)
  
  ## ------------------------------------------------------------------------------------------------
  ## FUNCTION TO PLOT A SINGLE PANEL: PARAMETER 'PAR'
  ## ------------------------------------------------------------------------------------------------
  onepanel.ciplot <- function(parnm, partyp, colpt, trueval){
    ## parnm is not the plot type (e.g. "phi") like it is in the other plotting functions, because
    ## here we have a separate panel for every parameter, e.g. parnm="phi1", "phi2", etc.
    cipar.df <- allres[, c(parnm, paste0(c("cilow.", "cihi.", "cicode.", "cover."), parnm))]
    names(cipar.df) <- c("est", "cilow", "cihi", "cicode", "cover")
    cipar.df <- cipar.df[order(cipar.df$est),]
    
    allvals <- c(unlist(cipar.df[c("est", "cilow", "cihi")]), trueval)
    y.lo <- min(allvals) - textspace[1]*diff(range(allvals))
    y.hi <- max(allvals) + textspace[2]*diff(range(allvals))
    par.ylim <- c(y.lo, y.hi)
    
    ## ------------------------------------------------------------------------------------------
    ## Set vertical levels for text:
    incr <- (y.hi - y.lo)/15
    y.title <- y.hi - incr
    y.gridlim.hi <- min(c(y.title - incr, max(allvals)))
    y.gridlim.lo <- max(c(y.lo + incr, min(allvals)))
    
    ## ------------------------------------------------------------------------------------------
    ## Plot the CIs ordered by the estimated value:
    plot(1:nres, cipar.df$cilow, pch=16, ylim=par.ylim, xlab="Simulation index",
         ylab="Confidence interval", cex=cex.points, col=colpt,
         cex.axis=cex.axis, cex.lab=cex.axis, xlim = c(1, attributes(res)$n_datasets))
    
    ## Gridlines if wanted:
    gridspace.par <- eval(parse(text=paste0("gridspace.", partyp)))
    if(!is.na(gridspace.par)){
      grid.hvals <- seq(y.gridlim.lo-y.gridlim.lo%%gridspace.par, y.gridlim.hi, by=gridspace.par)
      grid.hvals <- grid.hvals[(grid.hvals >= y.gridlim.lo ) & (grid.hvals <= y.gridlim.hi)]
      if(length(grid.hvals)>0) abline(h=grid.hvals, col="grey70", lty=3)
    }
    
    ## Draw in the true value:
    abline(h=trueval, col=true.colour, lwd=2)
    ## Upper CIs and estimates:
    points(1:nres, cipar.df$cihi, pch=16, col=colpt, cex=cex.points)
    points(1:nres, cipar.df$est, pch=16, col=est.colour, cex=cex.points)
    
    ## Overplot any points that are outside the CI:
    points((1:nres)[cipar.df$cover==0], cipar.df$cilow[cipar.df$cover==0], pch=21, col=out.colour,
           cex=cex.points, bg=out.colour)
    points((1:nres)[cipar.df$cover==0], cipar.df$cihi[cipar.df$cover==0], pch=21, col=out.colour,
           cex=cex.points, bg=out.colour)
    
    ## Title:
    ## Determine formatting length.
    ## Write trueval in scientific format and find the exponent e.
    ## If e<2 (including negative values), round to 2-e decimal places, UNLESS
    ## the true parameter is between 1 and 2, in which case we add an extra decimal place
    ## to give lambda values the same precision as phi values.
    trueval.sci <- strsplit(format(trueval, scientific=T), split="e")
    trueval.eval <- as.numeric(trueval.sci[[1]][2])
    if((trueval>=1) & (trueval<=2)) round.val <- 2 - trueval.eval + 1
    else if(trueval.eval<1) round.val <- 2 - trueval.eval
    else round.val <- 0
    text((nres+1)/2, y.title, paste0(parnm, " :  True = ", round(trueval, round.val),
                                     " ; CI = ", round(sum(cipar.df[, "cover"])/attributes(res)$n_datasets*100), "%"), cex=cex.main)
    
  }  ## end of onepar.ciplot
  
  ## ------------------------------------------------------------------------------------------------
  ## CREATE PLOTS FOR EACH PARAMETER:
  sapply(1:nplot, function(i) onepanel.ciplot(parnm=parnames.plot[i], partyp=partypes.plot[i],
                                              colpt=colvec.plot[i], trueval=truevals[i]))
  
  return(allres)
}

##############################################################

multiproj.power.plot <- function(projects=c("P11", "P13"), partype="lambda", timelabels=NULL,
                                 queryval=1, conf=0.95,
                                 n.plotcol=NULL, gridspace=NA,
                                 cex.main=1.8, cex.axis=1, cex.points=1, textspace=c(0.1, 0.3),
                                 threshold.negvar=1e-6, impose.01limits=T, plotit=T){
  ## multiproj.power.plot 25/11/15
  ## This function plots power results for multiple projects but for just one parameter type and one
  ## queried value.  Any projects specified that have not been run are ignored.
  ##
  ## timelabels can be NULL or a vector of time labels, e.g. timelabels=c(2001, 2002).
  ## If partype="lambda" or partype="N", timelabels will be ignored but the time range that the parameter
  ## spans will be printed in the output.
  ## If partype="phi", "pent", or "p", then if timelabels=NULL the default behaviour is to use the
  ## timelabel corresponding to the first estimated parameter of the correct type of the first project
  ## in the projects vector.  For example, in case "pent" the pent1 parameter will be calculated, so we'll
  ## use the timelabel corresponding to (say) pent2 in the projects[1] project.
  ## If timelabels is not NULL, and partype="phi", "pent", or "p", then results are reported for all projects that
  ## have that parameter estimated (either native or piggyback) in the specified timelabels.
  ## Derived parameters that are "calculated" don't have CIs and will not be showcased by this function.
  ##
  ## Any results with any variances < (-threshold.negvar) are removed entirely before any plotting is done.
  ##
  ## n.plotcol is the number of columns desired for the plot, for each of the times in timelabels.  If NULL,
  ## it will default to the number of projects that do have something to plot for the specified timelabels.
  ##
  ## textspace is the amount of space at bottom, top of each panel for plotting the title text.
  ##
  ## Note that this function does not distinguish between different types of parameter estimates:
  ## for example parameters estimated in their own time-slots versus piggy-back estimates.
  
  ## First extract which projects have been run:
  proj.info <- sapply(projects, getproj)
  projects <- projects[as.numeric(proj.info["run",])==1]
  if(length(projects)==0) return(NULL)
  if(!is.null(timelabels)) timelabels <- as.character(timelabels)
  
  ## Now create the list of results objects and model formulations:
  res.list <- setup.list <- list()
  for(proj in projects) {
    res.list[[proj]] <- getenv(paste0("result.", proj))
    ## Clean results all in one go at the beginning:
    ## Remove any rows that are non-converged (code not in (1, 2, 3) or flag=1),
    ## or any rows that have any negative variances more negative than threshold.negvar; negative
    ## variances within threshold.negvar are reset to 0:
    res.list[[proj]] <- clean.result.func(resdf=res.list[[proj]], threshold.negvar=threshold.negvar)
    setup.list[[proj]] <- attributes(res.list[[proj]])$setup
  }
  
  ## ------------------------------------------------------------------------------------------------------------------------
  ## Create details.by.time.list, which is a list with a component for each time-label.
  ## The component for timelabel "timel" is a data frame with columns "Project",
  ## "Parameter", "Description" which give the project name, the name of the parameter corresponding
  ## to that project for time timel, and the description of the parameter.
  ## EXAMPLES: if partype="lambda" then we might have the following data frame (all characters):
  ## details.by.time.list$all:
  ##       Project  Description                         Parameter
  ##       "P1"       "lambda: 2001 to 2015"     "lambda"
  ##       "P2"       "lambda: 2010 to 2015"     "lambda"
  ##
  ## If partype="phi" then we might have the following for 2002, including all projects that estimated
  ## a phi parameter for 2002:
  ## details.by.time.list$`2002` :
  ##       Project  Description  Parameter
  ##       "P1"       "phi: 2002"  "phi2"
  ##       "P3"       "phi: 2002"  "phi4"
  
  ## Separate this into different parameter types:
  if(partype %in% c("lambda", "N")){
    ## If partype="lambda", keep the projects that are lambdamodels and have lambda estimated:
    ## it suffices to look for the column "var.lambda" (or even just "lambda") in the results output.
    ## Similarly, if partype="N", we only need projects that have a column "var.N" in the results.
    varname.par <- paste0("var.", partype)
    is.par.estimated <- unlist(lapply(res.list, function(x)(varname.par %in% names(x))))
    proj.keep <- names(is.par.estimated)[is.par.estimated]
    
    ## For consistency with other settings, define timelabels="all":
    timelabels <- "all"
    
    ## Now create details.by.time.list, which will just have the single component for timelabel "all"
    ## in this case:
    ## details.by.time.list$all will be a data frame looking something like this:
    ##       Project  Description                         Parameter
    ##       "P1"       "lambda: 2001 to 2015"     "lambda"
    ##       "P2"       "lambda: 2010 to 2015"     "lambda"
    
    ## First find the descriptions for each project, i.e. the years covered:
    timedescrip <- character(length(proj.keep))
    names(timedescrip) <- proj.keep
    for(proj in proj.keep){
      setup.proj <- setup.list[[proj]]
      alltimes.proj <- setup.proj$ModelTable$time
      timedescrip[proj] <- paste0(partype, " ", alltimes.proj[1], " - ",
                                  alltimes.proj[length(alltimes.proj)])
    }
    
    details.by.time.list <- list(all = data.frame(Project=proj.keep, Description=timedescrip,
                                                  Parameter=rep(partype, length(proj.keep)), stringsAsFactors=F))
    
    
  } ## End partype "lambda" or "N"
  ## ---------------------------------------------------------------------------------------------
  else if(partype %in% c("phi", "pent", "p")){
    ## ModelTable has columns phivec, pvec, or pent: define the correct name:
    if(partype %in% c("phi", "p")) par.colname <- paste0(partype, "vec") else par.colname <- "pent"
    
    ## If timelabels is NULL, use the timelabel from projects[1] corresponding to the first estimated
    ## instance of this type of parameter:
    if(is.null(timelabels)){
      proj1.details <- setup.list[[projects[1]]]$ModelTable
      timelabels <- as.character(proj1.details$time[which( (proj1.details[[par.colname]] !="calc") &
                                                             (!is.na(proj1.details[[par.colname]])))[1] ])
      ## If there isn't any such timelabel available (e.g. the first project doesn't have any estimated
      ## parameters of this type), stop with an error:
      if(is.na(timelabels)) stop("Specify which time-labels the parameter should apply to.")
    }
    ## We now have a value or vector for timelabels.
    
    ## Now create details.by.time.list, which has a component for every time-label, e.g.
    ## details.by.time.list$`2002` :
    ##       Project  Description  Parameter
    ##       "P1"       "phi: 2002"  "phi2"
    ##       "P3"       "phi: 2002"  "phi4"
    details.by.time.list <- vector("list", length(timelabels))
    names(details.by.time.list) <- as.character(timelabels)
    timelabels.keep <- NULL
    ## Go through timelabels one at a time constructing the corresponding component:
    for(timel in timelabels){
      ## Go through the projects and find any that estimate the partype parameter at timel:
      out.timel <- NULL
      for(proj in projects){
        setup.proj <- setup.list[[proj]]
        modeltab.timel.proj <- setup.proj$ModelTable[setup.proj$ModelTable$time==timel,, drop=F]
        ## parcol.timelabels.proj contains results like c("calc", "pent2") specifying whether
        ## (say) the pent parameter at the two specified times in timelabels is "calc" or estimated.
        param.timel.proj <- modeltab.timel.proj[[par.colname ]]
        ## If the parameter is estimated, its entry in par.column of modeltable will not be calc, NA,
        ## or a number, but it will match partype itself: for example, if partype="p", the parameter
        ## will be "p1" or "p3" say; if partype="phi" it will be "phi1", etc.
        is.est.timel.proj <- length(grep(partype, param.timel.proj))
        ## If the parameter is estimated, add it to out.timel:
        if(is.est.timel.proj>0)
          out.timel <- rbind(out.timel, c(Project=proj,
                                          Description=paste0(partype, " ", timel),
                                          Parameter=param.timel.proj))
      } ## End of projects for timel.
      ## If there are any results for timel, convert to a data-frame and put them into
      ## details.by.time.list[[timel]]:  (cater below for both out.timel=NULL and
      ## somehow being a data frame with 0 rows and 0 columns):
      if(!is.null(out.timel)) if(nrow(out.timel)>0) {
        details.by.time.list[[timel]] <- data.frame(out.timel, stringsAsFactors=F)
        timelabels.keep <- c(timelabels.keep, timel)
      }
    } ## End of timelabels timel.  The list details.by.time.list is now created.
    
    ## Finally redefine timelabels to be just the kept ones, and remove unwanted elements of
    ## details.by.time.list:
    timelabels <- timelabels.keep
    details.by.time.list <- details.by.time.list[timelabels.keep]
    
  }  ## End partype one of "p", "pent", "phi".
  
  ## -------------------------------------------------------------------------------------------------------------------------------
  ## Plot setup:
  ## Set colours: can be changed into parameters later if required:
  query.colour <- "blue"
  est.colour <- "grey"
  ci.colour <- "slateblue"  ## CI endpoint colour
  out.colour <- "black"  ## CI endpoints for CIs that don't contain the true value.
  
  ## -------------------------------------------------------------------------------------------------------------------------------
  ## SUMMARY AND PLOTTING FUNCTION
  ## Go through the final timelabels, one by one, return a results table and plot one screen for each one:
  one.timelabel.plot <- function(timel){
    ## Extract which projects we are plotting for:
    details.timel <- details.by.time.list[[timel]]
    nplot <- nrow(details.timel)
    
    ## Prepare output columns in details.timel to be returned at the end of this function:
    ## the CIbelow and CIabove columns contain the percentage of times the CI lies wholly below
    ## and wholly above the queried value, respectively.
    details.timel$QueriedValue <- rep(queryval, nplot)
    details.timel$CIbelow <- details.timel$CIabove <- details.timel$OverallPower <- rep(NA, nplot)
    
    if(plotit){
      ## --------------------------------------------------------------------------------------------------------------
      ## Figure layout: need to do this here at the top. Projects are plotted individually in a
      ## loop below.
      ## Layout in n.plotcol columns with the required number of rows, plotting across rows:
      if(is.null(n.plotcol)) n.plotcol <- nrow(details.timel)
      nslot <- ceiling(nplot/n.plotcol)*n.plotcol
      layout(matrix(1:nslot, nrow=ceiling(nplot/n.plotcol), ncol=n.plotcol, byrow=T),
             widths=rep(1, nslot))
      ## Set up the plot. Default mar is (5, 4, 4, 2) in order c(bottom, left, top, right).
      par(mar=c(5, 3, 3, 2), mgp=c(2, 0.8, 0))
    }
    
    ## --------------------------------------------------------------------------------------------------------------
    ## Extract confidence intervals for the required parameters for each project:
    nproj <- nrow(details.timel)
    for(i in 1:nproj){
      proj <- details.timel$Project[i]
      res.proj <- res.list[[proj]]
      descrip.proj <- details.timel$Description[i]
      ## Use the "query.vals" argument of ci.func to test power.  query.vals needs to have the
      ## correct name that matches the name of the parameter in this project: for example,
      ## if the parameter is phi2 for this project, it needs to be a named vector c(phi2=0.9) or
      ## suchlike:
      parname.proj <- details.timel$Parameter[i]
      query.proj <- queryval
      names(query.proj) <- parname.proj
      ## ciproj.df is the data frame of CI results: cihi, cilow, cover, cicode, est:
      ciproj.df <- ci.func(res.proj, par.names=parname.proj, conf=conf, lognormal=(partype=="N"),
                           query.vals=query.proj, include.estcolumns=T, impose.01limits=impose.01limits)
      ## Rename the columns of ciproj.df so we only need to use its generic names, because
      ## it's just for a single parameter:
      ciproj.df <- ciproj.df[, c(parname.proj, paste0(c("cilow.", "cihi.", "cicode.", "cover."), parname.proj))]
      names(ciproj.df) <- c("est", "cilow", "cihi", "cicode", "cover")
      ciproj.df <- ciproj.df[order(ciproj.df$est),]
      nres <- nrow(ciproj.df)
      
      ## Add the power results for this project to details.timel: the three columns below give percentage
      ## performance:
      powertab.proj <- table(factor(ciproj.df$cicode, levels=c("cib", "cia", "cic")))
      details.timel$CIbelow[i] <- powertab.proj["cib"] / attributes(res.proj)$n_datasets * 100
      details.timel$CIabove[i] <- powertab.proj["cia"] / attributes(res.proj)$n_datasets * 100
      details.timel$OverallPower[i] <- details.timel$CIbelow[i] + details.timel$CIabove[i]
      
      ## -----------------------------------------------------------------------------------
      ## Create the plot for Project i on the timelabel timel screen:
      if(plotit){
        ## ------------------------------------------------------------------------------------------
        ## Set vertical levels for text:
        allvals <- c(unlist(ciproj.df[c("est", "cilow", "cihi")]), queryval)
        y.lo <- min(allvals) - textspace[1]*diff(range(allvals))
        y.hi <- max(allvals) + textspace[2]*diff(range(allvals))
        par.ylim <- c(y.lo, y.hi)
        
        incr <- (y.hi - y.lo)/15
        y.title <- y.hi - incr
        y.gridlim.hi <- min(c(y.title - incr, max(allvals)))
        y.gridlim.lo <- max(c(y.lo + incr, min(allvals)))
        
        ## ------------------------------------------------------------------------------------------
        ## Plot the CIs ordered by the estimated value:
        plot(1:nres, ciproj.df$cilow, pch=21, ylim=par.ylim, col=ci.colour, xlab="Simulation index",
             ylab="Confidence interval", cex=cex.points, bg=ci.colour, cex.axis=cex.axis,
             cex.lab=cex.axis, xlim = c(1, attributes(res.proj)$n_datasets))
        
        ## Gridlines if wanted:
        if(!is.na(gridspace)){
          grid.hvals <- seq(y.gridlim.lo-y.gridlim.lo%%gridspace, y.gridlim.hi,
                            by=gridspace)
          grid.hvals <- grid.hvals[(grid.hvals >= y.gridlim.lo ) & (grid.hvals <= y.gridlim.hi)]
          if(length(grid.hvals)>0) abline(h=grid.hvals, col="grey70", lty=3)
        }
        
        ## Draw in the queried value:
        abline(h=queryval, col=query.colour, lwd=2)
        ## Upper CIs and estimates:
        points(1:nres, ciproj.df$cihi, pch=21, col=ci.colour, cex=cex.points, bg=ci.colour)
        points(1:nres, ciproj.df$est, pch=16, col=est.colour, cex=cex.points)
        
        ## Overplot any points that are outside the CI:
        points((1:nres)[ciproj.df$cover==0], ciproj.df$cilow[ciproj.df$cover==0], pch=21,
               col=out.colour, cex=cex.points, bg=out.colour)
        points((1:nres)[ciproj.df$cover==0], ciproj.df$cihi[ciproj.df$cover==0], pch=21,
               col=out.colour, cex=cex.points, bg=out.colour)
        ## Add the title text:
        text((nres+1)/2, y.title, paste0(proj, ": ", descrip.proj), cex=cex.main)
      }  ## End if(plotit)
      
    }  ## End project i
    ## Pause between graphics if wanted:
    ## if(timel != timelabels[length(timelabels)]) readline("Press return for next plot...")
    
    ## Return the details for time timel, ordered with the highest overall power at the top:
    details.timel <- details.timel[order(details.timel$OverallPower, decreasing=T),]
    return(details.timel)
  }  ## END OF PLOTTING FUNCTION one.timelabel.plot
  ## ------------------------------------------------------------------------------------------------
  
  ## ------------------------------------------------------------------------------------------------
  ## CREATE PLOTS AND SUMMARY FOR EACH TIME LABEL:
  power.res <- lapply(timelabels, one.timelabel.plot)
  names(power.res) <- timelabels
  return(power.res)
  
}

##############################################################


checkModel.func <- function(model){
  ## checkModel.func checks the model supplied for any errors, e.g. parameters out of
  ## range or inconsistent with each other.  Returns a list with components checkOK (true or false
  ## to say whether the model check was successful) and msg which is the error message to be
  ## returned.
  ## If there is no error, this function returns a blank message, but a further message will be generated
  ## back in the main function create.model.func() about whether the model already exists or
  ## whether the save has been successful.
  ## This function is called from ModelBuilder in ModelUI/server.R.
  
  ## -----------------------------------------------------------------------------------------------------------
  ## Check there are at least two surveys (common to all model types):
  ## -----------------------------------------------------------------------------------------------------------
  if(sum(model$paramdf$timeopt)<2)
    return(list(checkOK=F,
                msg=c("Error in model: you need at least two selected surveys.",
                      "Your model has NOT been saved. Correct the error, and try again.")))
  
  ## -----------------------------------------------------------------------------------------------------------
  ## Check N (common to all model types):
  ## -----------------------------------------------------------------------------------------------------------
  is.N.number <- (!is.na(suppressWarnings(as.numeric(model$superpopn))))
  
  ## If N is a number, find what it is:
  if(is.N.number){
    N.number <- suppressWarnings(as.numeric(model$superpopn))
    if(N.number < 0)
      return(list(checkOK=F,
                  msg=c("Error in model: the 'N<sub>s</sub>' entry must be either 'N', or a number greater than 0.",
                        "Your model has NOT been saved. Correct the error, and try again.")))
  }
  ## If N is not a number, it should be 'N':
  else if(model$superpopn != "N")
    return(list(checkOK=F,
                msg=c(
                  "Error in model: the 'N<sub>s</sub>' entry must be either 'N', or a number greater than 0.",
                  "Your model has NOT been saved. Correct the error, and try again.")))
  ## end of N checks. N should be fine now.
  
  ## -----------------------------------------------------------------------------------------------------------
  ## Check the capture probabilities (common to all model types):
  ## -----------------------------------------------------------------------------------------------------------
  pcap <- model$paramdf$capturepr
  timeopt <- model$paramdf$timeopt
  ## Find which capture probabilities are numbers:
  ## p.as.numbers converts all the p's to numbers where possible:
  p.as.numbers <- suppressWarnings(as.numeric(pcap))
  which.p.numbers <- which(!is.na(p.as.numbers))
  ## For the capture probabilities that are numbers, make sure they are between 0 and 1:
  if(length(which.p.numbers)>0){
    p.numbers <- p.as.numbers[which.p.numbers]
    if(any(p.numbers < 0 | p.numbers > 1))
      return(list(checkOK=F,
                  msg=c("Error in model: all numeric capture probabilities must be between 0 and 1.",
                        "Your model has NOT been saved. Correct the error, and try again.")))
  }
  
  ## All remaining capture probabilities for selected times have to be "pt" where t is in 1:length(pcap).
  ## If the survey belonging to time t is blank, then that pt isn't allowed:
  if(any(is.na(p.as.numbers))){
    ## If there are any non-numeric capture probabilities:
    
    ntimes <- length(pcap)
    p.not.numbers <- is.na(p.as.numbers)
    selected.pvals <- pcap[timeopt & p.not.numbers]
    
    allowed.t <- which(timeopt & p.not.numbers)
    allowed.pvals <- c(paste("p", 1:ntimes, sep=""))
    allowed.pvals <- allowed.pvals[allowed.t]
    
    if(any(is.na(match(selected.pvals, allowed.pvals))))
      return(list(checkOK=F,
                  msg=c(paste("Error in model: every capture probability must be a number, or of the form pt, where t is a selected survey time with non-numeric p. <br>Here, the allowed times t mean you can use ",
                              paste("p", allowed.t, sep="",collapse=", "), ".<p>", sep=""),
                        "Your model has NOT been saved. Correct the error, and try again.")))
    
    ## Next want to check that all pt's that are used match t back to its native time period.
    ## For example, if the capture probability for time slot 5 uses p2, then p2 must appear in time slot 2.
    ## Although this check could be combined with the previous check, we can give more specific
    ## error information if we do it separately:
    
    ## Find the allowed native p-values, which are those who match their own native time period:
    native.pvals.reference <- paste("p", 1:ntimes, sep="")
    native.pvals <- native.pvals.reference[pcap==native.pvals.reference]
    
    if(any(is.na(match(selected.pvals, native.pvals)))){
      times.involved <- (1:ntimes)[allowed.t]
      times.using.nonnative.p <- times.involved[is.na(match(selected.pvals, native.pvals))]
      nonnative.p.chosen <- selected.pvals[is.na(match(selected.pvals, native.pvals))]
      time.wrongly.used <- strsplit(nonnative.p.chosen[1], split="p")[[1]][2]
      
      return(list(checkOK=F,
                  msg=c(paste("Error in model: capture probability ", nonnative.p.chosen[1],
                              " is used for time period ", times.using.nonnative.p[1],
                              " but is not defined as the capture probability in period ", time.wrongly.used,
                              ". Only p's defined for their own time period can be reused at other times.  <br>You could replace all instances of ", nonnative.p.chosen[1], " with p", times.using.nonnative.p[1], ".<br>", sep=""),
                        "Your model has NOT been saved. Correct the error, and try again.")))
      
      
    }
    
  }  ## End of case where non-numeric p's exist.
  
  ## -----------------------------------------------------------------------------------------------------------
  ## Multiple-phi check (full model only):
  ## -----------------------------------------------------------------------------------------------------------
  if(model$modeltype=="fullmodel"){
    ## For full models, allow either "phi" or "phi1, phi2".  However, do not allow mixing of the styles:
    ## if any of the entries are "phi", then all of the non-numeric entries must be phi.
    
    phivec <- model$paramdf$survrate

    ## Find which survival probabilities are numbers:
    ## phi.as.numbers converts all the phi's to numbers where possible:
    phi.as.numbers <- suppressWarnings(as.numeric(phivec))
    which.phi.numbers <- which(!is.na(phi.as.numbers))
    ## For the survival probabilities that are numbers, make sure they are between 0 and 1:
    if(length(which.phi.numbers)>0){
      phi.numbers <- phi.as.numbers[which.phi.numbers]
      if(any(phi.numbers < 0 | phi.numbers > 1))
        return(list(checkOK=F,
                    msg=c("Error in model: all numeric survival rates (phi) must be between 0 and 1.",
                          "Your model has NOT been saved. Correct the error, and try again.")))
    }
    
    ## All remaining survival probabilities for selected times have to be either "phi", OR a combination of
    ## "phi_t", but not a mixture.
    ## If they are phi_t, the t must be in 1:length(phivec). If the survey belonging to time t is blank,
    ## or if it is the last ticked survey, then that phi_t isn't allowed.
    
    if(any(is.na(phi.as.numbers))){
      ## If there are any non-numeric survival probabilities:
      
      ntimes <- length(phivec)
      phi.not.numbers <- is.na(phi.as.numbers)
      
      ## To be in the selected phivals, we need to have:
      ## phi not a number;
      ## NOT any times before the minimum time for which timeopt=T;
      ## NOT any times beyond the maximum time for which timeopt=T, including NOT that
      ## maximum time itself (this value is blank because phi goes from one time period to the next);
      ## otherwise, timeopt can be True or False because survival continues across gaps.
      
      include.phi.times <- rep(T, length(phivec))
      ## Remove all times strictly before the first ticked survey:
      if(min(which(timeopt))>1) include.phi.times[1:(min(which(timeopt))-1)] <- F
      ## Remove all times from the last ticked survey to the end (inclusive):
      include.phi.times[ rev(which(timeopt))[1] : length(include.phi.times) ] <- F
      selected.phivals <- phivec[include.phi.times & phi.not.numbers]

      ## If any of the values in selected.phivals are blank, it probably means they don't correspond to
      ## surveys.
      if(any(selected.phivals==""))
        return(list(checkOK=F,
                    msg=c("Error in model: some phi values are blank. If you don't want to specify phi's in survey gaps, redefine your time periods so you don't have any gaps.",
                          "Your model has NOT been saved. Correct the error, and try again.")))
      
      
      
      ## If all these values in selected.phivals are "phi", this is fine.  If not, we need to continue:
      if(!all(selected.phivals=="phi")){
        ## If ANY of the selected.phivals are "phi", fail the check because mixing phi with phi_t
        ## is not allowed:
        if(any(selected.phivals=="phi"))
          return(list(checkOK=F,
                      msg=c("Error in model: you can't mix 'phi' with other symbols. Non-numeric phi entries must either all be 'phi', or they must all be of the form phi<sub>t</sub>, but they can't be a mixture.",
                            "Your model has NOT been saved. Correct the error, and try again.")))
        
        ## If we've got to here, then "phi" does not appear in the list. All values must now be phi_t
        ## where the t is a *survey* time with native phi_t defined.
        ## Note that even though the selected phi include non-survey times, because survival must
        ## continue regardless, we do not allow non-survey times n to provide phi_n's.
        
        ## The "allowed" values of t that phi_t can take are therefore
        ## include.phi.times & phi.not.numbers & timeopt
        allowed.t <- which(include.phi.times & phi.not.numbers & timeopt)
        allowed.phivals <- c(paste("phi", 1:ntimes, sep=""))
        allowed.phivals <- allowed.phivals[allowed.t]
        
        if(any(is.na(match(selected.phivals, allowed.phivals))))
          return(list(checkOK=F,
                      msg=c(paste("Error in model: every phi must be a number, or of the form phi<sub>t</sub>, where t is a selected survey time with non-numeric phi. <br>Here, the allowed times t mean you can use ",
                                  paste("phi", allowed.t, sep="",collapse=", "), ".<p>", sep=""),
                            "Your model has NOT been saved. Correct the error, and try again.")))
        
        ## Next want to check that all phi-t's that are used match t back to its native time period.
        ## For example, if the phi for time slot 5 uses phi2, then phi2 must appear in time slot 2.
        ## Although this check could be combined with the previous check, we can give more specific
        ## error information if we do it separately:
        
        ## Find the allowed native phi-values, which are those who match their own native time
        ## period:
        native.phivals.reference <- paste("phi", 1:ntimes, sep="")
        native.phivals <- native.phivals.reference[phivec==native.phivals.reference]
        
        if(any(is.na(match(selected.phivals, native.phivals)))){
          times.involved <- which(include.phi.times & phi.not.numbers)
          times.using.nonnative.phi <- times.involved[is.na(match(selected.phivals, native.phivals))]
          nonnative.phi.chosen <- selected.phivals[is.na(match(selected.phivals, native.phivals))]
          time.wrongly.used <- strsplit(nonnative.phi.chosen[1], split="phi")[[1]][2]
          
          return(list(checkOK=F,
                      msg=c(paste("Error in model: ", nonnative.phi.chosen[1],
                                  " is used for time period ", times.using.nonnative.phi[1],
                                  " but is not defined as the survival probability in period ", time.wrongly.used,
                                  ".<br>Either enter ", nonnative.phi.chosen[1], " in time period ", time.wrongly.used, ", or replace all instances of ", nonnative.phi.chosen[1], " with phi<sub>t</sub> corresponding to a survey time t.<br>", sep=""),
                            "Your model has NOT been saved. Correct the error, and try again.")))
        }
        
        
      } ## End of case where some selected phi values are not numbers and not "phi".
    }  ## End of case where non-numeric phi's exist.
    
  }  ## End of multiple-phi check for full model type.
  
  ## -----------------------------------------------------------------------------------------------------------
  ## Single phi check: applies to Lambda models and Single Phi models:
  ## -----------------------------------------------------------------------------------------------------------
  
  if(model$modeltype=="lambdamodel" | model$modeltype=="singlephimodel"){
    ## For lambda or single-phi models, phi should be a number or phi.
    ## It is extracted from the lowest element of paramdf$survrate: note that there are at least
    ## two ticked surveys, or else a previous error has already been thrown, so this element
    ## of paramdf$survrate must exist.
    phival <- model$paramdf$survrate[min(which(model$paramdf$timeopt))]
    is.phi.number <- (!is.na(suppressWarnings(as.numeric(phival))))
    
    ## If phi is a number, find what it is:
    if(is.phi.number) phi.number <- suppressWarnings(as.numeric(phival))
    else phi.number <- 0  ## dummy value of phi for lambda to check against in lambda models below.
    
    if(is.phi.number){
      ## If phi is a number, it should be > 0 and < 1:
      if(phi.number < 0 | phi.number > 1)
        return(list(checkOK=F,
                    msg=c(
                      "Error in model: the 'Phi' entry must be either 'phi', or a number between 0 and 1.",
                      "Your model has NOT been saved. Correct the error, and try again.")))
    }
    
    ## If phi is not a number, it should be 'phi':
    if(!is.phi.number & phival!="phi")
      return(list(checkOK=F,
                  msg = c("Error in model: the 'Phi' entry must be either 'phi', or a number between 0 and 1.",
                          "Your model has NOT been saved. Correct the error, and try again.")))
    ## end of "phi is not a number".  phi should be fine now.
    
  }  ## End of Lambda and Single Phi models for checking phi.
  
  ## -------------------------------------------------------------
  ## Lambda check: applies to lambdamodels only:
  ## -------------------------------------------------------------
  if(model$modeltype=="lambdamodel"){
    ## lambda should be a number, lambda, or phi.
    is.lambda.number <- (!is.na(suppressWarnings(as.numeric(model$lambdaparam))))
    
    ## If lambda is a number, find what it is:
    if(is.lambda.number)
      lambda.number <- suppressWarnings(as.numeric(model$lambdaparam))
    
    if(is.lambda.number){
      ## If lambda is a number, it should be > 0 and > phi:
      ## (if phi isn't a number, phi.number is set to 0 above, so no damage is done below):
      if(lambda.number < 0)
        return(list(checkOK=F,
                    msg = c("Error in model: if the 'Lambda' entry is a number it must be greater than 0.",
                            "Your model has NOT been saved. Correct the error, and try again.")))
      
      else if(lambda.number < phi.number)
        return(list(checkOK=F,
                    msg = c("Error in model: if 'Lambda' and 'Phi' are both numbers, lambda must be greater than or equal to phi.",
                            "Your model has NOT been saved. Correct the error, and try again.")))
    }
    
    if(!is.lambda.number){
      ## lambda is not a number, so it should be either lambda or phi (don't allow capital letters):
      if(model$lambdaparam!="lambda" & model$lambdaparam!="phi")
        return(list(checkOK=F,
                    msg = c("Error in model: the 'Lambda' entry must be either 'lambda' or 'phi', or a number greater than 0.",
                            "Your model has NOT been saved. Correct the error, and try again.")))
      
    }  ## end of "lambda is not a number".  lambda should be fine now.
    
  }  ## End of Lambda models for lambda check.
  
  
  ## -----------------------------------------------------------------------------------------------------------
  ## Check the pent values (single-phi models and full models):
  ## -----------------------------------------------------------------------------------------------------------
  
  if(model$modeltype=="singlephimodel" | model$modeltype=="fullmodel"){
    
    pent <- model$paramdf$prentry
    timeopt <- model$paramdf$timeopt
    ## Find which pents are numbers:
    ## pent.as.numbers converts all the pent's to numbers where possible:
    pent.as.numbers <- suppressWarnings(as.numeric(pent))
    which.pent.numbers <- which(!is.na(pent.as.numbers))
    ## For the pents that are numbers, make sure they are between 0 and 1:
    ## (Note that in the model UI they do need to be specified between 0 and 1, because
    ## the first pent is always derived from the others, so we can't determine the scaling for relative pents.)
    if(length(which.pent.numbers)>0){
      pent.numbers <- pent.as.numbers[which.pent.numbers]
      if(any(pent.numbers < 0 | pent.numbers > 1))
        return(list(checkOK=F,
                    msg=c("Error in model: all numeric p<sub>ent</sub> must be between 0 and 1.",
                          "Your model has NOT been saved. Correct the error, and try again.")))
      
      ## The sum of any numerical pent's must be less than 1:
      if(sum(pent.numbers) > 1)
        return(list(checkOK=F,
                    msg=c(
                      "Error in model: the numerical p<sub>ent</sub> values sum to more than 1.",
                      "Your model has NOT been saved. Correct the error, and try again.")))
      
    }
    
    
    ## All remaining pents for selected times have to be "pent_t" where t is in 1:length(pent),
    ## except for the first one which is "calculated".  Note that if pent1 is "calculated", nonetheless pent1
    ## is still a legitimate choice for other times.
    ## If the survey belonging to time t is blank, then that pent_t isn't allowed:
    if(any(is.na(pent.as.numbers))){
      ## If there are any non-numeric pents:
      ntimes <- length(pent)
      pent.not.numbers <- is.na(pent.as.numbers)
      ## To be in the selected pentvals, we need to have:
      ## timeopt = True
      ## NOT the minimum time for which timeopt=T (this value is "calculated")
      ## pent not a number.
      timeopt.remove.first <- timeopt
      timeopt.remove.first[which(timeopt)[1]] <- F
      selected.pentvals <- pent[timeopt.remove.first & pent.not.numbers]
      
      ## Stay with unadjusted timeopt below because this lowest pent(t) *is* allowed to be
      ## reused in other slots:
      allowed.t <- which(timeopt & pent.not.numbers)
      allowed.pentvals <- c(paste("pent", 1:ntimes, sep=""))
      allowed.pentvals <- allowed.pentvals[allowed.t]
      
      if(any(is.na(match(selected.pentvals, allowed.pentvals))))
        return(list(checkOK=F,
                    msg=c(paste("Error in model: every p<sub>ent</sub> must be a number, or of the form pent<sub>t</sub>, where t is a selected survey time with non-numeric pent. <br>Here, the allowed times t mean you can use ",
                                paste("pent", allowed.t, sep="",collapse=", "), ".<p>", sep=""),
                          "Your model has NOT been saved. Correct the error, and try again.")))
      
      ## Next want to check that all pent-t's that are used match t back to its native time period.
      ## For example, if the pent for time slot 5 uses pent2, then pent2 must appear in time slot 2.
      ## Although this check could be combined with the previous check, we can give more specific
      ## error information if we do it separately:
      
      ## Find the allowed native pent-values, which are those who match their own native time period:
      native.pentvals.reference <- paste("pent", 1:ntimes, sep="")
      native.pentvals <- native.pentvals.reference[pent==native.pentvals.reference]
      ## Add the lowest pent, which appears as "calculated" but is nonetheless a valid choice for pent,
      ## to native.pentvals.  Usually this will be pent1, but not always.
      native.pentvals <- c(paste("pent", which(timeopt)[1], sep=""), native.pentvals)
      
      if(any(is.na(match(selected.pentvals, native.pentvals)))){
        times.involved <- which(timeopt.remove.first & pent.not.numbers)
        times.using.nonnative.pent <- times.involved[is.na(match(selected.pentvals, native.pentvals))]
        nonnative.pent.chosen <- selected.pentvals[is.na(match(selected.pentvals, native.pentvals))]
        time.wrongly.used <- strsplit(nonnative.pent.chosen[1], split="pent")[[1]][2]
        
        return(list(checkOK=F,
                    msg=c(paste("Error in model: ", nonnative.pent.chosen[1],
                                " is used for time period ", times.using.nonnative.pent[1],
                                " but is not defined as the entry probability in period ", time.wrongly.used,
                                ". Only pent's defined for their own time period can be reused at other times.  <br>You could replace all instances of ", nonnative.pent.chosen[1], " with pent", times.using.nonnative.pent[1], ".<br>", sep=""),
                          "Your model has NOT been saved. Correct the error, and try again.")))
      }
      
    }  ## End of case where non-numeric pent's exist.
    
  }  ## End of pent check for single-phi models and pent models.
  
  
  ## ---------------------------------------------------------------------------------------------------------------
  ## If we get through to the end of the function, then all checks have been satisfied.
  ## Return checkOK=T and no message.
  return(list(checkOK=T, msg=c("", "")))
  
}  ## End of checkModel.func


###############################################################
checkSim.func <- function(sim){
  ## checkSim.func checks the sim supplied for any errors, e.g. parameters out of
  ## range or inconsistent with each other.  Returns a list with components checkOK (true or false
  ## to say whether the sim check was successful) and msg which is the error message to be
  ## returned.
  ## If there is no error, this function returns a blank message, but a further message will be generated
  ## back in the main function create.sim.func() about whether the sim already exists or
  ## whether the save has been successful.
  ##
  ## This function is called from SimBuilder in SimUI/server.R.
  
  ## -----------------------------------------------------------------------------------------------------------
  ## Check there are at least two surveys (common to all sim types):
  ## -----------------------------------------------------------------------------------------------------------
  if(sum(sim$paramdf$timeopt)<2)
    return(list(checkOK=F,
                msg=c("Error: you need at least two selected surveys.",
                      "Your SimSet has NOT been saved. Correct the error, and try again.")))
  
  ## -----------------------------------------------------------------------------------------------------------
  ## Check N (common to all sim types):
  ## -----------------------------------------------------------------------------------------------------------
  is.N.number <- (!is.na(suppressWarnings(as.numeric(sim$superpopn))))
  
  ## If N is a number, find what it is:
  if(is.N.number){
    N.number <- suppressWarnings(as.numeric(sim$superpopn))
    if(N.number < 0)
      return(list(checkOK=F,
                  msg=c("Error: the 'N<sub>s</sub>' entry must be a number greater than 0.",
                        "Your SimSet has NOT been saved. Correct the error, and try again.")))
  }
  ## If N is not a number, throw an error:
  else
    return(list(checkOK=F,
                msg=c("Error: the 'N<sub>s</sub>' entry must be a number greater than 0.",
                      "Your SimSet has NOT been saved. Correct the error, and try again.")))
  
  ## end of N checks. N should be fine now.
  
  ## -----------------------------------------------------------------------------------------------------------
  ## Check the capture probabilities (common to all sim types):
  ## -----------------------------------------------------------------------------------------------------------
  ## Find which capture probabilities are numbers.  Only check pcap that correspond to ticked surveys.
  timeopt <- sim$paramdf$timeopt
  pcap <- sim$paramdf$capturepr[timeopt]
  
  ## p.as.numbers converts all the p's to numbers where possible:
  p.as.numbers <- suppressWarnings(as.numeric(pcap))
  which.p.numbers <- which(!is.na(p.as.numbers))
  ## For the capture probabilities that are numbers, make sure they are between 0 and 1:
  if(length(which.p.numbers)>0){
    p.numbers <- p.as.numbers[which.p.numbers]
    if(any(p.numbers < 0 | p.numbers > 1))
      return(list(checkOK=F,
                  msg=c("Error: all capture probabilities must be between 0 and 1.",
                        "Your SimSet has NOT been saved. Correct the error, and try again.")))
  }
  
  ## There should be no p's that are not numbers:
  if(any(is.na(p.as.numbers)))
    return(list(checkOK=F,
                msg=c("Error: all capture probabilities must be numbers between 0 and 1.",
                      "Your SimSet has NOT been saved. Correct the error, and try again.")))
  
  ## Capture probabilities should be fine now.
  
  ## -----------------------------------------------------------------------------------------------------------
  ## Multiple-phi check (full sim only):
  ## -----------------------------------------------------------------------------------------------------------
  if(sim$simtype=="fullsim"){
    ## All the phi's must be numbers between 0 and 1. There should be a numeric phi entered
    ## in every time period from the first survey to the time immediately before the last survey
    ## (whether or not that time is a survey time).
    phivec <- sim$paramdf$survrate
    include.phi.times <- rep(T, length(phivec))
    ## Remove all times strictly before the first ticked survey:
    if(min(which(timeopt))>1) include.phi.times[1:(min(which(timeopt))-1)] <- F
    ## Remove all times from the last ticked survey to the end (inclusive):
    include.phi.times[ rev(which(timeopt))[1] : length(include.phi.times) ] <- F
    
    ## Redefine phivec to be only those for which a phi is relevant:
    phivec.used <- phivec[include.phi.times]
    
    ## Find which survival probabilities are numbers:
    ## phi.as.numbers converts all the phi's to numbers where possible:
    phi.as.numbers <- suppressWarnings(as.numeric(phivec.used))
    which.phi.numbers <- which(!is.na(phi.as.numbers))
    
    ## If any phi are not numbers, throw an error:
    if(any(is.na(phi.as.numbers)))
      return(list(checkOK=F,
                  msg=c("Error: survival rates (phi) must be specified for every time from the first survey to the time immediately before the last survey. All phi values must be numbers between 0 and 1.",
                        "Your SimSet has NOT been saved. Correct the error, and try again.")))
    
    ## For the survival probabilities that are numbers, make sure they are between 0 and 1:
    if(length(which.phi.numbers)>0){
      phi.numbers <- phi.as.numbers[which.phi.numbers]
      if(any(phi.numbers < 0 | phi.numbers > 1))
        return(list(checkOK=F,
                    msg=c("Error: all survival rates (phi) must be between 0 and 1.",
                          "Your SimSet has NOT been saved. Correct the error, and try again.")))
    }
    
  } ## End of multiple-phi check for full sim type.
  
  ## -----------------------------------------------------------------------------------------------------------
  ## Single phi check: applies to Lambda sims and Single Phi sims:
  ## -----------------------------------------------------------------------------------------------------------
  if(sim$simtype=="lambdasim" | sim$simtype=="singlephisim"){
    ## For lambda or single-phi sims, phi should be a number or phi.
    ## It is extracted from the lowest element of paramdf$survrate: note that there are at least
    ## two ticked surveys, or else a previous error has already been thrown, so this element
    ## of paramdf$survrate must exist.
    phival <- sim$paramdf$survrate[min(which(sim$paramdf$timeopt))]
    is.phi.number <- (!is.na(suppressWarnings(as.numeric(phival))))
    
    ## If phi is a number, check it is between 0 and 1:
    if(is.phi.number){
      phi.number <- suppressWarnings(as.numeric(phival))
      if(phi.number < 0 | phi.number > 1)
        return(list(checkOK=F,
                    msg=c("Error: the 'Phi' entry must be a number between 0 and 1.",
                          "Your SimSet has NOT been saved. Correct the error, and try again.")))
    }
    ## If phi is not a number, throw an error:
    else
      return(list(checkOK=F,
                  msg = c("Error: the 'Phi' entry must be a number between 0 and 1.",
                          "Your SimSet has NOT been saved. Correct the error, and try again.")))
    
    
  }  ## End of Lambda and Single Phi sims for checking phi.
  
  
  ## -------------------------------------------------------------
  ## Lambda check: applies to lambdasims only:
  ## -------------------------------------------------------------
  if(sim$simtype=="lambdasim"){
    ## lambda should be a number greater than or equal to phi.
    is.lambda.number <- (!is.na(suppressWarnings(as.numeric(sim$lambdaparam))))
    
    ## If lambda is a number, find what it is:
    if(is.lambda.number){
      lambda.number <- suppressWarnings(as.numeric(sim$lambdaparam))
      
      ## If lambda is a number, it should be > phi:
      ## (phi has already been confirmed above to be a number between 0 and 1, called phi.number)
      if(lambda.number < phi.number)
        return(list(checkOK=F,
                    msg = c("Error: lambda must be greater than or equal to phi.",
                            "Your SimSet has NOT been saved. Correct the error, and try again.")))
    }
    else
      ## lambda is not a number: throw an error
      return(list(checkOK=F,
                  msg = c("Error: the 'Lambda' entry must be a number greater than phi.",
                          "Your SimSet has NOT been saved. Correct the error, and try again.")))
    
  }  ## End of Lambda sims for lambda check.  Lambda should be fine now.
  
  
  ## -----------------------------------------------------------------------------------------------------------
  ## Check the pent values (single-phi sims and full sims):
  ## -----------------------------------------------------------------------------------------------------------
  
  if(sim$simtype=="singlephisim" | sim$simtype=="fullsim"){
    ## Only check the pent's corresponding to survey times:
    pent <- sim$paramdf$prentry[which(sim$paramdf$timeopt)]
    
    ## Find which pents are numbers:
    ## pent.as.numbers converts all the pent's to numbers where possible:
    pent.as.numbers <- suppressWarnings(as.numeric(pent))
    which.pent.numbers <- which(!is.na(pent.as.numbers))
    ## For the pents that are numbers, make sure they are greater than 0.
    ## Note they do NOT need to be less than 1 for the SimUI, because they are scaled to sum to 1.
    if(length(which.pent.numbers)>0){
      pent.numbers <- pent.as.numbers[which.pent.numbers]
      if(any(pent.numbers < 0))
        return(list(checkOK=F,
                    msg=c("Error: all p<sub>ent</sub> must be numbers greater than or equal to 0.",
                          "Your SimSet has NOT been saved. Correct the error, and try again.")))
    }
    
    if(any(is.na(pent.as.numbers)))
      return(list(checkOK=F,
                  msg=c("Error: entry proportions (pent) must be specified for every survey time. All pent values must be numbers greater than or equal to 0.",
                        "Your SimSet has NOT been saved. Correct the error, and try again.")))
    
    
  }  ## End of pent check for single-phi sims and pent sims.
  
  
  ## ---------------------------------------------------------------------------------------------------------------
  ## If we get through to the end of the function, then all checks have been satisfied.
  ## Return checkOK=T and no message.
  return(list(checkOK=T, msg=c("", "")))
  
}  ## End of checkSim.func


###########################################################
popan.setup.func <- function(model, simset){
  ## popan.setup.func 8/9/14
  ## This function is used for setting up simulations or for fitting to real data. If fitting to real data,
  ## the simset is only used to set up starting values.
  ## The function returns a variety of objects:
  ## - SimTable : this is the matrix of survey times and parameters that will be used for simulations or
  ## start values;
  ## - SimN : this is the trueval / startval of superpopulation size, N;
  ## - ModelTable : this is the model matrix showing survey times and parameters to be estimated;
  ## - allparvec : the vector of all parameter slots, with a number if the parameter is to be fixed at this
  ## number and not estimated, or the name of the parameter that is going to be estimated for
  ## that slot otherwise;
  ## - pars.estvec : the names of parameters to be estimated;
  ## - constvalues : matches the format of allparvec, and has entry 0 if the corresponding parameter is to
  ## be estimated, and the number at which it is to be fixed if the corresponding parameter is to be fixed.
  ## - truevals : gleaned from simset, matches the format of allparvec, giving the true values used for every
  ## parameter slot, whether or not the parameter being estimated is the correct one for that slot.
  ## - startvals : matches the format of pars.estvec.  For each parameter to be estimated, it takes the start-value
  ## for likelihood maximization as the average of the truevals that use that parameter.
  ## To understand the difference between truevals and startvals: if the simset has p1=0.1, p2=0.4,
  ## but we estimate both slots with the same parameter p1, then truevals["p2"] will have the true value 0.4,
  ## whereas startvals will use the average (0.1 + 0.4)/2 for startvals["p1"] and startvals["p2"] doesn't exist.
  
  ## ---------------------------------------------------------------------------------------------------------------
  ## Checks and conversions of entries from the UI to model and simset format needed for R:
  ## ---------------------------------------------------------------------------------------------------------------
  ## Dump if model and simset cover different time arrangements.
  ## For clarity, demand that the set of defined time periods AND the set of survey years should all be the same
  ## for model and simset.  For example, it won't work if the simset has labels 2001, 2003, 2005
  ## but the model has labels 2001, 2002, 2003, 2004, 2005 with surveys at 2001, 2003, 2005,
  ## because we need to simulate what is happening during the survey gaps.
  
  ## Timelabels should be characters below, but use as.character just to be sure.
  stopMessage <- "Model and Sim cover different arrangements of time labels and/or survey years. Models and Sims can only be combined into Projects if they have the same survey configuration."
  if(nrow(model$paramdf)!=nrow(simset$paramdf)) stop(stopMessage)
  if(any(as.character(model$paramdf$timelabels) != as.character(simset$paramdf$timelabels)))
    stop(stopMessage)
  if(any(model$paramdf$timeopt != simset$paramdf$timeopt)) stop(stopMessage)
  
  ## -------------------------------------------------------------------------------------------------------------------
  ## Define survey.years: we've already checked that the model and simset versions are the same.
  ## -------------------------------------------------------------------------------------------------------------------
  survey.years <- as.numeric(as.character(model$paramdf$timelabels[model$paramdf$timeopt]))
  if(length(survey.years)<2) stop("Your simset and model have fewer than two surveys. At least two surveys are needed to proceed with simulations.")
  
  ## -------------------------------------------------------------------------------------------------------------------
  ## Check the model format: this is the same code as ModelBuilder uses, but is applied again here
  ## in case the model has been changed or missed the interface check:
  checkMod <- checkModel.func(model)
  if(!checkMod$checkOK){
    ## If the model failed the model check, replace the HTML codes in the error message, print it,
    ## and dump:
    errorMsg <- gsub("<br>", "\n", checkMod$msg[1])
    errorMsg <- gsub("<p>", "\n", errorMsg)
    errorMsg <- gsub("</p>", "\n", errorMsg)
    errorMsg <- gsub("<sub>", "_", errorMsg)
    errorMsg <- gsub("</sub>", "", errorMsg)
    stop(paste("\n------------------------\nModel check failed!\n------------------------\n", errorMsg,
               "\nYou can edit the model using ModelBuilder(\"",
               project["model"],
               "\"). \nCorrect it, save, recreate the project using ProjectBuilder(), and try again.",
               sep=""))
  }
  
  ## -------------------------------------------------------------------------------------------------------------------
  ## Check the Sim format: this is the same code as SimBuilder uses, but is applied again here
  ## in case the sim has been changed or missed the interface check:
  checkSim <- checkSim.func(simset)
  if(!checkSim$checkOK){
    ## If the Sim failed the sim check, replace the HTML codes in the error message, print it, and dump:
    errorMsg <- gsub("<br>", "\n", checkSim$msg[1])
    errorMsg <- gsub("<p>", "\n", errorMsg)
    errorMsg <- gsub("</p>", "\n", errorMsg)
    errorMsg <- gsub("<sub>", "_", errorMsg)
    errorMsg <- gsub("</sub>", "", errorMsg)
    stop(paste("\n------------------------\nSim check failed!\n------------------------\n", errorMsg,
               "\nYou can edit the Sim using SimBuilder(\"",
               project["simset"],
               "\"). \nCorrect it, save, recreate the project using ProjectBuilder(), and try again.",
               sep=""))
  }
  
  ## -----------------------------------------------------------------------------------
  ## Create SimTable for Sim:
  ## -----------------------------------------------------------------------------------
  ## SimTable is largely a repeat of Sim$paramdf, but strips out time periods before the first survey and
  ## after the last survey (not included in Ns), and recomputes pent using either lambda or prentry so that
  ## the pent column is full-precision instead of the rounded values used for display.
  ## Also converts all columns into numbers.
  ## SimTable includes a column "timeSlot" that refers back to the time slot before any surveys before the
  ## first or after the last are stripped out. For example, if the first survey is at time 2, this will be recorded via
  ## timeSlot[1]=2.  The correct capture probability for this survey will be p2, rather than p1.
  
  ## First convert N into a number: using as.character is probably unnecessary, but is included in case
  ## something has somehow become stored as a factor.
  SimN <- as.numeric(as.character(simset$superpopn))
  
  ## Create time (character), survey (logical), pvec (numeric), and phivec (numeric) in SimTable.
  ## Note that entries of pvec or phivec that were characters (including blank) become NA through this
  ## conversion:
  SimTable <- data.frame(time=as.character(simset$paramdf$timelabels),
                         timeSlot=1:nrow(simset$paramdf),
                         survey=as.logical(simset$paramdf$timeopt),
                         pvec=suppressWarnings(as.numeric(as.character(simset$paramdf$capturepr))),
                         phivec=suppressWarnings(as.numeric(as.character(simset$paramdf$survrate))),
                         stringsAsFactors=F)
  #Ntbefore = suppressWarnings(as.numeric(as.character(simset$paramdf$Nt)))
  
  ## Keep only the time slots from the first to the last survey inclusive:
  survrange <- range(which(SimTable$survey))
  SimTable <- SimTable[survrange[1]:survrange[2],]
  
  ## survvec contains the times at which surveys took place:
  survvec <- which(SimTable$survey)
  nsurv <- length(survvec)
  gapvec <- diff(survvec)
  cumvec <- c(0, cumsum(gapvec))
  
  ## -----------------------------------------------------------------------------------
  ## Calculate the pent column for SimTable.
  ## If it is a lambdasim, calculate it from lambda and phi;
  ## otherwise, calculate from prentry:
  if(simset$simtype=="lambdasim"){
    ## For lambda-sims, calculate pent from lambda and phi.
    ## This replicates the code used in SimUI/server.R but is needed to get it back to full precision
    ## instead of the display values in simset$paramdf.
    ## Because the first row of SimTable corresponds to the first survey, SimTable$phivec[1] should
    ## always be a number and for a lambda-sim is equal to all other values of phivec. Check it here
    ## in case of glitches:
    phicheck <- SimTable$phivec[!is.na(SimTable$phivec)]
    if(any(diff(phicheck)!=0)) stop("Sim specifies a lambda-model but has more than one value of phi in paramdf.\nThis suggests the Sim has been manipulated outside of the GUI.\nCorrect the Sim in SimBuilder and try again.")
    ## If we've passed the check, the first value of phi is what we need:
    phival <- SimTable$phivec[1]
    ## Extract lambda:
    lambdaval <- as.numeric(as.character(simset$lambdaparam))
    
    ## Set the pent for the first survey equal to 1, temporarily:
    pentvec <- rep(0, nrow(SimTable))
    pentvec[1] <- 1
    
    ## If there are any more surveys, find their pent's relative to the first survey:
    if(nsurv>1){
      for(t in 2:nsurv){
        pentvec[survvec[t]] <- (lambdaval - phival) *
          sum(  phival^(0 : (gapvec[t-1]-1)) * lambdaval^((cumvec[t]-1):cumvec[t-1]))
      }
      ## Rescale all the pents to add to 1:
      pentvec <- pentvec/sum(pentvec)
    }  ## End of more than one ticked survey
    SimTable$pent <- pentvec
  }
  ## ------------------------------------------------------------------
  else{
    ## Not a lambda-sim.  Calculate pent from the prentry column:
    relpent <- suppressWarnings(as.numeric(
      as.character(simset$paramdf$prentry[survrange[1]:survrange[2]])))
    ## pent's at non-survey times should be NA.  To be sure, set all pent's at non-survey times to 0.  If there
    ## were any other NAs in pent, they shouldn't be there and should cause an error in a moment.
    relpent[!SimTable$survey] <- 0
    SimTable$pent <- relpent / sum(relpent)
  }
  
  ## -----------------------------------------------------------------------------------
  ## Now calculate E(Nt) to full precision using the final pentvec.
  ## We have:
  ## N1 = pent[1] * Ns
  ## Nt = phi * N[t-1] + pent[t] * Ns
  ## If there are survey gaps, we only report Nt for the same times as we report pent.
  ## Survival can carry on throughout the gaps, but only applies to the animals alive at the
  ## start of the gap.  The pent parameters in the lambda model accommodate new
  ## births from throughout the gap (and omit any births that died during the gap).
  ## Thus if we have, say, N[t], gap, gap, N[t+3]
  ## then we want N[t+3] = phi^3 N[t] + pent[t+3]*Ns.
  
  ## Set the Nt for the first survey:
  Ntvec <- rep(0, nrow(SimTable))
  Ntvec[1] <- SimTable$pent[1] * SimN
  
  ## If there are any more surveys, find their Nt's:
  if(nsurv>1){
    for(t in 2:nrow(SimTable)){
      ## Apply survival to the animals present at time t-1:
      Ntvec[t] <- Ntvec[t-1] * SimTable$phivec[t-1]
      ## If there is a survey at time t, add the new entrants:
      if(SimTable$survey[t]) Ntvec[t] <- Ntvec[t] + SimN * SimTable$pent[t]
    }
  }
  SimTable$Nt <- Ntvec
  ## Best only to report Nt for times at which surveys occur, because it is misleading at
  ## other times because it includes mortality but not recruitment.
  SimTable$Nt[!(SimTable$survey)] <- NA
  
  ## -----------------------------------------------------------------------------------
  ## Create ModelTable for Model:
  ## -----------------------------------------------------------------------------------
  ## ModelTable is largely a repeat of Model$paramdf, but strips out time periods before the first survey and
  ## after the last survey, and creates the column "pent" instead of "prentry" that is NA if prentry="calculated".
  ## ModelTable includes a column "timeSlot" that refers back to the time slot before any surveys before the
  ## first or after the last are stripped out. For example, if the first survey is at time 2, this will be recorded via
  ## timeSlot[1]=2.  The correct capture probability for this survey will be p2, rather than p1.
  ## ModelTable also replaces entries "phi" with (for example) "phi1" everywhere, or whatever the minimum
  ## time slot is: for example, if the first survey occurs at time 2, then it becomes phi2 everywhere.
  
  ## Create time (character), survey (logical), pvec (character), phivec (character), and pent (character).
  ModelTable <- data.frame(time=as.character(model$paramdf$timelabels),
                           timeSlot=1:nrow(model$paramdf),
                           survey=as.logical(model$paramdf$timeopt),
                           pvec=as.character(model$paramdf$capturepr),
                           phivec=as.character(model$paramdf$survrate),
                           pent=as.character(model$paramdf$prentry), stringsAsFactors=F)
  ## Keep only the time slots from the first to the last survey inclusive:
  survrange <- range(which(ModelTable$survey))
  ModelTable <- ModelTable[survrange[1]:survrange[2],]
  
  ## Replace any blank entries with NA. Leave entries marked "calculated" as they are, for now.
  ModelTable$pvec[ModelTable$pvec==""] <- NA
  ModelTable$phivec[ModelTable$phivec==""] <- NA
  ModelTable$pent[ModelTable$pent==""] <- NA
  ModelTable$pent[ModelTable$pent=="calculated"] <- "calc"
  ## For phi, there should not be a mix of symbols "phi1" AND "phi". This has already been checked in
  ## checkModel.func. Replace anything that is "phi" with phi_t, where t is the minimum timeslot
  ## involving a phi:
  which.phi <- which(ModelTable$phivec=="phi")
  if(length(which.phi)>0)
    ModelTable$phivec[which.phi] <- paste("phi", min(ModelTable$timeSlot[which.phi]), sep="")
  
  ## Change model$lambdaparam to NA if it is blank:
  if(model$lambdaparam=="") model$lambdaparam <- NA
  
  ## ------------------------------------------------------------------------------------------------------------------
  ## Determine model structure, parameters to fix as constants, and parameters to be estimated:
  ## ------------------------------------------------------------------------------------------------------------------
  ## Compile all the parameter types into a single vector, allparvec:
  ## allparvec is a vector listing all parameter slots by name, and the parameter to use in character strings.
  
  ## Recall k is the number of surveys.  Let nyrs be the total number of years spanned by the surveys.
  ## For example, if the surveys occur in 2001-2003, and 2014-2016, then nyrs=16 but k=6.
  ## We need the following parameter slots in allparvec:
  ## N : one slot
  ## lambda : one slot.  Will be NA if the model is a non-lambda model.
  ## phi : nyrs-1 slots, although the number of different parameters that can go into these slots is at most k-1.
  ## p : k slots, named according to the time point of surveys (e.g. a survey at time 8 has parameter p8).
  ## pent : k slots, again named according to the survey time point.  Will be NA if the model is a
  ## lambda-model.
  
  ## Thus allparvec could the following format, e.g. if k=3 and nyrs=5, surveys occur in years 1, 3, and 5,
  ## and it's a lambda-model: (note there are only k-1=4 slots for phi's)
  ##   N    lambda      phi1  phi2   phi3  phi4   p1     p3   p5     pent1  pent3  pent5
  ## "N" "lambda"   "phi" "phi" "phi" "phi" "p1"  "p1" "p1"    NA      NA      NA
  
  ## Or if k=3 and nyrs=5, surveys occur in years 1, 3, and 5, and it's a non-lambda model:
  ##   N    lambda   phi1  phi2   phi3    phi4      p1     p3     p5     pent1    pent3    pent5
  ## "N"       NA    "phi1" 0.9  "phi3" "phi3"  "p1"  "p1" "p1"  "pent1" "pent3" "pent5"
  
  ## Note that the time labels on each parameter are consistent: e.g. parameter p3 matches time slot 3
  ## which is also the time slot that phi3 and pent3 refer to.
  
  nyrs <- nrow(SimTable)
  survyears <- ModelTable$timeSlot[ModelTable$survey]
  phinames <- paste("phi", ModelTable$timeSlot[-length(ModelTable$timeSlot)], sep="")
  pnames <- paste("p", survyears, sep="")
  pentnames <- paste("pent", survyears, sep="")
  allnames <- c("N", "lambda", phinames, pnames, pentnames)
  
  allparvec <- c(model$superpopn, model$lambdaparam, ModelTable$phivec[-length(ModelTable$phivec)],
                 ModelTable$pvec[ModelTable$survey], ModelTable$pent[ModelTable$survey])
  names(allparvec) <- allnames
  
  ## Check everything is as expected given the model type:
  ## -- if it's a lambda model, all pent parameters should be "calculated", and all phi parameters should be
  ## equal;
  ## -- if it's not a lambda model, lambda should be NA, and exactly one pent parameter should be "calculated".
  ## Note that in principle you could have all pent parameters being specified as numbers, and none of them
  ## would need to be "calculated" in that case. However, it's easier to leave it that you specify all but one of
  ## the numbers, so that there is still one entry saying "calculated". Then we don't have to make any special
  ## checks for this situation: the usual code will carry through unchanged, and we don't have to check that
  ## the numbers specified sum to 1.
  if(model$modeltype=="lambdamodel"){
    if(is.na(allparvec["lambda"])) stop("This is a lambda-model but there is nothing specified for lambda!")
    if(any(allparvec[pentnames]!="calc")) stop("All pent parameters should be CALC for a lambda model!")
    if(any(allparvec[phinames]!=allparvec[phinames][1]))
      stop("All phi parameters should be equal for a lambda model!")
  }
  else{
    ## Not a lambda model:
    if(!is.na(allparvec["lambda"])) stop("There should be no lambda parameter for a non-lambda model!")
    which.calc <- which(allparvec[pentnames]=="calc")
    if(length(which.calc)!=1)
      stop("There should be exactly one pent parameter specified as CALC for a non-lambda model!")
  }
  
  ## ---------------------------------------------------------------------------------------------------------------
  ## I don't think SimTable and ModelTable can have different configurations, because simset and model
  ## have the same configuration; but check here anyway.
  stopMessage <- "Something has gone wrong with SimTable and ModelTable: they have different configurations even though simset and model have the same configuration..."
  if(any(ModelTable$time != SimTable$time)) stop(stopMessage)
  if(any(ModelTable$timeSlot != SimTable$timeSlot)) stop(stopMessage)
  if(any(ModelTable$survey != SimTable$survey)) stop(stopMessage)
  
  ## ---------------------------------------------------------------------------------------------------------------
  ## Now want two further vectors:
  ## 1. constvalues lists all the parameter slots in allparvec, with 0 everywhere except for parameters that are
  ## to be held constant and not estimated:  e.g. constvalues could be:
  ##   N    lambda      phi1  phi2   phi3  phi4   p1     p3   p5     pent1  pent3  pent5
  ##   0         0            0.99  0.99   0.99  0.99    0        0     0         0         0         0
  ## Note that these entries will be added to the parameter values in the code, so we use 0 for non-const
  ## entries, even if the parameter slot is NA in allparvec.
  ## If for some reason a parameter is to be fixed at value 0 itself, it would still work this way.
  ##
  ## 2. pars.estvec lists only the free parameters to be estimated, for sending through the optimisation routines:
  ## e.g. pars.estvec = c("N", "lambda", "p1", "p3", "p5") in the example above because pent's are all "calc".
  
  ## To begin with, constvalues will have NA's for any non-numeric values.
  ## Using suppressWarnings below suppresses the warning of coercing non-numerics to numeric to give NAs:
  ## failed coercion is the desired behaviour so we don't want a warning.
  constvalues <- suppressWarnings(as.numeric(allparvec))
  names(constvalues) <- names(allparvec)
  ## pars.estvec contains the unique NON-numeric values in allparvec, that aren't NA or "calc":
  pars.estvec <- unique( allparvec[is.na(constvalues)] )
  pars.estvec <- pars.estvec[!is.na(pars.estvec)]
  pars.estvec <- pars.estvec[pars.estvec!="calc"]
  ## Redefine NAs in constvalues as 0's:
  constvalues[is.na(constvalues)] <- 0
  
  ## ------------------------------------------------------------------------------------------------------------------
  ## Find truevals and startvals:
  ## ------------------------------------------------------------------------------------------------------------------
  ## Truevals are detected from the simset.
  ## This mimics the construction of allparvec from ModelTable; in the same way, truevals is
  ## constructed from the same slots in SimTable.
  ## Note that Simlambda will be NA if it isn't a lambda model.
  if(simset$simtype!="lambdasim") SimLambda <- NA
  else SimLambda <- as.numeric(as.character(simset$lambdaparam))
  truevals <- c(SimN, SimLambda, SimTable$phivec[-length(SimTable$phivec)],
                SimTable$pvec[SimTable$survey], SimTable$pent[SimTable$survey])
  names(truevals) <- allnames
  
  ## For startvals, take the average of all the truevals that correspond to the specified parameter
  ## in allparvec.
  startvals <- numeric(length(pars.estvec))
  names(startvals) <- pars.estvec
  ## Define allparvec.1 and truevals.1 to be the same as allparvec and truevals, removing any
  ## elements that are NA in allparvec, because otherwise the NAs cause trouble:
  allparvec.1 <- allparvec[!is.na(allparvec)]
  truevals.1 <- truevals[!is.na(allparvec)]
  for(pr in pars.estvec) startvals[pr] <- mean(truevals.1[allparvec.1==pr])
  
  ## If the sim does not possess lambda, but the model needs to estimate lambda, the startval
  ## for lambda will be NA.  Replace it here with the derived value from simset$Nt(end) and
  ## simset$Nt(beginning):
  ## We have lambda = E(Nt+1) / E(Nt)
  ## so over max(timeslot) - min(timeslot) time intervals, we should have
  ## N(tmax) = approx lambda^(tdiff) N(tmin) where tdiff = max(timeslot) - min(timeslot).
  ## So lambda^tdiff = approx  N(tmax) / N(tmin)
  ## lambda = approx {N(tmax) / N(tmin)}^(1/tdiff)
  if(any(pars.estvec=="lambda")){
    if(is.na(startvals["lambda"]))
      startvals["lambda"] <- (SimTable$Nt[nrow(SimTable)]/SimTable$Nt[1])^(1/
                                                                             (max(SimTable$timeSlot)-min(SimTable$timeSlot)))
  }
  
  ## In the likelihood function, we will get the correct values into N, lambda, phi, and pvec using
  ## values <- pars[match(allparvec, pars.estvec)].
  
  return(list(SimTable=SimTable, SimN=SimN, SimLambda=SimLambda, ModelTable=ModelTable,
              lambdamodel=(model$modeltype=="lambdamodel"),
              survey.years=survey.years, gapvec=gapvec, cumvec=cumvec,
              allparvec=allparvec, pars.estvec=pars.estvec, constvalues=constvalues,
              phinames=phinames, pnames=pnames, pentnames=pentnames,
              startvals=startvals, truevals=truevals))
  
}
## End of popan.setup.func

#############################################################

popan.sim.wrap <- function(projname=NULL, Nsim=1000, modelname=NULL, simname=NULL, existing_data = NULL){
  ## popan.sim.wrap 21/6/13 resumed 2/5/14
  ## General-purpose wrapper for all model and sim types.
  ##
  ## First creates the model and simset objects, given a suitable combination of either projname, OR
  ## both modelname and simname.
  ##
  ## Then calls popan.setup.func to set up the estimation parameters, values to be fixed as constants,
  ## true values, and simulation start values, using this model and simset.
  ##
  ## Then repeatedly simulates data and fits it with popan.func.
  ##
  ## If this function runs successfully, it automatically assigns the output to result.projname in
  ## environment CPenv, and sets the "run" component of the project to 1.
  ##
  ## The default way to supply both model and starting values is to supply argument projname,
  ## e.g. projname="P3" will pick up project P3 in ProjectList, which specifies the name of a model
  ## and a simset.
  ##
  ## The optimisation starting values are detected from the generating values in simset.
  ## The survey.years must be the same for simset and model.
  ##
  ## If projname is not supplied it is concocted from modelname and simname
  ## and permanently added to ProjectList in CPenv.
  ##
  ## If project is supplied, the function will dump if either model or simset are also supplied.
  ## This aims to avoid any subtle precedence behaviour (e.g. project overrides model + simset)
  ## that might confuse which project is actually being run.
  ##
  ## If project is NOT supplied, then modelname and simname must both be supplied instead.  This allows a
  ## project to be compiled on the fly.  The code will first check that the (sim,model) combination does not
  ## already exist in a named project; if it does, the output will be assigned the name of that project.  If it
  ## doesn't, the output will be assigned the name "S*.M*"  where S* denotes the simname and M* the
  ## modelname.
  
  ## ------------------------------------------------------------------------------------------------------------------
  ## Extract model and simset and check they are compatible with each other:
  ## ------------------------------------------------------------------------------------------------------------------
  
  All.Projects <- get("ProjectList", envir=CPenv)
  if(!is.null(projname)){
    ## -------------------------------------------------------------------
    ## Project name is supplied (default from user interface)
    ## -------------------------------------------------------------------
    ## If project is supplied: dump function if model or simset arguments are also supplied.
    if(!is.null(modelname) | !is.null(simname))
      stop("Must not supply modelname or simname arguments if projname is supplied.")
    
    ## Extract project from ProjectList in CPenv:
    project <- All.Projects[[projname]]
    ## If the project doesn't exist, stop:
    if(is.null(project)) stop("Project doesn't exist!  Did you forget to save it?")
    
    ## Dump if project has already been run, or doesn't contain a model or a simset:
    if(project["run"]==1)
      # stop("Project already has results. Previous results must be removed before project can be run again.")
      print("Project already has results. Previous results must be removed before project can be run again.")
    if(project["model"]=="" | project["simset"]=="") stop("Project is missing a model or sim!")
    
    ## Extract model and simset for the project:
    model <- get("ModelList", envir=CPenv)[[ project["model"] ]]
    simset <- get("SimList", envir=CPenv)[[ project["simset"] ]]
  }
  else{
    ## ---------------------------------------------------------------------------------------------------------------
    ## Project name is not supplied: check if it exists, or if not create it here as "Simset.Model"
    ## ---------------------------------------------------------------------------------------------------------------
    if(is.null(modelname) | is.null(simname))
      stop("Either project, OR model and sim, must be supplied.")
    ## --------------------------------------------------------------------------------------------
    ## Check whether the (simset, model) combination already exists as a project:
    ## --------------------------------------------------------------------------------------------
    for(proj in names(All.Projects))
      if(All.Projects[[proj]]["simset"]==simname & All.Projects[[proj]]["model"]==modelname){
        ## Use the pre-existing project name:
        catline("This project already exists with name ", proj, ".")
        ## Check it hasn't already been run:
        projname <- proj
        project <- All.Projects[[projname]]
        if(project["run"]==1)
          stop("Project already has results. Previous results must be removed before project can be run again.")
        ## If not, continue:
        catline("Results will be saved to ", paste("result.", proj, sep=""), " and the project will be set to Run.")
        ## Extract model and simset for the project:
        model <- get("ModelList", envir=CPenv)[[ project["model"] ]]
        simset <- get("SimList", envir=CPenv)[[ project["simset"] ]]
      }
    ## -----------------------------------------
    ## Otherwise create the new project:
    ## -----------------------------------------
    ## If the project already existed under some name, the following block of code is skipped.
    ## If not, projname is still NULL and the project is created and given a new name:
    if(is.null(projname)){
      ## Create new project here:
      model <- get("ModelList", envir=CPenv)[[modelname]]
      simset <- get("SimList", envir=CPenv)[[simname]]
      
      ## Check whether model and simset are compatible, i.e. they cover the same arrangement
      ## of time labels AND survey years:
      stopMessage <- "Model and Sim cover different arrangements of time labels and/or survey years. Models and Sims can only be run together if they have the same survey configuration."
      if(nrow(model$paramdf)!=nrow(simset$paramdf)) stop(stopMessage)
      if(any(as.character(model$paramdf$timelabels) != as.character(simset$paramdf$timelabels)))
        stop(stopMessage)
      if(any(model$paramdf$timeopt != simset$paramdf$timeopt)) stop(stopMessage)
      
      ## If they are compatible, continue:
      ## Create a dummy projname to use in printed output and add to ProjectList:
      projname <- paste(simname, modelname, sep=".")
      catline("Creating new project, ", projname, ", and saving it to database")
      ## Create a new project with this name and description, and assign it to ProjectList:
      ## currently run=0 because there might be errors.
      project <- c(name=projname, description=projname, simset=simname, model=modelname,
                   run=0)
      All.Projects[[projname]] <- project
      assign("ProjectList", All.Projects, envir=CPenv)
    }
  }
  
  ## ------------------------------------------------------------------------------------------------------------------
  ## Set up the project:
  ## ------------------------------------------------------------------------------------------------------------------
  proj.setup <- popan.setup.func(model=model, simset=simset)
  
  ## ------------------------------------------------------------------------------------------------------------------
  ## Start the simulations
  ## ------------------------------------------------------------------------------------------------------------------
  resmat <- NULL
  
  s <- proc.time()
  
  for(sim in 1:Nsim){
    # Increment progress-bar
    incProgress(1 / Nsim)
    
    catline("\n================================\n")
    catline("Project ", projname, "   Iteration ", sim, "\n")
    
    ## Generate data:
    dat.sim <- sim.func(
      N = proj.setup$SimN,
      existing_data = existing_data,
      simtable = proj.setup$SimTable
    )
    
    # Check that capture history attribution succeeded if requested.
    if(is.null(attributes(dat.sim)$animals.not.placed)) {
      ## Fit model:
      res.sim <- try(popan.func(det.dat=dat.sim, setup.res=proj.setup, printit=F))
      
      ## If fitting was successful, append results to resmat:
      if(!inherits(res.sim, "try-error")){
        resmat <- rbind(resmat, res.sim)
      }
    }
  }
  
  print(proc.time() - s)
  
  rownames(resmat) <- NULL
  resmat <- data.frame(resmat, stringsAsFactors=F)
  attributes(resmat)$setup <- proj.setup
  attributes(resmat)$call <- match.call()
  
  ## If we've got to here, the result has been successfully created.  Assign result.projname to CPenv:
  resultname <- paste("result.", projname, sep="")
  assign(resultname, resmat, envir=CPenv)
  ## Set the "run" tag of project to 1:
  All.Projects[[projname]]["run"] <- 1
  assign("ProjectList", All.Projects, envir=CPenv)
  resmat
}

###############################################################

popan.func <- function(det.dat, setup.res, printit=T){
  ## popan.func 19/9/14
  ## Fit for any POPAN model, either lambda or standard, incorporating:
  ## up to k capture probabilities;
  ## up to k-1 survival parameters phi (must be at most one phi parameter if a POPAN-lambda model is
  ## being fit);
  ## EITHER up to k-1 entry proportions pent OR a single lambda value;
  ## single superpopulation size N.
  ## Any of these can be entered as constant values not to be estimated if required.
  ##
  ## Note that k is the number of surveys, not the total number of years spanned by the surveys.
  ## For example, if the surveys occur in 2001-2003, and 2014-2016, then k=6.
  ##
  ## This function is only ever called from a wrapper function: either popan.sim.wrap or popan.data.wrap.
  ## So there are no setup or compatibility checks done within this function, as popan.setup.func has already
  ## been run before we get to here.  The setup.res argument is returned from popan.setup.func.
  
  ## ------------------------------------------------------------------------------------------------------------------
  ## Extract setup results:
  ## ------------------------------------------------------------------------------------------------------------------
  k <- ncol(det.dat)
  if(k < 2) stop("negloglike.func assumes k >= 2.")
  
  nhist <- nrow(det.dat)
  allparvec <- setup.res$allparvec
  pars.estvec <- setup.res$pars.estvec
  startvals <- setup.res$startvals
  
  ## ------------------------------------------------------------------------------------------------------------------
  ## Create vectors to be used inside the likelihood for slotting parameter values into the right places:
  ## ------------------------------------------------------------------------------------------------------------------
  ## which.pars.into.allvalues.tmp has the same length as allparvec, but contains the *index* of the pars.estvec
  ## value that should be slotted into the corresponding position in allvalues, or NA if there isn't any such
  ## parameter.
  ## For example, if allparvec is:
  ## N    lambda   phi1     phi2    phi3     p1     p2     p4    pent1   pent2   pent4
  ## "N"  "1.1"    "phi1"   "phi1"  "phi1"   "p1"   "p2"   "p4"  "calc"  "calc"  "calc"
  ## then
  ## pars.estvec = c("N", "phi1", "p1", "p2", "p4")
  ## and
  ## which.pars.into.allvalues.tmp = c(1, NA, 2, 2, 2, 3, 4, 5, NA, NA, NA)
  ##
  ## Then inds.insert.into.allvalues gives the indices of which.pars.into.allvalues.tmp that are not NA:
  ## inds.insert.into.allvalues = c(1, 3, 4, 5, 6, 7, 8)
  ##
  ## Finally, which.pars.into.allvalues is the shortened version which only includes the non-NAs:
  ## which.pars.into.allvalues = c(1, 2, 2, 2, 3, 4, 5)
  which.pars.into.allvalues.tmp <- match(allparvec, pars.estvec)
  inds.insert.into.allvalues <- which(!is.na(which.pars.into.allvalues.tmp))
  which.pars.into.allvalues <- which.pars.into.allvalues.tmp[inds.insert.into.allvalues]
  
  ## ------------------------------------------------------------------------------------------------------------------
  ## Preliminary data calculations for the likelihood:
  ## ------------------------------------------------------------------------------------------------------------------
  ## For each element in det.dat, find the first sighting (first.obs) and the last sighting (last.obs),
  ## these take values in 1, 2, ..., k corresponding to the k survey times:
  
  ## 1. caps makes a vector of the number of captures on each occasion which can be multiplied with the
  ## log of the capture probability vector and subtracted from the negative log-likelihood.
  ## 2. non.caps.mat is a matrix of zeros of the same dimensions as the data where non-captures between the 
  ## first and last captures for each animal are recorded as ones.  Its column-sums (non.caps) can be multiplied 
  ## by the log of the complement of the capture probability vector and subtracted from the negative log-likelihood. 
  ## 3. first.tab and last.tab are the frequency tables of the first.obs and last.obs vectors.  
  ## These can be multiplied by the logs of the prob.to.f.m.1 (Robin added below) and chi vectors respectively 
  ## and subtracted from the negative log-likelihood.
  ## 4. survive.mat is a matrix of zeros of the same dimensions as the data where survival on occasions from the 
  ## first to last-minus-one-th captures for each animal are recorded as ones.  Its column-sums (survives) can be 
  ## multiplied by the log of the psurvive.gap vector and subtracted from the negative log-likelihood.
  caps <- colSums(det.dat)
  non.caps.mat <- matrix(0, nrow = nhist, ncol = k)
  survive.mat <- matrix(0, nrow = nhist, ncol = k)
  first.obs <- last.obs <- numeric(nhist)
  for(elt in 1:nhist){
    which.1 <- which(det.dat[elt,]==1)
    first <- min(which.1)
    last <- max(which.1)
    first.obs[elt] <- first
    last.obs[elt] <- last
    if(last > first){
      non.caps.mat[elt, first:last] <- 1 - det.dat[elt, first:last]
      survive.mat[elt, first:(last-1)] <- 1
    }
  }
  non.caps <- colSums(non.caps.mat)
  first.tab <- table(factor(first.obs, levels=1:k))
  last.tab <- table(factor(last.obs, levels=1:k))
  survives <- colSums(survive.mat)[1:(k-1)]
  
  # Indices of parameters in allparvec starting from zero for the TMB C++
  # function
  allparnames <- names(allparvec)
  lambdaind <- grep("lambda", allparnames) - 1
  if(length(lambdaind) == 0) lambdaind = 0
  pentinds <- match(setup.res$pentnames, allparnames) - 1
  calcind <- grep("calc", allparvec[pentinds + 1]) - 1
  
  # If multiple pents are calc we calculate them all so set calcind to
  # zero so can import as integer either way
  if(length(calcind) > 1) calcind = 0
  
  # Check if lambda being estimated so can reparameterize as rho = lambda
  # - phi >= 0 in TMB function
  lambda.par.ind <- match("lambda", pars.estvec)
  lambda.est <- !is.na(lambda.par.ind)
  
  # Create data and parameters lists and pass to TMB to create
  # automatically differentiated negative log likelihood function
  data <- list(
    k = k, lambdamodel = as.numeric(setup.res$lambdamodel), 
    gapvec = setup.res$gapvec, nhist = nhist, firsttab = first.tab, 
    lasttab = last.tab, caps = caps, noncaps = non.caps, 
    survives = survives, constvalues = setup.res$constvalues, 
    indsinsertintoallvalues = inds.insert.into.allvalues - 1,
    whichparsintoallvalues = which.pars.into.allvalues - 1, 
    phiinds = match(setup.res$phinames, allparnames) - 1, 
    pentinds = pentinds, pinds = match(setup.res$pnames, allparnames) - 1, 
    Nind = grep("N", allparnames) - 1, lambdaind = lambdaind, 
    calcind = calcind, lambdaest = as.numeric(lambda.est)
  )
  
  ## ------------------------------------------------------------------------------------------------------------------
  ## Find the MLEs:
  ## ------------------------------------------------------------------------------------------------------------------
  # Set bounds and parameter scale.  Most parameters are usually
  # probabilities, update others next.
  npar <- length(startvals)
  upper.bounds <- rep(1, npar)
  lower.bounds <- rep(0, npar)

  # If N is estimated set its bounds and parameter scale differently.
  N.par.ind <- match("N", pars.estvec)
  if (!is.na(N.par.ind)) {
    upper.bounds[N.par.ind] <- Inf
    lower.bounds[N.par.ind] <- nhist
  }
  
  # If lambda is estimated set its upper bound, starting value and
  # parameter scale to that for rho = lambda - phi, which is what will be
  # optimized in TMB.  This is much simpler than implementing the bound
  # that lambda > phi.
  phi.ind <- grep("phi", pars.estvec)
  if (!is.na(lambda.par.ind)) {
    upper.bounds[lambda.par.ind] <- Inf
    startvals[lambda.par.ind] <- startvals[lambda.par.ind] - 
      startvals[phi.ind]
  }
  
  # Make automatically differentiated negative log-likelihood function
  # with TMB
  obj <- MakeADFun(data, list(pars = startvals), DLL = "popan_nll", 
                   silent = T)
  
  # Find MLEs.  For lambda models reparameterizes with rho = lambda - phi
  # >= 0 in TMB function
  mle.res <- nlminb(
    start = obj$par, objective = obj$fn, gradient = obj$gr, hessian = obj$he,
    scale = 1 / obj$par, lower = lower.bounds, upper = upper.bounds,
    control = list(iter.max = 500, eval.max = 500)
  )
  
  # If estimating lambda add estimate of phi as TMB function actually
  # estimating rho
  if (lambda.est) {
    mle.res$par[lambda.par.ind] <- mle.res$par[lambda.par.ind] + 
      mle.res$par[phi.ind]
  }

  # Add names to parameter estimates
  mle.params <- mle.res$par
  names(mle.params) <- pars.estvec
  
  ## Calculate the AIC. See the MARK manual, page 111 (4-34).
  min.negloglike <- mle.res$objective
  npar <- length(mle.params)
  AIC <- 2 * npar + 2 * min.negloglike
  AICc <- AIC + 2 * npar * (npar + 1) / (nhist - npar - 1)
  
  ## ------------------------------------------------------------------------------------------------------------------
  ## Find std errors using TMB:
  ## ------------------------------------------------------------------------------------------------------------------
  sds <- summary(sdreport(obj))
  var.vec <- head(sds[, 2], npar)^2
  names(var.vec) <- paste("var", pars.estvec, sep=".")
  
  # If estimating lambda get variance from TMB separately because actually
  # estimates rho = lambda - phi.  Can't just add their variances as the
  # estimates are correlated.
  if (lambda.est) {
    var.vec[lambda.par.ind] <- sds[npar + 1, 2]^2
  }
  
  ## ------------------------------------------------------------------------------------------------------------------
  ## Compile results for returning:
  ## ------------------------------------------------------------------------------------------------------------------
  
  # Flag is never true but is still checked in various places
  all.res <- c(mle.params, var.vec, min.nll = min.negloglike, npar = npar,
               AIC = AIC, AICc = AICc, code = mle.res$convergence == 0, 
               flag = is.nan(mle.res$objective), 
               iter = mle.res$iterations, exp_n_alive = tail(sds, k))
  attributes(all.res)$allparvec <- allparvec
  all.res
}

###############################################################
unrun.func <- function(){
  ## unrun.func 17/5/13 : for testing, allows all projects to be unrun
  all.proj <- get("ProjectList", envir=CPenv)
  for(pr in names(all.proj)) {
    all.proj[[pr]]["run"] <- 0
    resultname <- paste("result.", pr, sep="")
    print(resultname)
    if(exists(resultname, envir=CPenv)) rm(list=resultname, envir=CPenv)
  }
  assign("ProjectList", all.proj, envir=CPenv)
  
}

###############################################################
proj.func <- function() get("ProjectList", envir=CPenv)
getenv <- function(what) get(what, envir=CPenv)
rmenv <- function(what) rm(list=what, envir=CPenv)
putenv <- function(name, what) assign(name, what, envir=CPenv)
lsenv <- function() names(as.list(CPenv))

getmodel <- function(name) getenv("ModelList")[[name]]
getsim <- function(name) getenv("SimList")[[name]]
getproj <- function(name) getenv("ProjectList")[[name]]
getresult <- function(name) getenv(paste0("result.", name))

simlist <- function() getenv("SimList")
modellist <- function() getenv("ModelList")
projlist <- function() getenv("ProjectList")

############################################################

sim.func <- function(N, simtable, existing_data = NULL){
  ## sim.func 22/6/13
  ## General simulation function for any POPAN model, including lambda-models or standard POPAN models.
  ## A simset is returned from SimBuilder.  popan.sim.wrap uses the simset to derive components
  ## N (numeric) and simtable.  Simtable is like simset$paramdf but the pent values are stored to full precision.
  ##
  ## lambda does not enter this function at all, because it has already been used to create the pent values
  ## in simout.
  ##
  ## In every simulation, the true N is used, so the total number of animals who were exposed to at
  ## least one survey is exactly N.  However the entry rates do have variability, so for a particular simulation
  ##  the number of animals entering in each of the survey years differs.
  
  nt <- nrow(simtable)
  nsurv <- sum(simtable$survey)
  
  ## ------------------------------------------------------------------------------------------------------------
  ## Time of entry
  ## ------------------------------------------------------------------------------------------------------------
  ## Determine the time of entry of each of the N animals: time.ent corresponds to the row of simtable
  ## in which the animals enter.
  
  # Use rmultinom to divide the N animals into groups corresponding to the 
  # entry proportions and then enter them directly using rows 1 to n1 for the n1 entering on 
  # occasion 1, and so on.  total.ent keeps track of the total number of animals that have entered
  # the population up to and including the current occasion.
  time.ent <- rmultinom(1, N, simtable$pent)
  total.ent <- cumsum(time.ent)
  
  ## ------------------------------------------------------------------------------------------------------------
  ## Survival and Detection
  ## ------------------------------------------------------------------------------------------------------------
  ## Create alive.dat, a matrix specifying the range of survey times for which the animal is alive (1) or
  ## either not-born or dead (0).
  ## alive.dat has a column for each time, whether or not a survey takes place at that time.
  alive.dat <- matrix(0, nrow=N, ncol=nt)
  
  # Animals entering on occasion one.
  alive.dat[1:total.ent[1], 1] <- 1
  
  ## psurv are the capture probabilities corresponding to surveys (not ""):
  psurv <- simtable$pvec[simtable$survey]
  
  ## Animals not alive automatically get 0 for detection.
  det.hist <- matrix(0, ncol=nsurv, nrow=N)
  
  ## Simulate detections of animals alive on the first occasion if there is a survey then and no existing data.
  ## Start keeping track of number of surveys done.
  surv.num <- 0
  if(simtable$survey[1]){
    surv.num <- 1
    if(is.null(existing_data)) {
      det.hist[1:total.ent[1], 1] <- rbinom(time.ent[1], 1, psurv[1])
    }
  }
  
  # Loop over occasions after occasion 1.
  for(t in 2:nt){
    # Enter the animals which survive from the previous occasion.
    # It's faster to call rbinom only for the number that are alive before and
    # index the results into those rows with as.logical(...) 
    # but maybe not to subset them on the left hand side.
    alive.dat[as.logical(alive.dat[, t-1]), t] <-
      rbinom(sum(alive.dat[1:total.ent[t-1], t-1]), 1, simtable$phivec[t-1])
    
    # If there is a survey on this occasion
    if(simtable$survey[t]){
      # Enter the animals entering on this occasion in their rows as determined above.
      if(total.ent[t-1] < total.ent[t])
        alive.dat[(total.ent[t-1] + 1):total.ent[t], t] <- 1
      
      # Increment number of surveys including this one
      surv.num <- surv.num + 1
      
      # If no existing data or survey is for after existing data
      if(is.null(existing_data) || (surv.num > ncol(existing_data))) {
        # Simulate detections of animals alive on this occasion
        det.hist[as.logical(alive.dat[, t]), surv.num] <- 
          rbinom(sum(alive.dat[1:total.ent[t], t]), 1, psurv[surv.num])
      }
    }
  }
  
  # If there's an existing dataset
  if(!is.null(existing_data)) {
    # Attribute the capture histories to animals alive at the
    # necessary times if possible.  This may fail in which case
    # there is a non-NULL attribute animals.not.placed. Might want
    # to try to prioritise more likely attributions later.
    det.hist <- attr_cap_hists(
      dat = existing_data, 
      survs = as.numeric(simtable$time[simtable$survey][1:ncol(existing_data)]), 
      alive.dat = alive.dat[, simtable$survey], 
      det.hist = det.hist
    )
  }
  
  ## Delete animals that are never seen, to give the observed data:
  det.dat <- det.hist[rowSums(det.hist) > 0, ]
  
  # Write data to disk for examples/assignments etc
  # write.csv(det.dat, "popan_data.csv", row.names = F)
  
  # Return capture histories
  return(det.dat)
}


#############################################################

catline <- function(...)
{
  cat(..., "\n")
}

##############################################################
Ns.calc.func <- function(nalive.in, year.in, lambda, phi, years.out){
  ## Ns.calc.func 10/5/13
  ## Given a single value of nalive, corresponding to year.in, and a lambda and a phi,
  ## this function calculates the superpopulation size Ns corresponding to the specific years in years.out,
  ## as well as the ENt's for these years.
  ## Unlike previous versions of this function, the Ns calculation accommodates survey gaps and ONLY
  ## contains animals that were ever alive during a survey.
  ## EXAMPLE:
  ## Ns.calc.func(nalive.in=340, year.in=2003,
  ##                lambda=1.03, phi=0.95, years.out=c(2001:2003, 2008, 2011:2014))
  ## gives the superpopulation size Ns for 2001 to 2014 exposed to surveys in the given years and
  ## corresponding to a population with Nalive=340 in 2003, growing at lambda=1.03 and with phi=0.95.
  ##
  
  year1 <- min(years.out)
  yearmax <- max(years.out)
  allyears <- year1:yearmax
  totyears <- length(allyears)
  
  ## year1 is the first year for which we want a prediction.  This formula for nalive.1 (number alive in year 1)
  ## works no matter whether year.in is greater than, equal to, or less than year1.
  nalive.1 <- nalive.in / lambda^(year.in - year1)
  
  ## Establish pentvec to span the whole range from year1 to yearmax, including surveys that don't take place:
  pentvec <- rep(0, totyears)
  ## Set pent[1] to 1 and rescale later:
  pentvec[1] <- 1
  
  ## Establish Ntvec:
  Ntvec <- rep(0, totyears)
  Ntvec[1] <- nalive.1
  
  ## If we imagine there is 1 animal present in year 1, then there should be lambda^(t-1) present in year t.
  ## Of these, the number "new" (entered) in year t is all except the survivors from previous entries.
  ## Thus the relative value of pent[t] if pent[1]=1 is:
  ## pent[t] = lambda^(t-1) - (sum over previous surveys j < t) pent[j] * phi^(t-j)
  ## i.e. we're taking each animal from the time it first entered (j), and asking how many periods it needs
  ## to survive in order to be still present at time t.
  if(length(years.out)>1) for(t in years.out[-1]){
    ## tval translates t to time origin year1=1:
    tval <- t - year1 + 1
    popsize.t <- lambda^(tval-1)
    jvec <- 1:(tval-1)
    ## Years that had no survey just have pentvec = 0 so don't contribute to this sum:
    pentvec[tval] <- popsize.t - sum( pentvec[jvec]*phi^(tval-jvec) )
    ## Fill in the value for Nt:
    Ntvec[tval] <- nalive.1 * popsize.t
  }
  ## Now scale pentvec so it sums to 1:
  pentvec <- pentvec / sum(pentvec)
  
  ## Then superpopulation size satisfies Ns * pent.1 = nalive.1, so back-calculate it:
  Ns <- nalive.1 / pentvec[1]
  ## Cumulative superpopulation size, cumNs:
  cumNs <- cumsum(Ns * pentvec)
  
  ## Calculate the Nt for the years requested:
  nalive.out <- data.frame(year=allyears, cumulativeNs=cumNs, pent=pentvec, ENt=Ntvec)
  ## Reduce the table to only the years specified in years.out:
  nalive.out <- nalive.out[match(x=years.out, table=nalive.out$year),]
  
  list(yeartable=nalive.out, Ns=Ns)
  
}


############################################################

pent.calc.func <- function(res.clean, res.setup){
  ## pent.calc.func 28/11/15
  ## Takes a data frame of results, res.clean, which has already been cleaned by clean.result.func,
  ## and the corresponding setup, and fills in any columns corresponding to pent parameters that are
  ## "calculated".
  ## For a non-lambda-model, this is typically pent1 which is calculated as 1 minus the sum of the other pents,
  ## which might include constant or non-estimated values.
  ## For a lambda-model, we need to perform the calculation of pents based on lambda and phi.
  
  ## modelTable contains entries for survey-years only:
  modelTable <- res.setup$ModelTable[res.setup$ModelTable$survey,]
  nres <- nrow(res.clean)
  n_datasets = attributes(res.clean)$n_datasets
  
  if(!res.setup$lambdamodel){
    ## ---------------------------------------------------------------------------------------
    ## NOT A LAMBDA-MODEL:
    ## Find the name of the pent parameter that is calculated (there should only be one):
    ## First remove any non-survey rows from ModelTable:
    which.calc <- modelTable$timeSlot[modelTable$pent=="calc"]
    if(length(which.calc)>1)
      stop("Something's wrong: there is more than one pent=calc parameter, but this is not a lambda model?")
    pent.name <- paste0("pent", which.calc)
    ## Typically, pent.name will be "pent1".
    ## Now find the formula for calculating this pent. When this pent is removed from modelTable$pent,
    ## we want 1 - all the other entries.  For example, if the other entries are c("pent2", "pent2", 0.2)
    ## then we want 1 - pent2 - pent2 - 0.2.
    pent.others <- modelTable$pent[-which.calc]
    pent.others[is.na(suppressWarnings(as.numeric(pent.others)))] <- paste0("res.clean$",
                                                                            pent.others[is.na(suppressWarnings(as.numeric(pent.others)))])
    pent.eval <- paste0("1-", paste0(pent.others, collapse="-"))
    ## Add the corresponding column to res: for example, in the example above we create column
    ## res$pent1 = 1 - res$pent2 - res$pent2 - 0.2:
    res.out <- res.clean
    res.out[[pent.name]] <- eval(parse(text=pent.eval))
  }
  if(res.setup$lambdamodel){
    ## ---------------------------------------------------------------------------------------
    ## LAMBDA-MODEL:
    
    ## It is a lambda-model: need to calculate the "calc" pent parameters given lambda, phi for each
    ## of the results in res:
    ## Find pentnames from modeltable:
    which.calc <- modelTable$timeSlot[modelTable$pent=="calc"]
    if(length(which.calc)<nrow(modelTable)) stop("Something's wrong: this is a lambda-model, but the pent column is not comprised only of 'calc'?")
    pent.names <- paste0("pent", which.calc)
    
    ## A special case for lambda-models: if either phi or lambda are specified as constants in the
    ## model, they aren't in the res matrix so they need to be calculated separately:
    lambda.const <- suppressWarnings(as.numeric(res.setup$allparvec["lambda"]))
    phi.const <- suppressWarnings(as.numeric(res.setup$allparvec["phi1"]))
    if(!is.na(lambda.const)) res.lambda <- rep(lambda.const, nres)
    else res.lambda <- res.clean[["lambda"]]
    if(!is.na(phi.const)) res.phi <- rep(phi.const, nres)
    else res.phi <- res.clean[[modelTable$phivec[1]]]
    
    ## Continuing with the pent calculation, extract survey details k, gapvec, cumvec:
    k <- length(res.setup$survey.years)
    gapvec <- res.setup$gapvec
    cumvec <- res.setup$cumvec
    ## Create the pent data-frame:
    pent.mat <- matrix(0, nrow=nres, ncol=k)
    pent.mat[, 1] <- rep(1, nres)
    ## We already know that k>=2 or else the models won't run.
    for(t in 2:k){
      pent.mat[,t] <- (res.lambda - res.phi) *
        sapply(1:nres, function(i) sum(res.phi[i]^(0 : (gapvec[t-1]-1)) *
                                         res.lambda[i]^((cumvec[t]-1):cumvec[t-1])))
    }
    ## Rescale all pent results so they add to 1:
    pent.mat <- pent.mat/rowSums(pent.mat)
    ## Convert to a dataframe:
    pent.df <- data.frame(pent.mat)
    ## Rename the columns of pent.df to pent.names:
    names(pent.df) <- pent.names
    ## Adjoin pent.df to res:
    res.out <- data.frame(res.clean, pent.df)
  }
  attributes(res.out)$n_datasets <- n_datasets
  return(res.out)
}

############################################################

# Function to get Nt and pent values from Ns, lambda, and phi.
# From Rfunc.R in Rachel's Maui work.
nalive.calc.func <- function(survs=survey.years, Ns=135.0, lambda=0.9706, phi=0.8413){
  ## nalive.calc.func 10/6/14
  ## NOTE: this has been UPDATED since the equivalent in the HUMPBACK directory, and now matches
  ## the values from the CAPOW interface.  The difference is that survey.years matters in this function:
  ## any animals born and died during a gap are not included in Ns and therefore they need to be adjusted for
  ## in nalive.t.
  ##
  ## NOTE: function Ns.calc.func does the opposite of this: calculates N for a given nalive.
  ##
  
  ## First calculate pent as determined by lambda and phi:
  firstsurv <- min(survs)
  lastsurv <- max(survs)
  nsurv <- length(survs)
  ## Set the pent for the first survey equal to 1, temporarily:
  pentvec <- rep(0, nsurv)
  pentvec[1] <- 1
  
  ## For other surveys, find the pent's relative to the first survey:
  gapvec <- diff(survs)
  cumvec <- c(0, cumsum(gapvec))
  for(t in 2:nsurv){
    pentvec[t] <- (lambda - phi) *
      sum(  phi^(0 : (gapvec[t-1]-1)) * lambda^((cumvec[t]-1):cumvec[t-1]))
  }
  ## Rescale all the pents to add to 1:
  pentvec <- pentvec/sum(pentvec)
  
  ## Now find the Nalive values, Nt.
  ## Nt for the first survey is just Ns * pent[1]:
  realyearspan <- lastsurv - firstsurv + 1
  Ntvec <- rep(0, realyearspan)
  Ntvec[1] <- Ns * pentvec[1]
  survey.counter <- 1
  
  for(t in 1:(realyearspan-1)){
    Ntvec[t+1] <- Ntvec[t] * phi
    ## If there is a survey at time t+1, add Ns * pent[t+1]:
    if(any(survs==firstsurv+t)){
      survey.counter <- survey.counter + 1
      Ntvec[t+1] <- Ntvec[t+1] + Ns * pentvec[survey.counter]
    }
  }
  
  Nt <- Ntvec[which((firstsurv:lastsurv)%in% survs)]
  
  cbind(survs, Nt, pentvec)
}

######################################################################

# Function containing code from Rachel's maui.sim.func to attribute existing
# capture histories to simulated animals with appropriate life spans. 
# Inputs
#   dat - the existing data, captures and non-captures for animals caught at least
#     once.
#   survs - the survey years.
#   alive.dat - the life histories of the simulated animals.
#   det.hist - the capture histories of the simulated animals.
# Outputs
#   det.hist - updated set of capture histories.
#   animals.not.placed - character vector of animals whose capture histories could
#     not be accomodated, as "a" + the index of the animal in the existing dataset.

attr_cap_hists <- function(dat, survs, alive.dat, det.hist) {
  ## Now go through the original data, dat, and for each entry try to find a row of alive.dat that can yield those
  ## detections.  Overwrite det.hist[,1:length(survs)] with the original data where possible.
  ## Start with the rows of dat that have the longest spans of detections.
  
  ## For each dolphin in dat, find its first sighting and last sighting:
  first.obs <- apply(dat, 1, function(x) min(which(x == 1)))
  last.obs <- apply(dat, 1, function(x) max(which(x == 1)))
  first.obs.surv <- survs[first.obs]
  last.obs.surv <- survs[last.obs]  ## last.obs.surv might be 2011, whereas last.obs would be 7, the 7th survey
  
  # Span is only used for ordering the observed animals to search for histories,
  # which is actually important because you need to place the hard ones first to
  # maximise the chances of success.
  span <- last.obs.surv - first.obs.surv + 1
  
  ## Now try to apportion each animal in dat to a row of the augmented alive.dat.
  ## Start with the biggest spans which are the hardest to accommodate:
  N <- nrow(alive.dat)
  taken.rows <- rep(0, N)
  
  ## where.put says which row animal i of original data "dat" is put into in the new data frame.
  # Not using in capow.
  # where.put <- rep(0, nrow(dat))
  
  ## animals.not.placed gives the animal row from the original data, and the corresponding span,
  ## of any animals from the original data that can't be fitted into the new data.
  ## e.g. if animals.not.placed = c(a3=10, a17=9) it means that original rows 3 and 17, with corresponding
  ## spans 10, and 9, have been omitted from the new data frame.
  animals.not.placed <- NULL
  
  for(sp in rev(sort(unique(span)))){
    which.sp <- which(span == sp)  ## which.sp are the animals in the original data who have this span
    first.sp <- first.obs[span == sp]
    last.sp <- last.obs[span == sp]
    n.sp <- length(which.sp)
    
    ## Find all rows of alive.dat such that animals are alive in both columns first.sp and last.sp:
    for(i in 1:n.sp){
      ## Deal with animal i within span sp:
      allowed.rows <- which(alive.dat[, first.sp[i]] == 1 & alive.dat[, last.sp[i]] == 1 & taken.rows == 0)
      
      ## Pick one of the allowed rows at random:
      chosen.row.i <- NA
      
      # If there is an animal which could have had this capture history
      if(length(allowed.rows) > 0){
        
        ## first line is to avoid the oddity in "sample" when length = 1
        if(length(allowed.rows) == 1) chosen.row.i <- allowed.rows
        else chosen.row.i <- sample(allowed.rows, size = 1)
        taken.rows[chosen.row.i] <- 1
        
        ## Overwrite the new data in chosen.row.i with the original data for which.sp[i]:
        det.hist[chosen.row.i, 1:length(survs)] <- as.vector(unlist(dat[which.sp[i],]))
        
        ## Log what has been done:
        # where.put[which.sp[i]] <- chosen.row.i
      }
      else{
        ## If there aren't any rows available, add the record to animals.not.placed.
        ## The where.put ticket will be 0 for these animals.
        animal.span <- sp
        names(animal.span) <- paste("a", which.sp[i], sep="")
        animals.not.placed <- c(animals.not.placed, animal.span)
      }
    }  ## End of animal i with span sp.
  }  ## End of span=sp.
  
  # Return updated set of capture histories and vector of animals whose capture
  # histories could not be accomodated.
  attributes(det.hist)$animals.not.placed <- animals.not.placed
  det.hist
}