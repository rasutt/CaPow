displaySim <- function(res){
        ## displaySim is for displaying a single SimSet in nice HTML.
        ##
        ## HTML code for ticks:
        ## Empty checkbox:  &#9744;
        ## Ticked checkbox: &9745;
        ## Single tick version 1: &#10003;
        ## Single tick version 2: &#10004;

        resdf <- res$paramdf

        ## Convert survey selection timeopt column to ticks where True, and blank where False:
        resdf$timeopt[resdf$timeopt==T] <- "<font size=5>&#10003;</font>"
        resdf$timeopt[resdf$timeopt==F] <- " "

        ## Change names of the table to make them more informative:
        names(resdf)[names(resdf)=="timelabels"] <- "Time:"
        names(resdf)[names(resdf)=="timeopt"] <- "Survey:"
        names(resdf)[names(resdf)=="capturepr"] <- "Capture probability, p:"
        names(resdf)[names(resdf)=="Nt"] <- "E(N<sub>t</sub>):"
        ## If it's a lambda model, replace Relative Pent with lambda:
        if(res$simtype=="lambdasim"){
                ## Replace the row by lambda, and make it so that it has the same blank entries as phi:
                resdf$prentry <- rep(as.character(res$lambdaparam), length(resdf$prentry))
                resdf$prentry[resdf$survrate==""] <- ""
                names(resdf)[names(resdf)=="prentry"] <- "Growth rate, &lambda;:"
        }
        ## Otherwise, leave it as Relative Pent:
        else{
                names(resdf)[names(resdf)=="prentry"] <- "Relative p<sub>ent</sub>:"
        }
        names(resdf)[names(resdf)=="pentscaled"] <- "Entry proportion, p<sub>ent</sub>:"
        names(resdf)[names(resdf)=="survrate"] <- "Survival, &Phi;:"


        ##	tabstr <- "<p><table border=\"0\">"
        tabstr <- "<p><table style=\"border:solid; border-width:1px; border-color:#BFBFBF\">"

        ## Top row: superpopulation:
        tabstr <- paste(tabstr, "<tr><th style=\"text-align:right; padding:2px\">Superpopulation, N<sub>s</sub>: </th><td>",
                        res$superpopn, "</td></tr>")


        for(i in 1:ncol(resdf)){
                ## Column i of resdf will be row i of the output:
                rowstart <- paste("<tr><th style=\"text-align:right; padding:2px\">", names(resdf)[i], "</th>  <td style=\"width:1em\"></td><td style=\"width:4em\">")
                rowbody <- paste(resdf[,i], collapse="</td>  <td style=\"width:4em\">")
                rowend <- paste("</td> </tr>")
                rowstr <- paste(rowstart, rowbody, rowend, collapse=" ")
                tabstr <- paste(tabstr, rowstr, collapse=" ")
        }
        tabstr <- paste(tabstr, "</table></p><p style=\"margin-bottom:1.4cm\"></p>", collapse=" ")
        tabstr

}


###########################################################
displayModel <- function(res){
        ## displayModel is for displaying a single Model in nice HTML.
        ## Almost identical to displaySim.

        resdf <- res$paramdf

        ## Replace "calculated" by "calc" throughout: calculated is too long.
        resdf[resdf=="calculated"] <- "calc"

        ## Convert survey selection timeopt column to ticks where True, and blank where False:
        resdf$timeopt[resdf$timeopt==T] <- "<font size=5>&#10003;</font>"
        resdf$timeopt[resdf$timeopt==F] <- " "

        ## Change names of the table to make them more informative:
        names(resdf)[names(resdf)=="timelabels"] <- "Time:"
        names(resdf)[names(resdf)=="timeopt"] <- "Survey:"
        names(resdf)[names(resdf)=="capturepr"] <- "Capture probability, p:"
        ## If it's a lambda model, replace Pent with lambda:
        if(res$modeltype=="lambdamodel"){
                ## Replace the row by lambda, and make it so that it has the same blank entries as phi:
                resdf$prentry <- rep(as.character(res$lambdaparam), length(resdf$prentry))
                resdf$prentry[resdf$survrate==""] <- ""
                names(resdf)[names(resdf)=="prentry"] <- "Growth rate, &lambda;:"
        }
        ## Otherwise, leave it as Pent:
        else{
                names(resdf)[names(resdf)=="prentry"] <- "Entry proportion, p<sub>ent</sub>:"
        }
        names(resdf)[names(resdf)=="survrate"] <- "Survival, &Phi;:"


        ##	tabstr <- "<p><table border=\"0\">"
        tabstr <- "<p><table style=\"border:solid; border-width:1px; border-color:#BFBFBF\">"

        ## Top row: superpopulation:
        tabstr <- paste(tabstr, "<tr><th style=\"text-align:right; padding:2px\">Superpopulation, N<sub>s</sub>: </th><td>",
                        res$superpopn, "</td></tr>")


        for(i in 1:ncol(resdf)){
                ## Column i of resdf will be row i of the output:
                rowstart <- paste("<tr><th style=\"text-align:right; padding:2px\">", names(resdf)[i], "</th>  <td style=\"width:1em\"></td><td style=\"width:4em\">")
                rowbody <- paste(resdf[,i], collapse="</td>  <td style=\"width:4em\">")
                rowend <- paste("</td> </tr>")
                rowstr <- paste(rowstart, rowbody, rowend, collapse=" ")
                tabstr <- paste(tabstr, rowstr, collapse=" ")
        }
        tabstr <- paste(tabstr, "</table></p><p style=\"margin-bottom:1.4cm\"></p>", collapse=" ")
        tabstr

}


###########################################################

# Changed because currently sourcing before now user-specific CPenv created, but probably a simpler implementation later
# DisplSim.func <- function(simnames){
DisplSim.func <- function(simnames, SimList){
        ## Function to display all existing SimSets.  Takes a list of sim names and calls displaySim for each one.
        ## DisplaySim displays the simset in an HTML table.  This function is called from ui.R in various UI
        ## directories.
	simnames <- naturalsort(simnames)
        outprnt <- NULL
        if(!is.null(simnames)) for(simname in simnames){
                # res <- get("SimList", envir=CPenv)[[simname]]
                res <- SimList[[simname]]
                outprnt <- paste(outprnt, h4(HTML("&nbsp;SimSet ", simname)))
                outprnt <- paste(outprnt, HTML(res$description))
                outprnt <- paste(outprnt, displaySim(res))
        }
        HTML(outprnt)
}

###########################################################################

# Changed because currently sourcing before now user-specific CPenv created, but probably a simpler implementation later
# DisplModel.func <- function(modelnames){
DisplModel.func <- function(modelnames, ModelList){
        ## Function to display all existing models.  Takes a list of model names and calls displayModel for each one.
        ## DisplayModel displays the model in an HTML table.  This function is called from ui.R in various UI
        ## directories.
	modelnames <- naturalsort(modelnames)
        outprnt <- NULL
        if(!is.null(modelnames)) for(modelname in modelnames){
                # res <- get("ModelList", envir=CPenv)[[modelname]]
                res <- ModelList[[modelname]]
                outprnt <- paste(outprnt, h4(HTML("&nbsp;Model ", modelname)))
                outprnt <- paste(outprnt, HTML(res$description))
                outprnt <- paste(outprnt, displayModel(res))
        }
        HTML(outprnt)
}



###########################################################

matLay =
        ## A function for generating a HTML Table
        ##  from a R matrix object.
        ## The output HTML Table is for layout purposes
        ##  and cannot be accessed as a whole (but elements
        ##  inside the Table can, as long as they have IDs).
        function(layinp, ndatarow = NULL, padding = "5px") {
                tagList(
                        if(!is.null(layinp) && nrow(layinp) > 0)
                        {
                                # Loop over non-data rows and make an HTML table
                                curtab = NULL
                                for(i in 1:nrow(layinp)){
                                        currow = NULL
                                        for(j in 1:ncol(layinp))
                                                currow = c(currow, list(tags$td(layinp[i, j],
                                                                                style = paste0("padding:", padding))))
                                        curtab = c(curtab, list(tags$tr(currow)))
                                }
                                tags$table(class = "matLay", curtab)
                        }, tags$script(
                                '(function(){
                        var matLayTextFix = function(){
                                var matLayText = $("table.matLay input[type=\'text\']");
                                matLayText.off("input");
                                matLayText.off("keyup");
                        }
                        window.setTimeout(matLayTextFix, 100);
                        /*var matLayCheckboxFix = function(){
                                var matLayCheckbox = $("table.matLay input[type=\'checkbox\']");
                                matLayCheckbox.off("input");
                                matLayCheckbox.off("change");
                        };
                        window.setTimeout(matLayCheckboxFix, 100);*/
                  })()'))
                ## The code block above starting tags$script is added Jan 2016 and fixes
                ## the problem that users had to type briskly into the matlay boxes or else
                ## an endless loop might result.
        }

################################################################

matLayAc =
        ## An Accessor function for matLay for generating
        ##  complex matLay output that contains shinyUI elements.
        function(colTypes = NULL, colIDs = NULL, colArgs = NULL, colNames = NULL, nrow = 1,
                 ndatarow = NULL, ## number of data rows so can rule line under them
                 print.rownames=T){

                ndigits = nchar(nrow)
                ## Check and correct for args
                if(is.null(colTypes))
                        stop("colTypes must be specified")
                ncol = length(colTypes)
                if(is.null(colIDs))
                        colIDs = paste0("col", 1:ncol, "row")
                if(length(colIDs) != ncol)
                        colIDs = rep(colIDs, length = ncol)

                outmat = NULL
                for(i in 1:nrow){
                        currow = NULL
                        for(j in 1:ncol){
                                rowargs = colIDs[j]
                                rowargs[rowargs != ""] = paste0(rowargs,
                                       sprintf(paste0("%0", ndigits, "d"), i))
                                rowargs = c(list(id = rowargs),
                                lapply(colArgs[[j]], function(x) rep(x, length = nrow)[i]))
                                currow = c(currow, list(do.call(colTypes[j], rowargs)))
                        }
                        outmat = rbind(outmat, currow)
                }

                if(!is.null(colNames))
                        outmat = rbind(lapply(colNames, HTML), outmat)

                if(print.rownames){
                        outmat <- cbind(c("", 1:nrow), outmat)
                }

                matLay(layinp = outmat, ndatarow = ndatarow)
        }

################################################################

extInput =
        ## Extended Input accessor function for HTML input elements
        ##  allowing specification of any of its attributes
        ## Extra things extInput does:
        ##  - Adds capability to add caption/labels to the left or right
        ##    of the input element.
        ##  - Makes checked/disabled attribute more robust such that
        ##    it accepts a logical and adapts HTML accordingly.
        ## Some useful attributes:
        ##  id
        ##  checked = "checked" for type = "checkbox" or "radio"
        ##  disabled = "disabled"
        ##  pattern = regexp to check value against
        ##  required = "required"
        ##  style = CSS styles
        ##  type = "button", "checkbox", "number", "radio", "text"
        ##  value = text - starting value
        ##  reverse : swaps around the label and the entry box.  reverse=T puts label to the right of the box.
        function(..., label = NULL, reverse = FALSE, checked = FALSE, disabled = FALSE, labelwidth = NULL){
                attrlist = list(...)
                if(checked == TRUE)
                        attrlist = c(attrlist, list(checked = "checked"))
                if(disabled == TRUE)
                        attrlist = c(attrlist, list(disabled = "disabled"))
                out = do.call(tags$input, attrlist)
                if(!is.null(label))
                        out = c(list(HTML(paste("&nbsp;", label, "&nbsp;"))), list(out))
                if(reverse)
                        out = rev(out)
                out
        }

################################################################

efeval = function(inptext, TimeN){
        ## Evaluates EasyFill (ef) expressions
        ## Shortcuts:
        ## p(t), phi(t), pent(t) become their obvious results: can be entered into EasyFill with or without quotes, as p(t) or "p(t)"
        ## p(.), phi(.), pent(.) become p1 everywhere, phi everywhere, and pent2 everywhere:
        ## (note we use pent2 because pent1 will be different for the start of the survey).
        ## Likewise these can be entered into EasyFill with or without quotes, as p(.) or "p(.)"
        ##
        ## For more elaborate expressions:
        ## %t becomes 1:TimeN
        ## _  becomes paste0 on elements.


        ## Shortcuts: p(t), phi(t), and pent(t):
        if(inptext=="p(t)" | inptext=="\"p(t)\"") return(paste("p", 1:TimeN, sep=""))
        if(inptext=="p(.)" | inptext=="\"p(.)\"") return(rep("p1", TimeN))

        if(inptext=="phi(t)" | inptext=="\"phi(t)\"") return(paste("phi", 1:TimeN, sep=""))
        if(inptext=="phi(.)" | inptext=="\"phi(.)\"") return(rep("phi", TimeN))

        if(inptext=="pent(t)" | inptext=="\"pent(t)\"") return(paste("pent", 1:TimeN, sep=""))
        if(inptext=="pent(.)" | inptext=="\"pent(.)\"") return(rep("pent2", TimeN))

        ## ------------------------------------------------------------------------
        ## If there are commas, treat it as a vector that has been input directly.  Cycle it to length TimeN if necessary:
        if(length(grep(",", inptext)>0)){
                cpts <- strsplit(inptext, split=",")[[1]]
                ## cpts is a vector of strings.  The following gsub command removes leading or trailing spaces from
                ## each element of the vector, but leaves any interior spaces intact:
		## Apparently the same effect is got by space instead of \\s, as in:
		## gsub("^ +| +$", "", x)
                cpts.clean <- gsub("^\\s+|\\s+$", "", cpts)
                ## If what is obtained is the wrong length, make it the right length:
                if(length(cpts.clean) < TimeN)  cpts.clean <- rep(cpts.clean, times=TimeN)[1:TimeN]
                return(cpts.clean)
        }

        ## ------------------------------------------------------------------------
        ## More complicated expressions using %t and "_" for paste:

        replace.t.func <- function(elt){
                ## This function replaces the character string %t with the numeric values 1:TimeN,
                ## and evaluates whatever it gets.
                ## e.g. replace.t.func("p") = "p"
                ## whereas replace.t.func("%t + 1") = c(2, 3, ..., TimeN+1)
                if(length(grep("%t", elt, fixed=T))>0){
                        return(eval(parse(text=gsub("%t", "1:TimeN", elt, fixed=T))))
                }
                else elt
        }

        ## ------------------------------------------------------------------------
        ## First deal with paste characters:
        if(length(grep("_", inptext, fixed=T))>0){
                ## If there is a paste character in the string, split up the string so that each component to be pasted
                ## is a separate member of paste.elts:
                ## e.g. if inptext="p_(%t + 1)_a"  then paste.elts = c("p", "(%t+1)", "a"):
                paste.elts <- strsplit(inptext, split="_", fixed=T)[[1]]
                ## Replace %t in each of the paste components:
                ## e.g. in the case above, elts.with.t.replaced will be a list with components "p",  c(2,3,4,5), and "a":
                elts.with.t.replaced <- sapply(paste.elts, replace.t.func)
                ## The following line pastes together all components of the list, however many there are,
                ## e.g. if there are three components the line has the effect of saying
                ## paste0(elts.with.t.replaced[[1]], elts.with.t.replaced[[2]], elts.with.t.replaced[[3]])
                ## It will return c(p2a, p3a, p4a, p5a) in the case above.
                if(is.list(elts.with.t.replaced))
                        out.text <- do.call("paste0", elts.with.t.replaced)
                else
                        out.text <- paste0(elts.with.t.replaced, collapse="")
        }
        else{
                ## No paste characters in the string, so we only need to evaluate a single element
                ## by replacing %t if necessary:
                inp.with.t.replaced <- replace.t.func(inptext)
                out.text <- inp.with.t.replaced
        }

        ## Cycle vector if it has length less than TimeN:
        if(length(out.text)<TimeN) out.text <- rep(out.text, times=TimeN)[1:TimeN]
        out.text

}

######################################################################

HTMLnoid =
        function(text, id, ...)
        HTML(text, ...)

######################################################################

old.efeval =
        ## Jimmy's former EasyFill function
        ## Evaluates EasyFill (ef) expressions
        ## Allows for these special shortcuts:
        ## %i -> 1:TimeN
        ## %+ -> paste0 on elements
        function(inptext, TimeN){
                ## Convert any %i
                inptext = gsub("%i", "1:TimeN", inptext, fixed = TRUE)
                ## If any %+
                if(length(grep("%+", inptext, fixed = TRUE)) > 0){
                        ## Split by %+, put them together as paste inputs
                        ##  call paste0 on it
                        inptext = paste0("paste0(",
                        paste(strsplit(inptext, "%+", fixed = TRUE)[[1]],
                              collapse = ","), ")")
                }
                outtext <- eval(parse(text = inptext))
                ## Rachel's addition: cycle vector if it has length less than TimeN:
                if(length(outtext)<TimeN) outtext <- rep(outtext, times=TimeN)[1:TimeN]
                outtext
        }

######################################################################


