# Things to do:

# *** First priority ***

# Tidy up module files and write explanation.

# Tidy up code to load module UIs.

# If I don't have one already, make an example to show boundary estimates with
# plausible variance estimates, and make sure code reports them but notes that
# they're illegitimate.  Also one that shows failure to converge.  Write down all
# my examples.

# Tidy up MatLay function in CaPowGUIFuncs.R

# Fix confusing error when try to display power plot for lambda for non-lambda
# models. And non estimated parameters generally?  This has caused long
# unnecessary freak-outs more than once!

# Fix error if try to boxplot lambda for non-lambda model.

# Make it more clear that running projects takes time?  Change info message
# about R console.

# Add a message in the Load tab to confirm when objects are loaded successfully.

# Fix NA parameter inputs in sim builder from non-lambda models?  Not supposed
# to use them yet, so maybe ok.  May be removing launch from data anyway.

# Add/improve messages in power and plot modules when they do something dumb
# like not select a project.  Stop them selecting lambda for non lambda
# projects?  Users are dumb, get good at creating messages stating the obvious.

# The value in Ns is wrong, because it's based on the initial values of timeN
# and timeopt?  Doesn't seem to update?  That would be cool in general.  But the
# way the code is now you're supposed to work out the Ns separately and enter
# it.  So maybe I should just delete it when a dataset is selected?  I dunno.
# Seems to be wrong though.  I guess long-term I want it to update.  Probably
# not important for now though?  
# Do something to stop parameter inputs implied by fit models being changed in
# sim builder?
# Fix problem with selecting extended surveys not enabling cap prob inputs
# sometimes unti switching datasets back and forth.

# Stop it crashing when remove fit that is currently used in sim builder.  Just
# update the input before removing it, maybe with a delay?
# Update RI server to require that fits not included in any simulations, and
# models and datasets not included in any fits, before removal.

# Find tick the save box messages from the recordings and remove them.

# This existing data extension method doesn't work.  It introduces huge bias
# when the simulated population is incongruous with the existing data.  We
# should just take the estimated population size at the end of the study and
# simulate from there.

# Fix error when phi close or equal to lambda. Does constrOptim fix this?
# Probably right?

# *** Second priority ***

# Try profiling it with profvis(runApp()) again.

# Change error message when no projects selected in power module.

# Plot titles for mean plots?  
  
# Smaller project titles in plots and summaries tab?  And power analysis tab?
  

# Remove the nlminb code but leave the note about why.

# Remove all stop() calls in capow_tmb.

# Add E(Nt) to the power analysis and diagnostics.

# Change stard error calculations to all use TMB functions rather than inverting
# hessians ourselves? Stop warnings from TMB when model not identifiable:
# Warning in sqrt(diag(cov)) : NaNs produced Warning in
# sqrt(diag(object$cov.fixed)) : NaNs produced

# Reverse order of diagnostics and power modules.

# Go back to constroptim to avoid Nan warnings?

# Make plots update when selected rather than requiring button click?

# Rewrite the entire simserver function.

# Make explicit the requirement that Ns and capture probabilities are estimated,
# and that only popan-lambda models are fit, for fit results to be used for
# simulations and don't allow others to be selected.  Is this currently just
# crashing the app?  Seems like just fills in lots of NA's which is reasonable
# haha.

# Cut up lecture recordings by topic, edit subtitles, and add to app.  Make sure
# to download them before they get deleted in June.

# Source functions more logically, not whole capow_tmb in every session.

# Display parameters held constant, calculated, in fit results?

# Errors from not having the constraint on lambda and phi in nlminb?  Go back to
# using constroptim?  Delete the nlminb version.  Check start values.  May have
# just been in viewfits.

# Use max(pent[1], 0) to avoid negative values, and min(pent[1], 0) to add a
# penalty to the likelihood?  Maybe use calcind in case it's not pent[1] that
# should be calculated?  Definitely use constrOptim to enforce the phi < lambda
# constraint.  The pent thing might not be a problem after all.

# ConstrOptim vs nlminb seems to be small and mainly when non-identifiability.

# Allow datasets to be removed without removing dependent objects.  Include
# checks on existence of datasets.  Really?  Or just allow Saving like that?
# That's no better.

# Remove mean plots?  Or make them comparable?  Scale by true param size?

# Figure out how the hell environments work lol

# Stop survey selections disappearing when you change number of times.

# Limit number of simulations to 5000.

# Make results etc easily available to users through R, and/or otherwise.

# Indicate parameters that are implied by data, at least/maybe just with line
# dividing parameter matrix?

# Change simbuilder and other functions to use req instead of checking for null.
# Could be related to below error.

# Remove Ns calculator and do it automatically?  

# Change code relying on phi1 to be, no, it's right for lambda models?  No, bc
# phi starts at the first survey, which might not be the first time period,
# though it probably should be?  popan.func and simbuilder?

# Could have one removal module/function that I pass each list to, up to checks
# of components to remove first?

# Tidy up code.
# Break large functions up into smaller functions.  
# Move related functions to their own files.

# Change default plot sizes or titles so that they're not cut off.
# Also CI coverage %'s in box plots.

# Change plot settings inputs in power and plot modules to follow same format,
# separate tab or optionally show at top.

# Display plot of expected population dynamics when model fit to dataset.  Could
# do the same for power analyses, could be pretty cool and help people
# understand what's going on.

# Update plots to look nicer, probably with ggplot2.

# Change sim builder so can't unselect surveys for existing data.

# Stop power module complaining when button pushed but no projects selected.

# Make model module update sim builder module if possible.
# Fix View Model and Sim displays to make space between lambdalambdalambda.
# Make View Sim and Sim List displays show whether/which dataset/fit included.


# Automate the checks for boundary estimates, numerical, and theoretical
# non-identifiability, and print big warnings, and remove them from power
# analysis!?

# Make sim function allocate capture histories to animals for which they are
# more likely.  I think this might be important considering the bias in the
# estimate of phi for the "est" setting of the Maui work.  Though now thinking
# that's just because difficult scenario, as seems to go in same direction as
# running the same thing without the data.  But seems to happen for rat data
# whereas it has no problem without the data...  Doesn't look good.

# Check that sim function attribution function is efficient.
# Report number of capture history attribution failures or something.

# Make short demonstration video.

# Write MEE paper.

# Learn more about how shiny works with local in viewfits

# Stop survey checkboxes in model and sim builders continuously updating when
# double-clicked. Seems to be due to renderUI changing inputs.  May not be
# solvable in Shiny yet.  Maybe using req()?