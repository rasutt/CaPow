// POPAN negative log-likelihood function for TMB
// Rewritten from capow-fast.R, capow with Robin's modifications for speed, with original comments and structure where possible.

#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_INTEGER(k);
  DATA_INTEGER(lambdamodel);
  DATA_IVECTOR(gapvec);
  DATA_SCALAR(nhist);
  DATA_VECTOR(firsttab);
  DATA_VECTOR(lasttab);
  DATA_VECTOR(caps);
  DATA_VECTOR(noncaps);
  DATA_VECTOR(survives);
  DATA_VECTOR(constvalues);
  DATA_IVECTOR(indsinsertintoallvalues);
  DATA_IVECTOR(whichparsintoallvalues);
  DATA_IVECTOR(phiinds);
  DATA_IVECTOR(pentinds);
  DATA_IVECTOR(pinds);
  DATA_INTEGER(Nind);
  DATA_INTEGER(lambdaind); // index of lambda for lambda model, zero otherwise
  DATA_INTEGER(calcind); // index of one calc pent value for pent model, zero otherwise
  
  PARAMETER_VECTOR(pars);
  
  // // ## Using the vector pars, place all values into their appropriate slots.
  // ## For example, if allparvec is:
  // ## N lambda   phi1     phi2    phi3      p1       p2      p4    pent1  pent2  pent4
  // ## "N"  "1.1" "phi1" "phi1" "phi1"   "p1"   "p2"   "p4"  "calc"  "calc"  "calc"
  // ## then
  // ## pars.estvec = c("N", "phi1", "p1", "p2", "p4")
  // ## and pars is the same length as pars.estvec, but contains numbers for each of the parameter values.
  // ##
  // ## allvalues has the same length and names as allparvec, and contains the correct values for each
  // ## parameter slot.  Use the vectors which.pars.into.allvalues and inds.insert.into.allvalues created above
  // ## to put the parameter values in the right places.
  // ##
  // ## First set allvalues to constvalues: this will establish any constant values and have 0s everywhere
  // ## else.  The reason for using constvalues instead of allparvec is that constvalues is a numeric
  // ## vector whereas allparvec is a character vector.
  vector<Type> allvalues = constvalues;
  // ## Replace any parameter slots with parameters-to-be-estimated in allvalues with their current
  // ## values from pars:
  for(int i = 0; i < indsinsertintoallvalues.size(); i++) {
    allvalues(indsinsertintoallvalues(i)) = pars(whichparsintoallvalues(i));
  }

  // The code below is replaced by the code further down by skipping the step of putting it into replace.calc.func.
  // // ## Replace any parameter slots that are derived from other parameters with their derived values from
  // // ## pars:
  // // ## the function replace.calc.func is defined before this function, and is defined differently for
  // // ## lambda-models and for other models.
  // // ## For lambda-models, it assumes (after checks already completed in popan.setup.func) that all
  // // ## pent parameters are "calc".  For other models, it detects which single one of the pent parameters
  // // ## is to be calculated as 1 - sum-of-the-others.
  // // ## The "calc" entries in allparvec are only used for detecting which of the pent parameters to
  // // ## replace in non-lambda models: they aren't used directly for lambda models.
  // allvalues = replace.calc.func(allvalues)
  // names(allvalues) = names(allparvec)
  //
  // // ## Extract N, phivec, pvec, and pentvec from allvalues:
  // N = allvalues["N"]
  Type N = allvalues(Nind);
  // phivec = allvalues[phinames]
  // The length of phivec is not k - 1 but the number of years between the first and last surveys.
  // All of those between each survey are modelled by one parameter in allvalues.
  int phisize = phiinds.size();
  vector<Type> phivec(phisize);
  for(int i = 0; i < phisize; i++) {
    phivec(i) = allvalues(phiinds(i));
  }
  // pvec = allvalues[pnames]
  vector<Type> pvec(k);
  for(int i = 0; i < k; i++) {
    pvec(i) = allvalues(pinds(i));
  }
  // pentvec = allvalues[pentnames]
  //
  // // ## ------------------------------------------------------------------------------------------------------------------
  // // ## Create the code needed to fill in "calc" values with derived functions of other parameters
  // // ## ------------------------------------------------------------------------------------------------------------------
  // // ## Here we create a function "replace.calc.func" that will be called from the likelihood function
  // // ## to fill in any values in allparvec that are marked "calculated" and need to be replaced by derived
  // // ## values of other parameters.
  // // ## There are two cases for replace.calc.func:
  // // ## 1. For lambda-models, we need code to calculate all the pent values as a function of lambda and phi.
  // // ## 2. For other models, we need code to calculate one of the pent values as 1 - sum (other pents).
  //
  vector<Type> pentvec(k);
  Type lambda = 0;
  if(lambdamodel == 1) {
  //   // ## Lambda models: calculate pents as a function of lambda and phi.
  //   // ## popan.setup.func already checked that for a lambda model, all the pent parameters are "calc".
  //   // replace.calc.func = function(allvals){
  //     // ## We've already checked in popan.setup.func that the model specifies a single value of phi.
  //     // ## Use allvals[phinames][1] to extract what it is.
    Type phival = phivec(0);
  //     // ## Extract lambda:
    lambda = allvalues(lambdaind);
  //
  //     // ## Create the pent vector: note that k, gapvec, and cumvec are all globally defined.
    pentvec.setZero();
    pentvec(0) = 1;
  //     // ## We have already checked that k>=2.
  //
  //     // ## ** Start of Robin's code replacing section below **
  //     // ## This seems to be faster because it doesn't use a loop.
  //       phivec = phival^gapvec
  //       lambda = lambdaval^gapvec
  //       pentvec[2:k] = (lambda - phivec) * cumprod(c(1, lambda[1:(k-2)]))
  //     // ## ** End of Robin's code replacing section below **
    Type cumlambda = 1;
    for(int t = 1; t < k; t++) {
      pentvec(t) = (pow(lambda, gapvec(t - 1)) - pow(phival, gapvec(t - 1))) * cumlambda;
      cumlambda *= pow(lambda, gapvec(t - 1));
    }
  //     // # for(t in 2:k){
  //     // #         pentvec[t] =
  //     // #                 (lambdaval - phival) * sum(phival^(0 : (gapvec[t-1]-1)) *
  //     // #                                                    lambdaval^((cumvec[t]-1):cumvec[t-1]))
  //     // # }
    Type sumpentvec = pentvec.sum();
    for(int i = 0; i < k; i++) {
      pentvec(i) = pentvec(i) / sumpentvec;
    }
  //         // ## Now insert the pentvec back into allvals:
  //         allvals[pentnames] = pentvec
  //           return(allvals)
  //   }
  //   // ## ------end of replace.calc.func for lambda-models-------
  } else {
  //   // ## Non-lambda models: exactly one of the pents should be "calc".
  //   // ## Even if all pents are supplied as numbers, it is easier to leave one as "calc" and apply
  //   // ## this function anyway instead of adding an extra "if" check.
  //   // ## popan.setup.func already checked that which.calc has length exactly 1.
  //   which.calc = which(allparvec[pentnames]=="calc")
  //   replace.calc.func = function(allvals){
  //     allvals[pentnames][which.calc] = 1 - sum(allvals[pentnames][-which.calc])
  //     return(allvals)
  //   }
    for(int i = 0; i < k; i++) {
      pentvec(i) = allvalues(pentinds(i));
    }
    pentvec(calcind) = 1 - pentvec.sum();
  //   // ## ------end of replace.calc.func for non-lambda models-------
  }

  // // ## --------------------------------------------------------------------
  // ## Capture history calculations begin here:
  // ## --------------------------------------------------------------------
  //
  // ## Find the chi parameters.  chivec[t] = P(never seen after occasion t | alive at t).
  // ## Simultaneously with chivec, create a vector of length k-1 called psurvive.gap, such that
  // ## psurvive.gap[i] = P(survive from survey i to survey i+1 | alive at survey i)
  // ## and it is the product of phivec probabilities spanning the calendar years from survey i to survey i+1.
  vector<Type> chivec(k);
  chivec(k - 1) = 1;
  vector<Type> psurvivegap(k - 1);
  int phiminindex = phisize;
  int newphiminindex;
  for(int i = k - 2; i >= 0; i--) {
    // ## phivec.i contains the survival probabilities from survey i to survey i+1:
    // ## we need to go back gapvec[i] places from the previous position (phi.min.index)
    // ## and take all elements of phi starting there and ending at phi.min.index-1:
    // ## then redefine phi.min.index for the next loop.
    newphiminindex = phiminindex - gapvec(i);
    // phivec.i = phivec[new.phi.min.index : (phi.min.index-1)]
    // ## psurvive.gap[i] is the probability of surviving the interval from survey i to survey i+1.
    // psurvive.gap[i] = prod(phivec.i)
    psurvivegap(i) = Type(1.0);
    for(int j = newphiminindex; j < phiminindex; j++) {
      psurvivegap(i) *= phivec(j);
    }
    chivec(i) = Type(1.0) - psurvivegap(i) + psurvivegap(i) * (Type(1.0) - pvec(i + 1)) * chivec(i + 1);
    phiminindex = newphiminindex;
  }
  vector<Type> logpsurvivegap = log(psurvivegap);
  
  // Find E(Nt) and specify it as a derived parameter to calculate and report the standard error
  vector<Type> exp_n_alive(k);
  exp_n_alive(0) = N * pentvec(0);
  for(int i = 1; i < k; i++) {
    exp_n_alive(i) = exp_n_alive(i - 1) * psurvivegap(i - 1) + N * pentvec(i);
  }
  ADREPORT(exp_n_alive);

  // ## ** Start of Robin's additions here **
  // ## Find the probability up to the first capture in a history with:
  // ## prob.to.f.m.1[1] = pentvec[1]
  // ## prob.to.f.m.1[t+1] = prob.to.f.m.1[t] * (1 - pvec[t]) * psurvive.gap[t] + pentvec[t+1]
  // ## The log of this vector can then be multiplied by the first.tab vector found above and subtracted
  // ## from the negative log-likelihood.
  vector<Type> probtofm1(k);
  probtofm1(0) = pentvec(0);
  for(int i = 1; i < k; i++) {
    probtofm1(i) = probtofm1(i - 1) * (1 - pvec(i - 1)) * psurvivegap(i - 1) + pentvec(i);
  }
  // ## ** End of Robin's additions here **

  // ## First create the Binomial portion of the likelihood, and account for all animals never seen.
  // ## p.unseen = pent1 (1-p1) chi_1 + pent2 (1-p2) chi_2 + ... + pent.k (1-pk) chi_k
  vector<Type> pveccomp(k);
  for(int i = 0; i < k; i++) {
    pveccomp(i) = Type(1.0) - pvec(i);
  }

  Type logpunseen = log((pentvec * pveccomp * chivec).sum());
  // ## The N-nhist animals never seen give likelihood contribution proportional to
  // ## N! / (N-nhist)! p.unseen^(N-nhist)
  // ## so the analytic continuation of the negative log likelihood contribution is
  // ## -lgamma(N+1) + lgamma(N-nhist+1) - (N-nhist) log(p.unseen):
  Type nllike = -lgamma(N + Type(1.0)) + lgamma(N - nhist + Type(1.0)) - (N - nhist) * logpunseen;

  // ## Now for the animals that were seen.
  // ## ** Start of Robin's code replacing section commented out in original **
  // ## With the additions above we can now complete the negative log-likelihood calculations in one step.
  nllike = nllike -
    (firsttab * log(probtofm1)).sum() -
    (lasttab * log(chivec)).sum() -
    (caps * log(pvec)).sum() -
    (noncaps * log(pveccomp)).sum() -
    (survives * logpsurvivegap).sum();
  // ## ** End of Robin's code replacing section commented out in original **

  return nllike;
}
