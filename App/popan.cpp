// POPAN negative log-likelihood function for TMB. Rewritten from capow-fast.R,
// capow with Robin's modifications for speed, with original comments and
// structure where possible.

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
  // index of lambda for lambda model, zero otherwise
  DATA_INTEGER(lambdaind); 
  // index of one calc pent value for pent model, zero otherwise
  DATA_INTEGER(calcind); 
  
  PARAMETER_VECTOR(pars);
  
  // The parameters to be optimized come in as pars.  The others come in as
  // constvalues, which has zeros where values from pars should go.  
  
  // Make vector for all parameter values, and fill in those held constant.
  vector<Type> allvalues = constvalues;
  
  // Fill in the values to be estimated.  We need their indices in pars because
  // the same parameters may be used multiple times when they're constrained to
  // be equal in the model.
  for(int i = 0; i < indsinsertintoallvalues.size(); i++) {
    allvalues(indsinsertintoallvalues(i)) = pars(whichparsintoallvalues(i));
  }

  // Pull out population size and capture probabilities
  Type N = allvalues(Nind);
  vector<Type> pvec(k);
  for(int i = 0; i < k; i++) {
    pvec(i) = allvalues(pinds(i));
  }

  // Find the number of phi parameters.  There are only k - 1 possible distinct
  // parameters in the model, but the values are repeated over years between
  // surveys.
  int philen = phiinds.size();
  
  // Pull out survival probabilities
  vector<Type> phivec(philen);
  for(int i = 0; i < philen; i++) {
    phivec(i) = allvalues(phiinds(i));
  }
  
  // Create variables for entry proportions and population growth rate
  vector<Type> pentvec(k);
  Type lambda = 0;
  
  // If it's a lambda-model, we need to calculate all the pent values as a
  // function of lambda and phi.
  if(lambdamodel == 1) {
    // Get survival probability, it's constrained to be equal over all surveys
    // for lambda-models
    Type phival = phivec(0);

    // Get population growth rate
    lambda = allvalues(lambdaind);

    // Find entry proportions for each survey. k > 1 is required for all models.
    // The proportion entering is the proportion of population growth minus the
    // proportion surviving, weighted by the relative sizes of the population
    // over time.
    pentvec.setZero();
    pentvec(0) = 1;
    Type cumlambda = 1;
    
    for(int t = 1; t < k; t++) {
      pentvec(t) = (pow(lambda, gapvec(t - 1)) - pow(phival, gapvec(t - 1))) * 
        cumlambda;
      cumlambda *= pow(lambda, gapvec(t - 1));
    }

    Type sumpentvec = pentvec.sum();
    for(int i = 0; i < k; i++) {
      pentvec(i) = pentvec(i) / sumpentvec;
    }
    
  // For non-lambda models, we need to calculate one of the pent values as 1 -
  // sum(other pents).
  } else {
    for(int i = 0; i < k; i++) {
      pentvec(i) = allvalues(pentinds(i));
    }
    pentvec(calcind) = 1 - pentvec.sum();
  }

  // ## --------------------------------------------------------------------
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
  int phiminindex = philen;
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
