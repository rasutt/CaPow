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

  // Super-population size, capture probabilities and their complements, and
  // entry proportions (filled in later)
  Type N = allvalues(Nind);
  vector<Type> pvec(k), pcompvec(k), pentvec(k);
  for(int i = 0; i < k; i++) {
    pvec(i) = allvalues(pinds(i));
    pcompvec(i) = Type(1.0) - pvec(i);
  }

  // If it's a lambda-model, we need to calculate all the pent values as a
  // function of lambda and phi.
  if(lambdamodel == 1) {
    // Get survival probability, it's constrained to be equal over all surveys
    // for lambda-models
    Type phival = allvalues(phiinds(0));

    // Get population growth rate
    Type lambda = allvalues(lambdaind);

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

  // Find the chi parameters, P(never seen after occasion i | alive at i), from
  // recurrence relation starting with one for i = k, and P(survive from survey
  // i to survey i + 1 | alive at survey i).  The phi values are repeated over
  // years between surveys.
  vector<Type> chivec(k);
  chivec(k - 1) = 1;
  vector<Type> psurvivegap(k - 1);
  int phiminindex = gapvec.sum();
  for(int i = k - 2; i >= 0; i--) {
    phiminindex = phiminindex - gapvec(i);
    psurvivegap(i) = pow(allvalues(phiinds(phiminindex)), gapvec(i));
    chivec(i) = Type(1.0) - psurvivegap(i) + psurvivegap(i) * 
      (Type(1.0) - pvec(i + 1)) * chivec(i + 1);
  }

  // Find the psi parameters, P(never seen before occasion i, and alive at i)
  // and E(N_t), from recurrence relations starting with entry proportion for i
  // = 1, and superpopulation size.
  vector<Type> psivec(k);
  psivec(0) = pentvec(0);
  vector<Type> exp_n_alive(k);
  exp_n_alive(0) = N * pentvec(0);
  for(int i = 1; i < k; i++) {
    psivec(i) = psivec(i - 1) * (1 - pvec(i - 1)) * psurvivegap(i - 1) + 
      pentvec(i);
    exp_n_alive(i) = exp_n_alive(i - 1) * psurvivegap(i - 1) + N * pentvec(i);
  }

  // Specify E(N_t) as a derived parameter for TMB to calculate and report the
  // standard error
  ADREPORT(exp_n_alive);
  
  // Negative log likelihood, composed of analytic continuation of number of
  // ways of dividing superpopulation into never captured, and captured at least
  // once, and capture history probabilities in each case.
  Type nllike = -lgamma(N + Type(1.0)) + lgamma(N - nhist + Type(1.0)) - 
    (N - nhist) * log((pentvec * pcompvec * chivec).sum()) -
    (firsttab * log(psivec)).sum() -
    (lasttab * log(chivec)).sum() -
    (caps * log(pvec)).sum() -
    (noncaps * log(pcompvec)).sum() -
    (survives * log(psurvivegap)).sum();
  
  return nllike;
}
