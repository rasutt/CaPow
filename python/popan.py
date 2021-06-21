# Import packages
import numpy as np
import jax.numpy as jnp
from jax import grad, jit, vmap
from jax import random
import jax.ops
import jax.scipy.special as jss

# Function to find negative log likelihood for popan model
def popan_nll(
    pars, k, lambdamodel, gapvec, nhist, firsttab, lasttab, caps, noncaps,
    survives, constvalues, indsinsertintoallvalues, whichparsintoallvalues, 
    phiinds, pentinds, pinds, Nind, lambdaind, calcind
):
    # Unpack parameters

    # Getting the right parameter values is a little compicated because they can be
    # held constant or only specified in survey years rather than all years...

    # I can still probably tidy this up a lot later when everything's working ok

    # Replace any parameter slots with parameters-to-be-estimated in allvalues 
    # with their current values from pars
    allvalues = constvalues
    for i in range(len(indsinsertintoallvalues)):
        allvalues[int(indsinsertintoallvalues[i])] = pars[int(whichparsintoallvalues[i])]

    N = allvalues[int(Nind)]

    n_phi = len(phiinds)
    phivec = jnp.zeros(n_phi)
    for i in range(n_phi):
        phivec = phivec.at[i].set(allvalues[int(phiinds[i])])

    k = int(k)
    pvec = jnp.zeros(k)
    for i in range(k):
        pvec = pvec.at[i].set(allvalues[int(pinds[i])])

    lambdaval = 0
    # If it's a lambdamodel calculate entry probabilities from growth and survival 
    # rates
    if (lambdamodel == 1):
        phival = phivec[0]

        lambdaval = allvalues[int(lambdaind)]

        pentvec = jnp.zeros(k)
        pentvec = pentvec.at[0].set(1)

        cumlambda = 1

        for t in range(1, k):
            pentvec = pentvec.at[t].set((lambdaval**gapvec[t - 1] - phival**gapvec[t - 1]) * cumlambda)
            cumlambda = cumlambda * lambdaval**gapvec[t - 1]

        pentvec = pentvec / jnp.sum(pentvec)

    # For other models find final entry probability from those sepcified
    else:
        for i in range(k):
            pentvec = pentvec.at[i].set(allvalues[int(pentinds[i])])

        pentvec = pentvec.at[int(calcind)].set(1 - jnp.sum(pentvec))

    # Probability of not being seen after occasion t given alive at t
    chivec = jnp.zeros(k)
    chivec = chivec.at[-1].set(1)
    psurvivegap = jnp.zeros(k - 1)
    phiminindex = n_phi
    for i in range(k - 2, -1, -1):
        newphiminindex = phiminindex - gapvec[i]
        psurvivegap = psurvivegap.at[i].set(1)
        for j in range(newphiminindex, phiminindex):
            psurvivegap = psurvivegap * phivec[j]

        chivec = chivec.at[i].set(1 - psurvivegap[i] + psurvivegap[i] * (1 - pvec[ i -1]) * chivec[i + 1])
        phiminindex = newphiminindex

    exp_n_alive = jnp.zeros(k)
    exp_n_alive = exp_n_alive.at[0].set(N * pentvec[0])
    for i in range(1, k):
        exp_n_alive = exp_n_alive.at[i].set(exp_n_alive[i - 1] * psurvivegap[i - 1] + N * pentvec[i])

    probtofm1 = jnp.zeros(k)
    probtofm1 = probtofm1.at[0].set(pentvec[0])
    for i in range(1, k):
        probtofm1 = probtofm1.at[i].set(probtofm1[i - 1] * (1 - pvec[i - 1]) * psurvivegap[i - 1] + pentvec[i])

    pveccomp = 1 - pvec

    punseen = jnp.sum(pentvec * pveccomp * chivec)

    nllike = -jss.gammaln(N + 1) + jss.gammaln(N - nhist + 1) - (N - nhist) * jnp.log(punseen)

    nllike = nllike - jnp.sum(jnp.array(firsttab) * jnp.log(probtofm1)) - \
        jnp.sum(jnp.array(lasttab) * jnp.log(chivec)) - \
        jnp.sum(jnp.array(caps) * jnp.log(pvec)) - \
        jnp.sum(jnp.array(noncaps) * jnp.log(pveccomp)) - \
        jnp.sum(jnp.array(survives) * jnp.log(psurvivegap))

    return nllike

popan_grad = grad(popan_nll, argnums=(0))

def popan_nll_item(
    pars, k, lambdamodel, gapvec, nhist, firsttab, lasttab, caps, noncaps,
    survives, constvalues, indsinsertintoallvalues, whichparsintoallvalues, 
    phiinds, pentinds, pinds, Nind, lambdaind, calcind
):
    return popan_nll(
        pars, k, lambdamodel, gapvec, nhist, firsttab, lasttab, caps, noncaps,
        survives, constvalues, indsinsertintoallvalues, whichparsintoallvalues, 
        phiinds, pentinds, pinds, Nind, lambdaind, calcind
    ).item()

def popan_grad_ary(
    pars, k, lambdamodel, gapvec, nhist, firsttab, lasttab, caps, noncaps,
    survives, constvalues, indsinsertintoallvalues, whichparsintoallvalues, 
    phiinds, pentinds, pinds, Nind, lambdaind, calcind
):
    grad_jnp_list = popan_grad(
        pars, k, lambdamodel, gapvec, nhist, firsttab, lasttab, caps, noncaps,
        survives, constvalues, indsinsertintoallvalues, whichparsintoallvalues, 
        phiinds, pentinds, pinds, Nind, lambdaind, calcind
    )
    n_grads = len(grad_jnp_list)
    grad_list = [None] * n_grads
    for i in range(n_grads):
        grad_list[i] = grad_jnp_list[i].item()
        
    return grad_list