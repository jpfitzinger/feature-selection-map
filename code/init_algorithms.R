# ------------------------------------------------------------------------------
# Method initialization
#
# Initialize feature selection algorithms using tidyfit::m
# Unsupervised models (not implemented in tidyfit) are initialized with a
# placeholder
#
# Author: Johann Pfitzinger
# Date: 2023-05-11
# ------------------------------------------------------------------------------

init_algorithms <- function(nvars) {

  model_list <- list()

  # Filter methods
  model_list[["Correlation"]] <- m("cor")
  model_list[["RReliefF"]] <- m("relief")
  model_list[["InfoGain"]] <- m("relief", estimator = "InfGain")

  # Wrapper methods
  model_list[["MRMR"]] <- m("mrmr", feature_count = nvars)

  # Subset selection
  model_list[["FS (AIC)"]] <- m("subset", method = "forward", nvmax = nvars, IC = "AIC")
  model_list[["RFE (AIC)"]] <- m("subset", method = "backward", nvmax = nvars, IC = "AIC")
  model_list[["FS (LOOCV)"]] <- m("subset", method = "forward", IC = "LOOCV")
  model_list[["RFE (LOOCV)"]] <- m("subset", method = "backward", IC = "LOOCV")
  model_list[["SeqRep"]] <- m("subset", method = "seqrep", nvmax = nvars)

  # Penalized regressions
  model_list[["Lasso"]] <- m("lasso", dfmax = nvars - 1)
  model_list[["ElasticNet"]] <- m("enet", dfmax = nvars - 1, alpha = c(0.5, 0.75, 0.9))
  model_list[["AdaLasso"]] <- m("adalasso", dfmax = nvars - 1, lambda_ridge = 0.1)

  # Bayesian
  model_list[["BMA"]] <- m("bma", burn = 10000, iter = 100000, mprior.size = nvars, mcmc = "rev.jump")
  model_list[["Bayes. Lasso"]] <- m("blasso", T = 5000, M = nvars)
  model_list[["Spike & Slab"]] <- m("spikeslab", expected.model.size = nvars, niter = 5000)

  # Factor methods
  model_list[["PCR"]] <- m("pcr", ncomp = nvars, jackknife = T, validation = "LOO")
  model_list[["PLSR"]] <- m("plsr", ncomp = nvars, jackknife = T, validation = "LOO")

  # ML
  model_list[["RF"]] <- m("rf")
  model_list[["Grad. Boost"]] <- m("boost", mstop = 100)
  model_list[["GETS"]] <- m("gets", max.paths = 10, t.pval = 0.001, turbo = T)

  # Genetic algorithm
  model_list[["GA"]] <- m("genetic",
                          populationSize = 5000, numGenerations = 50,
                          minVariables = 3, maxVariables = nvars,
                          statistic = "BIC")

  # Unsupervised
  model_list[["LaPlacian"]] <- tibble()
  model_list[["FOS-MOD"]] <- tibble()

  map2(names(model_list), model_list, function(name, model) {
    tibble(name = name, model = list(model), nvars = nvars)
    })

}
