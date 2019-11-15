## c:/Dropbox/SyntheticControl/SynthClassMult/SynthClassMult_functions/permutation_test.r

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2016-09-29

##A function to conduct the permutation test
permutation_test <- function() {
  ## perm.regions.out <- permutation_test()
  ##
  ## This function will conduct a permutation test
  ## and return region weights for each permutation
  ## test that was conducted
  ##
  ## Note: as in Abadie et al. (2010), we'll disregard control units
  ## with a MSPE more than five times higher than that
  ## for treatment unit during the pretreatment period.
  ## This will be a private function in the SynthClass Class

  set.seed(12345)

  ##If the MSPE.perm.max perm is not set, use 5 times the maximum
  ##as in the Abadie et al code
  if (is.null(self$MSPE.perm.max))
    self$MSPE.perm.max <- 5 * do.call("max",self$MSPE)

  max.MSPE <- do.call("max",self$MSPE)

  ##reassign the control units
  controls <- unique(unlist(self$controls))

  ##if self$compute.perm is not NULL only calculate the permuation test
  ##for only a random percentage of controls
  if (!is.null(self$percent.perm)) {
    treated.placebos <- sample(controls,round(length(controls)*self$percent.perm))
  } else {
    treated.placebos <- controls
  }
  ##Order
  treated.placebos <- treated.placebos[order(treated.placebos)]

  seeds <- private$get_seeds(length(treated.placebos), 12345)

  ##The function that we'll use to get the output
  f.placebo.test <- function(treat.placebo, seed) {
    temp <- private$synth_est(treat.placebo, seed)
    if ((temp$MSPE) > 5 * max.MSPE)
      return(NULL)
    return(temp$region.weights)
  }
  if (self$parallel) {

    ##Initiate the clusters
    cl <- makeCluster(self$parallel.cores)
    ##export data to each node
    clusterExport(cl,  varlist = c("max.MSPE", "f.placebo.test"),
                  envir = environment())
    clusterEvalQ(cl, library(magrittr)) ##load magrittr for each node
    clusterEvalQ(cl, library(Synth)) ##load Synth for each node
    clusterEvalQ(cl, library(data.table)) ##load data.table for each node
    clusterEvalQ(cl, library(optimx)) ##load optimx for each node
    clusterEvalQ(cl, library(kernlab)) ##load kernlab for each node
    clusterEvalQ(cl, library(LowRankQP)) ##load LowRankQP for each node
    placebo.test.out <- clusterMap(cl, f.placebo.test, treated.placebos, seeds,
                                   .scheduling = "dynamic")
    ##placebo.test.out <- parLapply(cl, treated.placebos, f.placebo.test)
    stopCluster(cl) ##Stop the cluster
  } else {
    ##Do not use parallel
    placebo.test.out <- Map(treated.placebos, f.placebo.test, seeds)
  }

  names(placebo.test.out) <- treated.placebos

  ##remove controls that are NULL b/c the MSPE was too high
  placebo.test.out <- placebo.test.out[!vapply(placebo.test.out, is.null, logical(1))]

  return(placebo.test.out)

}
