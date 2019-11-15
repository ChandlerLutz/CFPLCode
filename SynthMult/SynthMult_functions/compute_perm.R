## c:/Dropbox/SyntheticControl/SynthMult/SynthMult_functions/compute_perm_DT.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-01-15

##Public method to comute the permuation test

compute_perm <- function() {

  ##The permutation test
  self$perm.region.weights <- private$permutation_test()

  ##Get the permuation output
  self$perm.output <- private$synth_mult_out(names(self$perm.region.weights),
                                             self$perm.region.weights)

  ##Also add the bootstrapped confidence
  ##First create a small function to get the bootstrapped confidence intervals
  f.boot <- function(x) {
    x.sample <- sample(x, size = 1000, replace = TRUE)
    out <- quantile(x.sample, probs = self$gap.ci)
    out.df <- data.table(gap.ci.lower = out[1], gap.ci.upper = out[2])
    return(out.df)
  }


  ##Use the dplyr do() function to return multiple outputs
  perm.boot <- self$perm.output %>%
    .[, f.boot(gap), by = .(variable, time.index)]

  ##Add the bootstrapped confidence intervals to the synth.output.data
  self$synth.output <- self$synth.output %>%
    merge(., perm.boot, by = c("variable", "time.index"), all.x = TRUE)

  ##Finally, if this function is called,
  ##set compute.perm to TRUE
  self$compute.perm <- TRUE

  return(NULL)
}

