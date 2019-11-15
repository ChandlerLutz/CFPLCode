## c:/Dropbox/SyntheticControl/SynthMult/SynthMult_functions/mse_pre_DT.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-01-15

##To add the MSPE to the self$synth.output df


mspe_pre <- function() {

  if (is.null(self$synth.output)) {
    ##Only get the mspe if we have dependent variables
    ##and mspe_pre is not NULL
    return(invisible(NULL))
  }

  self$mspe.pre.output <- self$synth.output %>%
    .[time.index < self$time.policy] %>%
    .[, .(mspe.pre = sum(gap ^ 2) /
            length(self$time.index[self$time.index < self$time.policy])),
      by = .(region, variable)] %>%
    .[, .(region, variable, mspe.pre)]

  return(invisible(NULL))

}



