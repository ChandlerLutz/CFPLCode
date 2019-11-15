## c:/Dropbox/SyntheticControl/SynthMult/SynthMult_functions/compute_pred_tables_DT.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-01-15

compute_pred_tables <- function() {

  ##Small function to get the prediction table given the treated id
  ##and synthetic control weights
  f.pred.table <- function(treated.name, synth.weights) {
    treated.id.number <- self$region.numbers %>%
      .[region == c(treated.name), as.character(region.number)]

    ##get the  control id numbers
    control.id.numbers <- self$region.numbers %>%
      .[region %in% c(synth.weights$region), as.character(region.number)]

    ##the x for the control variables
    X.controls <- self$X[, control.id.numbers, drop = FALSE]

    ##Get the sample average
    sample.average <- self$X[, control.id.numbers, drop = FALSE] %*%
      rep(1/length(control.id.numbers),length(control.id.numbers))

    pred.table.temp <- cbind(self$X[, treated.id.number, drop=FALSE],
                             X.controls %*% synth.weights$w.weights,
                             sample.average
                             )
    colnames(pred.table.temp) <- c(treated.name, "Synthetic", "Sample Mean")
    return(pred.table.temp)
  }

  ##Compute the prediction tables
  self$pred.tables <- Map(f.pred.table, treated = self$treated,
                          synth.weights = self$region.weights)

  return(invisible(NULL))
}

