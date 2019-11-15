## c:/Dropbox/SyntheticControl/SynthMult/SynthMult_functions/expand_grid_DT.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-01-15

##A function to create a grid of outcomes
expand_grid <- function() {

  if (is.null(self$dependent.vars)) {
    ##When the user has set no dependent variables
    self$grid.outcomes <- data.table(treated = self$treated,
                                     dependent.vars = NA_character_)
  } else {


    self$grid.outcomes <- expand.grid(self$treated, self$dependent.vars,
                                      stringsAsFactors = FALSE) %>%
      setNames(., c("treated", "dependent.var")) %>%
      setDT %>%
      .[, id := paste(treated, dependent.var, sep = "_")] %>%
      setcolorder(c("id", "treated", "dependent.var"))
  }


  return(invisible(NULL))
}



