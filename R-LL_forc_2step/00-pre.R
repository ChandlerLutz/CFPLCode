## c:/Dropbox/CFPLSML/R-LL_forc_2step/00-formulas.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-12-01

## for the formulas

## call after all data are defined

f_get_dummies_trends <- function(DT) {

  ##The dummy variables
  time.dummies <<- names(DT) %>% .[grepl("^period_", x = .)]
  zip3.dummies <<- names(DT) %>% .[grepl("^zip3_[0-9]{3}$", x = .)]
  CA.time <<- names(DT) %>% .[grepl("^CA_period_", x = .)]
  forc.high.CA.time <<- names(DT) %>% .[grepl("forc.high_CA_period_", x = .)]
  forc.high.time <<- names(DT) %>% .[grepl("forc.high_period_", x = .)]
  ##lu dummies
  lu.dummies <<- paste0("lu_", time.dummies)
  ##trends
  zip3.trends <<- names(DT) %>% .[grepl("zip3_[0-9]{3}.trend$", x = .)]

  return(invisible(NULL))
}

