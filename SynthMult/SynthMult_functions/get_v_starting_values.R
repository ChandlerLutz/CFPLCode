## c:/Dropbox/SyntheticControl/SynthClass/SynthClass_functions/get_v_starting_values.r

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2016-11-29

get_v_starting_values <- function() {

    ##Get the starting values
    ##try regression, if that doesn't work, use
    ##equal weights for the starting values
    X.all <- cbind(rep(1, ncol(self$X.scaled)), t(self$X.scaled))
    Beta <- try(solve(crossprod(X.all)) %*%
                (t(X.all) %*% t(self$Z)), silent = TRUE)

    if (inherits(Beta, "try-error")) {
        ##use equal weights
        nvarsV <- length(self$special.predictors) + length(self$predictors)
        self$v.starting.values <- rep(1 / nvarsV, nvarsV)
    } else {
        Beta <- Beta[-1, ]
        V <- Beta %*% t(Beta)
        self$v.starting.values <- diag(V)
        self$v.starting.values <- self$v.starting.values / sum(self$v.starting.values)
        self$v.starting.values <- as.numeric(self$v.starting.values)
    }

    rm(X.all); rm(Beta);

    return(invisible(NULL))

}
