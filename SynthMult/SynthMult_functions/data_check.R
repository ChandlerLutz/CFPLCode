## c:/Dropbox/SyntheticControl/SynthClassMult/SynthClassMult_functions/data_check.r

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2016-09-28

data_check <- function() {
    ##check to make sure X, Y, and Z don't have NAs or infinite values

    ## -- Check X -- ##
    if (any(is.na(self$X))) {
        print(self$X); stop("!is.na(self$X) is not TRUE")
    }
    if (any(is.nan(self$X))) {
        print(self$X); stop("!is.nan(self$X) is not TRUE")
    }
    if (any(!is.finite(self$X))) {
        print(self$X); stop("is.finite(self$X) is not TRUE")
    }

    ## -- Check Y -- ##
    if (!is.null(self$Y)) {
        if (any(is.na(self$Y))) {
            print(self$Y); stop("!is.na(self$Y) is not TRUE")
        }
        if (any(is.nan(self$Y))) {
            print(self$Y); stop("!is.nan(self$Y) is not TRUE")
        }
        if (any(!is.finite(self$Y))) {
            print(self$Y); stop("is.finite(self$Y) is not TRUE")
        }

    }

    ## -- Check Z -- ##
    if (!is.null(self$Z)) {
        if (any(is.na(self$Z))) {
            print(self$Z); stop("!is.na(self$Z) is not TRUE")
        }
        if (any(is.nan(self$Z))) {
            print(self$Z); stop("!is.nan(self$Z) is not TRUE")
        }
        if (any(!is.finite(self$Z))) {
            print(self$Z); stop("is.finite(self$Z) is not TRUE")
        }

    }

    return(invisible(NULL))
}

