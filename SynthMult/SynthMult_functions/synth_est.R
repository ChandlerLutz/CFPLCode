## c:/Dropbox/SyntheticControl/SynthMult/SynthMult_functions/synth_est_DT.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-01-15


##a get seeds function from
##https://stackoverflow.com/a/21568878/1317443
get_seeds <- function(ntasks, iseed) {
  RNGkind("L'Ecuyer-CMRG")
  set.seed(iseed)
  seeds <- vector("list", ntasks)
  seeds[[1]] <- .Random.seed
  for (i in seq_len(ntasks - 1)) {
    seeds[[i + 1]] <- nextRNGSubStream(seeds[[i]])
  }
  seeds
}

synth_est <- function(treated, seed) {
  ## list <- synth_est(treated, seed)
  ##
  ## To conduct the synthetic control estimation
  ## given treated and control units for a SynthClass
  ## R6 object
  ## This will be a private function in the SynthClass class
  ##
  ## User-specified inputs:
  ##   treated -- string with the treated units
  ##   seed -- for the seed
  ##
  ## User-specified output:
  ##   list$dataprep.out -- the final dataprep object used in the
  ##                        synthetic control estimation
  ##   list$synth.out -- the final synthetic control object

  ##assign(".Random.seed", seed, envir = .GlobalEnv)
  assign(".Random.seed", seed)

  region.number <- NULL; region <- NULL ##Initialize for a package
  treated.id.number <- self$region.numbers %>%
    .[region == c(treated), as.character(region.number)]
  if (is.list(self$controls)) {
    if (treated %in% self$treated) {
      ##treated unit
      temp.controls <- self$controls[[treated]]
    } else {
      ##permutation test -- use all controls
      temp.controls <- unique(unlist(self$controls))
    }
  } else {
    temp.controls <- self$controls
  }
  control.id.numbers <- self$region.numbers %>%
    .[region %in% (temp.controls) & region != (treated),
      as.character(unique(region.number))]

  custom.v <- NULL; ##Set custom.v to NULL
  if (!is.null(self$custom.v)) {
    ##custom v set by the user, get
    custom.v <- self$custom.v[[treated]]
  }

  ##Get X1.scaled and Z1 for the treated unit
  X1.scaled <- self$X.scaled[, treated.id.number, drop = FALSE]
  if (!is.null(self$Z)) {
    ##When the user has set some dependent variable via
    ##self$dependent.vars
    Z1 <- self$Z[, treated.id.number, drop = FALSE]
  } else {
    ##For no dependent variables
    Z1 <- NULL
  }

  ##The number of variables over which we're going to optimize--find V
  nvarsV <- length(self$predictors)+length(self$special.predictors)

  convergence <- FALSE
  ##Note that in some instances the Synth package
  ##fails for a given control sample
  ##So, define a set (random) of controls to be removed
  ##that contains (a) 0 percent (b) 10 percent
  ##and (c) 20 percent, and (d) 30 percent of the sample
  ##The control lists are stored in private$control.ids

  ##Make sure that we are using the right controls
  temp.control.ids <- lapply(private$control.ids, function(x) x[x %in% control.id.numbers])

  j <- 1
  while (!convergence & j <= length(temp.control.ids)) {

    ##Get X0 and Z0
    X0.scaled <- self$X.scaled[, temp.control.ids[[j]], drop = FALSE]
    if (!is.null(self$Z)) {
      ##When the user has set self$dependent.vars
      Z0 <- self$Z[, temp.control.ids[[j]], drop = FALSE]
    } else {
      ##For no dependent variables
      Z0 <- NULL
    }

    ##The try-catch
    ##supress error messages
    ##options(show.error.messages = FALSE)



    result <- tryCatch(
      ##If self$optim.method is set,
      ##use the specified optimization method
      if (self$fast.compute) {
        ##If fast.compute is set to TRUE
        ##Use only the given for optimization
        ##Note, we are going to the "drop=FALSE"
        ##argument to maintain matrix form
        if (is.null(self$optim.method))
          stop("an optim method needs to be specified")
          private$synth_optim(X0.scaled = X0.scaled,
                              X1.scaled = X1.scaled,
                              Z0 = Z0,
                              Z1 = Z1,
                              nvarsV = nvarsV,
                              custom.v = custom.v,
                              optimxmethod = self$optim.method,
                              seed = seed)
      } else {
        ##Use the default settings Synth (Nelder-Mead and
        ##BFGS)
        synth(X1=self$X[,treated.id.number, drop=FALSE],
              X0=self$X[,temp.control.ids[[i]], drop = FALSE],
              Z1=self$Z[,treated.id.number,drop=FALSE],
              Z0=self$Z[,temp.control.ids[[i]], drop = FALSE],
              verbose = FALSE
              )

        ## private$synth_package(
        ##     X1=self$X[,treated.id.number, drop=FALSE],
        ##     X0=self$X[,temp.control.ids[[i]], drop = FALSE],
        ##     Z1=self$Z[,treated.id.number,drop=FALSE],
        ##     Z0=self$Z[,temp.control.ids[[i]], drop = FALSE],
        ##     verbose = TRUE
        ## )

      },
      error=function(e)  { print("error found"); print(treated);
        print(temp.control.ids[[j]]); print(e); e }

    )

    if (!inherits(result,"error")) {

      convergence <- TRUE
      ##Turn error messages back on
      options(show.error.messages = TRUE)

      ##The weights on each variable
      solution.v <- as.numeric(result$solution.v)

      ## loss.w <- t(X1.scaled - X0.scaled %*% solution.w) %*% V %*%
      ##   (X1.scaled - X0.scaled %*% solution.w)
      ##loss.v -- the loss -- MSPE
      ##loss.v <- t(Z1 - Z0 %*% as.matrix(solution.w)) %*% (Z1 - Z0 %*% as.matrix(solution.w))
      ## loss.v <- t(self$Z[,treated.id.number,drop=FALSE] - self$Z[,temp.control.ids] %*%
      ##               as.matrix(result$solution.w)) %*%
      ##   (self$Z[,treated.id.number,drop=FALSE] - self$Z[,temp.control.ids] %*% as.matrix(solution.w))
      ##Use crossprod() to speed things up
      if (!is.null(self$Z)) {
        ##If the user has specified dependent.vars and self$V is not NULL
        loss.v <- crossprod(Z1 - (Z0 %*% matrix(result$solution.w)))
        loss.v <- loss.v/length(self$time.index[self$time.index < self$time.policy])
        ##The MSPE from the Synth est
        MSPE <- as.numeric(loss.v)
      } else {
        ##If there are no dependent vars
        MSPE <- NA
      }

      ##The Synth Tables
      synth.weights <- as.numeric(result$solution.w)

      rm(result)
      ##force garbage collection
      ##gc()

      ##the region names
      temp.region.names <- self$region.numbers %>%
        .[region.number %in% temp.control.ids[[j]], region]

      ##Get the Regional weights
      region.weights <- data.frame(region = temp.region.names,
                                   w.weights = synth.weights,
                                   stringsAsFactors = FALSE)

      rm(.Random.seed)

      return(list(solution.v=solution.v,
                  MSPE=MSPE,
                  region.weights=region.weights
                  ))

    }
    j <- j+1
    ##Run garbage collection on every fourth iteration
    if (j %% 4 == 0)
      gc()
  }

  ##Turn error messages back on
  options(show.error.messages = TRUE)
  ##No convergence
  return(list(solution.v=NA,
              MSPE=NA,
              region.weights=NA
              ))
}





