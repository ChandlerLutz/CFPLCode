## c:/Dropbox/SyntheticControl/SynthMult/SynthMult_functions/assign_data_DT.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-01-12

assign_data <- function(env = parent.frame()) {
  ##Note: We will use set the environment to parent.frame() so that
  ##env will the environment that the function was called from

  ##for region, region.number, and time.index in all.data
  if (!("region" %in% names(env$all.data))) {
    stop("'region' column is not defined in all.data")
  } else if (!("region.number" %in% names(env$all.data))) {
    stop("'region.number' column is not defined in all.data")
  } else if (!("time.index" %in% names(env$all.data))) {
    stop("'time.index' column is not defined in all.data")
  }

  ##Assign the user-specified variables and remove as necessary
  self$all.data <- env$all.data;
  self$all.data <- self$all.data %>% as.data.table
  ##keep only alphanumeric characters in the region names
  self$all.data[, region := str_replace_all(region, "[^[:alnum:]]", "")]
  if (!is.null(env$dependent.vars)) self$dependent.vars <- env$dependent.vars
  self$treated <- str_replace_all(env$treated, "[^[:alnum:]]", "")
  ##Assign the controls
  if (is.list(env$controls)) {
    if (length(env$controls) != length(self$treated)) {
      stop("Error: If 'controls' is a list, the length(controls) needs to be equal to length(treated)")
    }

    self$controls <- lapply(env$controls, str_replace_all, "[^[:alnum:]]", "") %>%
      setNames(self$treated)
  } else if (is.vector(env$controls)) {
    self$controls <- str_replace_all(env$controls, "[^[:alnum:]]", "")
  } else {
    stop("Error: 'controls' needs to be a character vector vector or a list of character vectors")
  }


  self$time.policy <- env$time.policy
  self$sample <- env$sample
  self$time.index <- env$time.index
  self$predictors <- env$predictors
  self$special.predictors <- env$special.predictors
  self$custom.v <- env$custom.v

  ##Get the y axis label. If null use the dependent var label
  if (!is.null(env$y.axis.label)) {
    self$y.axis.label <- env$y.axis.label
  } else {
    self$y.axis.label <- self$dependent.vars
  }
  ##set the names to match the dependent variable
  self$y.axis.label <- setNames(self$y.axis.label, self$dependent.vars)
  ##The sample average label
  self$sample.average.label <-
    ifelse(is.null(env$sample.average.label),"Sample Average",env$sample.average.label)
  self$sample.average.df <- env$sample.average.df
  ##If self$sample.average.df is NULL, compute it form the average
  if (is.null(self$sample.average.df)) {
    self$sample.average.df <- self$all.data %>%
      .[, mget(c("time.index", self$dependent.vars))] %>%
      .[, lapply(.SD, mean, na.rm = TRUE), keyby = time.index] %>%
      .[complete.cases(.)]
  } else {
    ##If the sample average is provided, make sure it's a DT
    self$sample.average.df %>% setDT(.)
    if (!("time.index" %in% names(self$sample.average.df))) {
      stop("Error: sample.average.df needs to have a numeric column named time.index")
    }
  }

  self$compute.perm <- env$compute.perm
  self$percent.perm <- env$percent.perm
  self$MSPE.perm.max <- env$MSPE.perm.max
  self$gap.ci <- env$gap.ci
  self$fast.compute <- env$fast.compute
  self$optim.method <- env$optim.method
  self$parallel <- env$parallel
  ##The number of parallel cores. If it's missing, use the
  ##maximum number of cores. If the number of specified cores is
  ##greater than the number of the available cores, use the
  ##maximum number of available of cores
  self$parallel.cores <- ifelse(!is.null(env$parallel.cores),
                                env$parallel.cores,
                                detectCores())
  self$parallel.cores <- ifelse(self$parallel.cores > detectCores(),
                                detectCores(),
                                self$parallel.cores)
  self$plots <- env$plots

  print(self$optim.method)
  ## ---- Get the dataprep objects ---- ##
  first <- function(x) x[1]
  ##Get the regions and theirs region ID numbers
  self$region.numbers <- self$all.data %>%
    .[region %in% c(self$treated, unique(unlist(self$controls))),
      .(region = first(region)),
      keyby = region.number] %>%
    setcolorder(c("region", "region.number"))

  ##checks for custom v
  if (!is.null(self$custom.v)) {

    ##Checks for custom v
    if (!is.list(self$custom.v) & (length(self$custom.v) != (length(self$predictors) + length(self$special.predictors)))) {
      ##custom.v is not a list. In this case custom.v
      ##must be a vector with length equal to
      ##predictors + special.predictors
      stop("Error: length of custom.v not equal to length of predictors + special.predictors")
    } else if (is.list(self$custom.v)) {
      ##If custom.v is a list, it must have length equal to
      ##the number of treated units. The names of the list for custom.v
      ##must be equal to the number of regions
      custom.v.names <- names(self$custom.v) %>% sort
      if (!identical(custom.v.names, sort(self$treated))) {
        stop("Error: if custom.v is a list, each element must correspond to a region where the names of the list are the region names")
      }

    }

    ##if custom.v is a vector, repeat it for each region and give each element
    ##a name corresponding to the region name
    if (!is.list(self$custom.v)) {
      self$custom.v <- rep(list(self$custom.v), length(self$treated)) %>%
        setNames(self$treated)
    }
  } ## end of !is.null(self$custom.v) if

  ##get the average pre-treatment
  ##predictor values
  if (self$fast.compute) {
    private$dataprep_setup_fast()
  } else {
    ##Use the slower dataprep object
    temp.dataprep <-private$dataprep_setup(
      self$region.numbers[self$region.numbers$region == self$treated[1],2],
      self$region.numbers[self$region.numbers$region != self$treated[1],2]
    )

    self$X <- cbind(temp.dataprep$X1,temp.dataprep$X0)

    self$X <- self$X[,order(as.numeric(colnames(self$X)))]
    ##Now remove temp.dataprep
    rm(temp.dataprep)
  }

  ##Force garbage collection
  gc()

  ##Get the scaled version of X -- Subtract of means and
  ##normalize by larges value
  ##Get the scaled version of X -- divide each variable by its standard deviation
  self$X.scaled <- apply(self$X, 1, function(x) (x - mean(x))/sd(x)) %>% t(.)
  X.norm <- max(abs(self$X.scaled))
  self$X.scaled <- self$X.scaled / X.norm


  ##Initiate empty lists for the synth output using the number of independent
  ##variables. use names according the treated units
  self$solution.v <- vector(mode = "list", length = length(self$treated))
  names(self$solution.v) <- self$treated
  self$MSPE <- self$solution.v
  self$region.weights <- self$solution.v


  ##For control ids that need to be removed in Synth Estimation
  ##Start off not removing any controls
  all.controls <- self$region.numbers[region %in% unique(unlist(self$controls)),
                                      as.character(region.number)]

  ##Now add elements to the list for things that will be removed
  set.seed(12345)
  num.controls <- length(all.controls)
  private$control.ids <- c(
    list(all.controls),
    ##95 percent of all controls
    lapply(1:3, function(i)
      sample(all.controls, size = ceiling(num.controls * 0.95))
      ),
    ##90 percent of all controls
    lapply(1:3, function(i)
      sample(all.controls, size = ceiling(num.controls * 0.9))
      ),
    ##80 percent of all controls
    lapply(1:3, function(i)
      sample(all.controls, size = ceiling(num.controls * 0.8))
      ),
    ##70 percent of all controls
    lapply(1:3, function(i)
      sample(all.controls, size = ceiling(num.controls * 0.7))
      ),
    ##60 percent of all controls
    lapply(1:3, function(i)
      sample(all.controls, size = ceiling(num.controls * 0.6))
      ),
    ##50 percent of all controls
    lapply(1:3, function(i)
      sample(all.controls, size = ceiling(num.controls * 0.5))
      )
  )

  ##Remove the extra variables from the environment env
  rm(all.data, dependent.vars, time.policy, sample, time.index, treated,
     controls, predictors, special.predictors, custom.v,
     y.axis.label, sample.average.label, sample.average.var,
     compute.perm, percent.perm, MSPE.perm.max, gap.ci, fast.compute,
     optim.method, parallel, parallel.cores, plots,
     envir = env)

  ##Run garbage collection
  gc()

  return(NULL)
}
