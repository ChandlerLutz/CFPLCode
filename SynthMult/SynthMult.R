## c:/Dropbox/SyntheticControl/SynthMult/SynthMult.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2016-12-01

##The SynthMult Class -- for multiple Synthetic Control

##This program will hold the SynthMult Class which will
##allow for Synthetic Control Estimation using 0 to N dependent
##variables.

##Note: if the number of dependent variables
##is equal to  0, solution.v will either supplied by the user
##or given equal weights.

##Note: We assume that all dependent variables have the same
##avialable time index at the same frequency.

##load the R6 package
suppressMessages({require(R6); require(kernlab); require(optimx);
require(Synth);require(stringr);require(parallel);
require(data.table); require(magrittr);
require(optimx); require(quadprog); library(Matrix); library(matrixcalc);
require(grid); require(gridExtra); library(ggplot2);
require(LowRankQP);})

if (!require(MirrorDescSynth)) {
  if (.Platform$OS.type == "windows") {
    install.packages("MirrorDescSynth_0.0.0.9000.zip",
                     repos = NULL, type = "source")
  } else if (.Platform$OS.type == "unix") {
    install.packages("MirrorDescSynth_0.0.0.9000.tar.gz",
                     repos = NULL, type = "source")
  }
  require(MirrorDescSynth)
}

## -- Load functions -- ##

##Datarpep
source("SynthMult_functions/dataprep_setup.R")
source("SynthMult_functions/dataprep_setup_fast.R")
source("SynthMult_functions/assign_data.R")
source("SynthMult_functions/get_Y_Z.R")
source("SynthMult_functions/get_v_starting_values.R")
source("SynthMult_functions/expand_grid.R")
source("SynthMult_functions/data_check.R")

##Estimation
source("SynthMult_functions/synth_est.R")
source("SynthMult_functions/synth_optim.R")
source("SynthMult_functions/fn_V.R")
##Extra test functions from Synth package for potential testing
source("SynthMult_functions/synth_orig.R")

##Data cleaning
source("SynthMult_functions/region_weights_abbrv.R")
source("SynthMult_functions/synth_mult_out.R")
source("SynthMult_functions/mspe_pre.R")

##Prediction tables
source("SynthMult_functions/compute_pred_tables.R")

##Permutation Tests
##The (parallel) permutaiton test
source("SynthMult_functions/permutation_test.R")
##The public method to conduct the permutation test
source("SynthMult_functions/compute_perm.R")

##The plot functions
source("SynthMult_functions/plot_functions.R")

##The SythMult Class
SynthMult <- R6Class(
  classname = "SynthMult",
  public = list(
    ##variables Supplied by the user
    all.data = NULL,
    dependent.vars = NULL,
    time.policy = NULL,
    sample = NULL, ##For the name of file to be saved
    time.index = NULL,
    treated = NULL,
    controls = NULL, ##Either a vector or list. If a list, each element
                     ##of the list will be associated with a treated unit
    predictors = NULL,
    special.predictors = NULL,
    custom.v = NULL,            ##A custom V for Synth that can be supplied by
    ##the user. This is the weighted on each variable
    ##in predictors and special
    ##predictors. IF custom.v is not supplied, it will
    ##be chosen so that the pre-treatment outcome values
    ##are minimized every period.
    y.axis.label = NULL,
    sample.average.label = NULL,
    sample.average.df = NULL,  ##dataframe with the sample average
    ##first column should be the time index
    ##labeled "time.index"
    compute.pred.tables = TRUE, ##Logical -- prediction table will be computed
    compute.perm = TRUE,    ##Logical -- permuation test will be computed
    ##if TRUE
    percent.perm = NULL,   ##Numeric between 0 and 1 -- if set it will calculate
    ##the placebo tests for a random percentage of the
    ##controls
    MSPE.perm.max = NULL, ##The maximum MSPE to allow for iterations in the
    ##permutation test
    fast.compute = FALSE,  ##If set to TRUE, the dataprep function will not be
    ##used to compute
    optim.method = NULL,   ##Available options are "Nelder-Mead", "BFGS", "CG",
    ##"L-BFGS-B", "nlm", "nlminb", "spg", and "ucminf"
    parallel = FALSE,      ##If set to TRUE, use the parallel package to run
    ##some code in parallel
    parallel.cores = NULL,  ##The number of cores to be used in parallel processing
    ##If set to NULL, all available cores will be used
    plots = TRUE,           ##If set to FALSE -- plots will not be computed
    gap.ci = NULL, ##A decimal between 0 and 1 for the bootstrapped
    ##Confidence interval for the permutation test
    ## NOT IMPLEMENTED YET

    ## -- Output variables -- ##

    region.numbers = NULL, ##The ID numbers for each region
    X = NULL,             ##matrix of predictor data.
    ##nrows = number of predictors and (possibly)
    ##pre-treatment variables.
    ##ncol = number units (treated + control)
    X.scaled = NULL,      ##X matrix scaled so that each variable (row) is divided
    ##to have unit mean and zero variance
    Y.df = NULL, ##(data.frame) A data.frame to hold the original
    ##(not scaled) Outcome data with the following columns
    ##(1) time.index, (2) region.number, and then a
    ##column for each dependent variable
    Y = NULL, ##matrix of outcome data scaled to have zero mean and unit variance
    ##nrows = number of periods (pre-treatment + treatment).
    ##ncols = number of units (treated + controls
    Z = NULL, ##matrix of outcome data -- Y during the pre-treatment data
    v.starting.values = NULL, ##the starting values for V (the weights on the variables)
    grid.outcomes = NULL, ##(dataframe) A df of all outcomes
    ##covering self$dependent.vars and
    ##self$treated (a grid of outcomes)
    ## Add variables for the scaled gap, path, and permutation output
    ##Which will come from the Synth estimation on the scaled Z (outcome data)

    ## -- Estiamtion output variables -- ##

    solution.v = NULL,   ##solution to V -- the weights on all predictor variables
    MSPE = NULL, ## (list) The MSPE from (Z1 - Z0*W)'(Z1-Z0*W)/pre_tt at the optimal W
    region.weights = NULL, ##The region weights
    synth.output = NULL, ##A dataframe that holds dataframes with the Synth output
    ##for each treated unit including
    mspe.pre.output = NULL, ##dataframe with all of the MSPE pretreatment data
    region.weights.abbrv = NULL, ##Abbreviated control region weights
    pred.tables = list(),  ##pre-treatment predictors
    perm.region.weights = NULL, ##A list to hold the region weights for the
    ##Permutation test
    perm.output = NULL, ##A daframe that holds the permtuation output
    plots.all = NULL, ##A list of plots for all of the output

    ## -- Public Functions -- ##

    ##To dynamically add a function to the Class
    ##See http://stackoverflow.com/q/26331030/1317443
    add_function = function(name, meth) {
      self[[name]] <- meth
      environment(self[[name]]) <- environment(self$add_function)
    },
    save = function(folder = NULL) {
      file.name <- "output/"
      if (!is.null(folder)) {

        if (substr(folder, nchar(folder), nchar(folder)) != "/") {
          ##If the last character of the folder name is not
          ## a / --> add it
          folder <- paste0(folder, "/")
        }

        file.name <- paste0(file.name,folder)
      }
      print(file.name)
      ##Check to see if the folder exists; if not, create it
      ##see http://stackoverflow.com/a/29784923/1317443
      ifelse(!dir.exists(file.name), dir.create(file.name, recursive = TRUE), FALSE)
      file.name <- paste0(file.name,self$sample)

      dep.var.name <- paste(self$dependent.vars, collapse = "_")
      if (stringr::str_length(dep.var.name) > 25) {
        dep.var.name <- substr(dep.var.name, 1, 25)
      }

      file.name <- paste(file.name,
                         dep.var.name,
                         sep="_")
      file.name <- paste0(file.name,".rds")
      print(file.name)
      saveRDS(self,file=file.name)
    },

    ## -- The initializiation function -- ##

    initialize = function(all.data, dependent.vars = NULL, treated, controls,
                          time.policy, sample, time.index, predictors,
                          special.predictors = NULL, custom.v = NULL,
                          y.axis.label, sample.average.label, sample.average.var=NULL,
                          compute.perm=TRUE, percent.perm=NULL, MSPE.perm.max = NULL,
                          gap.ci = c(0.025, 0.975),
                          fast.compute = FALSE, optim.method=NULL,
                          parallel=FALSE, parallel.cores=NULL, plots=TRUE) {


      ##check for missing values and proper entry
      stopifnot(!missing(all.data), !missing(treated), !missing(controls),
                !missing(time.index), !missing(predictors), !missing(time.policy),
                !missing(sample)
                )


      ##Assign the data and run dataprep
      private$assign_data()

      ##Get self$Y (outcome data) and self$Z (outcome data before policy) if have
      ##dependent variables. if no dependent variables, make sure solution.v is set
      if (!is.null(self$dependent.vars)) {
        private$get_Y_Z()
      } else {
        ##No dependent variables, make sure custom.v is set. If it's not,
        ##set custom.v to equal weights. also set self$compute.perm to FALSE and
        ##set self$plots to FALSE
        if (is.null(self$custom.v)) {
          len.temp <- length(self$predictors) + length(self$special.predictors)
          self$custom.v = rep(1 / len.temp, len.temp)
          rm(len.temp)
        }
        ##Right now, private$compute_perm() and private$permutation_test()
        ##are not compatable with no dependent variables (need to update
        ##these functions), so set to FALSE for now
        self$compute.perm <- FALSE
        self$plots <- FALSE
      }

      ##Get the starting values for V -- the weights on each variable
      private$get_v_starting_values()

      ##Get the grid of outcomes -- A dataframe with the
      ##treated each treated unit and each dependent var
      private$expand_grid()

      ##Check to make sure the data is okay
      private$data_check()

      ##print a message saying finished with dataprep
      print("Dataprep finished. Starting Synth Estimation")

      ## -- Estimation -- ##

      ##the seeds
      private$seeds <- private$get_seeds(length(self$treated), 12345)

      ##Do the synthetic control estimation for each
      ##treated unit
      ##Do the synthetic control estimation for each
      ##treated unit
      if (self$parallel) {
        ##Run the  Synth estimation in parallel
        ##Note that for R6, we don't need to export the
        ##data or the objects to the nodes



        cl <- makeCluster(self$parallel.cores)
        clusterEvalQ(cl, library(magrittr)) ##load magrittr for each node
        clusterEvalQ(cl, library(Synth)) ##load Synth for each node
        clusterEvalQ(cl, library(data.table)) ##load data.table for each node
        clusterEvalQ(cl, library(optimx)) ##load optimx for each node
        clusterEvalQ(cl, library(kernlab)) ##load kernlab for each node
        clusterEvalQ(cl, library(LowRankQP)) ##load LowRankQP for each node
        temp <- clusterMap(cl, private$synth_est, self$treated, private$seeds,
                           .scheduling = "dynamic")
        stopCluster(cl) ##Stop the cluster
      } else {
        ##Synth est not in parallel
        temp <- Map(private$synth_est, self$treated, private$seeds)
      }

      print("Synth Finished -- Saving Output: ")

      ##Assign the final output data
      lapply(seq_along(temp), function(i) {
        ##Solution V
        self$solution.v[[i]] <- temp[[i]]$solution.v
        ##The MSPE
        self$MSPE[[i]] <- temp[[i]]$MSPE
        ##The region weights
        self$region.weights[[i]] <- temp[[i]]$region.weights

        return(invisible(NULL))
      })

      ##Get the abbreviated region weights
      self$region.weights.abbrv <- lapply(self$region.weights,
                                          private$region_weights_abbrv)

      ##Get the synth output using Y.df using self$synth_mult_output()
      self$synth.output <- private$synth_mult_out(self$treated, self$region.weights)

      ##Add the MSPE to self$synth.output
      private$mspe_pre()

      ##Remove temp and Force garbage collection
      rm(temp);gc()

      ##Get the prediction tables if requested by the user
      if (self$compute.pred.tables) self$compute_pred_tables()

      ##Run the permutation test if requested by the user
      if (self$compute.perm) {
        print("Running Permutation Test")
        self$compute_perm()
      }

      ##Create plots if requested by the user
      if (self$plots) self$create_plots()

      ##Save the object to an rds file
      self$save(self$sample)


    } ##End of initialize function

  ), ##End of public elements
  private = list(
    control.ids = list(), ##For controls ids that will be used in the Synth
    ##estimation as Synth sometimes fails
    seeds = NULL ##the seeds for random number generation
  ) ##end of private elements


) ## End of SynthMult Class

## -- Add more functions to the SynthMult Class -- ##

##Dataprep
SynthMult$set("private", "dataprep_setup_fast", dataprep_setup_fast);
rm(dataprep_setup_fast)
SynthMult$set("private", "dataprep_setup", dataprep_setup); rm(dataprep_setup)
SynthMult$set("private", "assign_data", assign_data); rm(assign_data)
SynthMult$set("private", "get_Y_Z", get_Y_Z); rm(get_Y_Z)
SynthMult$set("private", "get_v_starting_values", get_v_starting_values);
rm(get_v_starting_values)
SynthMult$set("private", "expand_grid", expand_grid); rm(expand_grid)
SynthMult$set("private", "data_check", data_check); rm(data_check)

##Estimation
SynthMult$set("private", "get_seeds", get_seeds); rm(get_seeds)
SynthMult$set("private", "synth_est", synth_est); rm(synth_est)
SynthMult$set("private", "synth_optim", synth_optim); rm(synth_optim)
SynthMult$set("private", "fn_V", fn_V); rm(fn_V)
SynthMult$set("private", "synth_orig", synth_orig); rm(synth_orig)
## SynthMult$set("private","fn_v_cpp", SynthHelpers::fn_v_cpp)
## SynthMult$set("private","solution_w_cpp", SynthHelpers::solution_w_cpp)

##Data cleaning
SynthMult$set("private","region_weights_abbrv", region_weights_abbrv);
rm(region_weights_abbrv)
SynthMult$set("private","synth_mult_out", synth_mult_out); rm(synth_mult_out)
SynthMult$set("private","mspe_pre", mspe_pre); rm(mspe_pre)
##Prediction Tables
SynthMult$set("public","compute_pred_tables", compute_pred_tables);
rm(compute_pred_tables)

##Permutation Tests
SynthMult$set("private","permutation_test", permutation_test); rm(permutation_test)
SynthMult$set("public","compute_perm", compute_perm); rm(compute_perm)


##Plots
SynthMult$set("private", "f.plot.check", f.plot.check); rm(f.plot.check)
SynthMult$set("private", "f.aes", f.aes); rm(f.aes)
SynthMult$set("private", "plot.gap.helper", plot.gap.helper); rm(plot.gap.helper)
SynthMult$set("public", "plot_treated_average", plot_treated_average); rm(plot_treated_average)
SynthMult$set("public", "plot_path", plot_path, overwrite = TRUE); rm(plot_path)
SynthMult$set("public", "plot_gap", plot_gap, overwrite = TRUE); rm(plot_gap)
SynthMult$set("public", "plot_permutation", plot_permutation, overwrite = TRUE);
rm(plot_permutation)
SynthMult$set("public", "plot_all", plot_all); rm(plot_all)
SynthMult$set("public", "create_plots", create_plots); rm(create_plots)
