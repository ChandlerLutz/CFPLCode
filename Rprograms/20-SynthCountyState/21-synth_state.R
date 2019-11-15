##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-02-20

##Clear the workspace
##Delete all objects and detach packages
rm(list = ls())
suppressWarnings(CLmisc::detach_all_packages())



suppressMessages({library(CLmisc)})

source("../../SynthMult/SynthMult.R", chdir = TRUE)

vars.state.df <- readRDS("RdsFiles/30-state_vars_df.rds")

state.month <- readRDS("../../Data/_Data_Final_/05-state_month.rds")
state.quarter <- readRDS("../../Data/_Data_Final_/05-state_quarter.rds")

f_synth <- function(.var) {

  print(.var)

  if (.var %in% names(state.month)) {
    DT <- copy(state.month)
    frequency <- "month"
  } else {
    DT <- copy(state.quarter)
    frequency <- "quarter"
  }

  ##Remove NAs
  DT <- DT[!is.na(get(.var))] %>%
    .[, N := sum(!is.na(.SD)), by = region,
      .SDcols = names(.) == .var] %>%
    .[N == max(N)] %>%
    .[, N := NULL]

  if (grepl("ret", .var)) {
    ##If var is a return, use it for var.pre
    var.pre <- .var
    ##If var is a return, use only returns for prediction
    predictors <- .var
  }

  if (frequency == "month") {
    ##if var.pre is not set, use zillow all homes returns
    if (!grepl("ret", .var)) {
      var.pre <- "State_Zhvi_AllHomes_ret"
      predictors <- c("State_Zhvi_AllHomes_ret", .var)
    }
  } else if (frequency == "quarter") {
    ##if var.pre is not set use fhfa
    if (!grepl("ret", .var)) {
      var.pre <- "fhfa_state_ret"
      predictors <- c("fhfa_state_ret", .var)
    }
  }

  ##Make sure that var.pre is available for all regions
  DT <- DT %>%
    .[, N := sum(!is.na(.SD)), by = region,
      .SDcols = names(.) == var.pre] %>%
    .[N == max(N)] %>%
    .[, N := NULL]


  ##The special predictors
  special.predictors <- list(
    list(paste0(var.pre, "_pre"), 2007.00, "mean"),
    list(paste0(var.pre, "_var_pre"), 2007.00, "mean"),
    list(paste0(var.pre, "_1_yr_pre"), 2007.00, "mean"),
    list("median.income.2007",      2007.00, "mean"),
    list("unemp.rate.2007",              2007.00, "mean"),
    list("bartik.2006.2008", 2007.00, "mean"),
    list("land.unavailable", 2007.00, "mean")
  )

  if (frequency == "month") {
    ##add returns for monthly data
    special.predictors <- c(special.predictors,
                            list(
                              list(var.pre, 2008.25, "mean"),
                              list(var.pre, 2008.33, "mean"),
                              list(var.pre, 2008.42, "mean"))
                            )
  } else if (frequency == "quarter") {
    ##add returns for quarterly data
    special.predictors <- c(special.predictors,
                            list(
                              list(var.pre, 2008.25, "mean")
                            ))
  }

  temp.treated <- "CA"
  temp.controls <- DT[state != "CA", unique(state)]
  time.index <- DT[, unique(time.index)]

  temp.synth <- SynthMult$new(
    all.data = DT,
    dependent.vars = .var,
    treated = temp.treated,
    controls = temp.controls,
    time.policy = 2008.497,
    sample = "31-synth_state",
    time.index = time.index,
    predictors = predictors,
    special.predictors = special.predictors,
    y.axis.label = .var,
    sample.average.label = "Sample Average",
    parallel = TRUE,        ##Run in parallel
    parallel.cores = 4,
    fast.compute = TRUE,
    plots = FALSE,
    compute.perm = TRUE,
    optim.method = "BFGS"
  )

  return(invisible(NULL))

}

lapply(vars.state.df$var, f_synth)
