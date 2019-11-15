## c:/Dropbox/SyntheticControl/SynthMult/SynthMult_functions/get_Y_Z_DT.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-01-15

get_Y_Z <- function() {

  ##First create a time index that will create that will extend
  ##across all to for all of Y across all increase
  temp.time <- rep(self$time.index, length(self$dependent.vars))

  ##Create a function that's to take the mean before the policy period
  f.std <- function(x) {
    x.mean <- mean(x[self$time.index < self$time.policy])
    x.sd <- sd(x[self$time.index < self$time.policy])
    return((x - x.mean) / x.sd)
  }

  ##Get the Y (outcome data) in its original format
  self$Y.df <- self$all.data %>%
    .[(region %in% c(self$treated, unique(unlist(self$controls)))) &
        (time.index %in% self$time.index)] %>%
    .[order(region.number, time.index)] %>%
    .[, mget(c("time.index", "region.number", self$dependent.vars))]




  ##Only standardize if the number of dependent variables is greater than 1
  if (length(self$dependent.vars > 1)) {
    self$Y <- self$Y.df %>%
      .[, lapply(.SD, f.std), .SDcols = c(self$dependent.vars)]
  }

  ##Clean the y data and return an matrix where the column names are the region numbers
  self$Y <- self$Y.df %>%
    ##Melt using time.index & region.number as id cols
    melt(id.vars = c("time.index", "region.number"),
         measure.vars = self$dependent.vars, variable.factor = FALSE) %>%
    dcast(time.index + variable ~ region.number, value.var = "value") %>%
    .[order(variable, time.index)] %>%
    .[, c("time.index", "variable") := NULL] %>%
    as.matrix(.) %>%
    magrittr::set_rownames(temp.time)

  self$Z <- self$Y[temp.time < self$time.policy,, drop = FALSE]

  ##Add the name of the region and if it was treated to self$Y.df
  self$Y.df <- self$Y.df %>%
    ##merge in the region names
    merge(self$region.numbers, by = "region.number") %>%
    .[, treated := (region %in% self$treated)] %>%
    ##tidy the dataframe (melt)
    melt(id.vars = c("region", "region.number", "treated", "time.index"),
         measure.vars = self$dependent.vars, variable.factor = FALSE)

  return(NULL)
}

