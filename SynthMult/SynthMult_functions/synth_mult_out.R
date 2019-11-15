## c:/Dropbox/SyntheticControl/SynthMult/SynthMult_functions/synth_mult_out_DT.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-01-15

## A function to get the gap for the multiple synthetic control

synth_mult_out <- function(treated.regions, region.weights) {
  ## list(path, gap) <- synth_mult_out(treated, region.weights)
  ##
  ## A function to get the path and gap Synthetic Control output for
  ## given the treated units, controls, and region weights
  ##
  ## User-specified inputs
  ##   treated.regions -- (character vector) vector of treated units
  ##   region.weights -- (list) list of synthetic control region
  ##                     weights where the names of the elements
  ##                     are the same as treated

  if (is.null(self$Y.df)) {
    ##No depedendent variable(s) specified by the user,
    ##return an invisible null
    return(invisible(NULL))
  }

  if (!is.list(region.weights)){
    ##convert to list and print a warning
    region.weights <- list(region.weights)
  }

  f_mult <- function(x, w) {
    if (is.numeric(x)) {
      return(x * w)
    } else {
      return(x)
    }
  }
  ##get Y0 for each treated unit based on the region weights --
  ##this is the value of Y for all units that weren't treated
  Y0.out <- lapply(region.weights, function(temp.weights) {
    Y0.temp <- copy(self$Y.df) %>%
      ##Get just the control units based on the regions in temp
      .[region %in% c(temp.weights$region), ] %>%
      .[, treated := NULL] %>%
      ##setkey("region") %>%
      ##Merge with the temporary region weights
      merge(., temp.weights, by = c("region")) %>%
      ##remove unneeded variables
      .[, c("region", "region.number") := NULL] %>%
      ##Multiply all variables by the weights by variable, time.index
      .[, lapply(.SD, f_mult, w.weights), by = .(variable, time.index)] %>%
      ##remove the weights
      .[, w.weights := NULL] %>%
      ##Sum for each time index to get Y0
      .[, lapply(.SD, sum, na.rm = TRUE), by = .(variable, time.index)] %>%
      setnames("value", "control")
  }) %>%
    setNames(., treated.regions) %>%
    rbindlist(., idcol = "region")

  ##Values for the the treated units
  Y1.out <- self$Y.df %>%
    .[(region %in% c(treated.regions))] %>%
    .[, treated := NULL]

  ##The sample average
  Y.mean <- self$sample.average.df %>%
    melt(id.vars = "time.index", variable.name = "variable",
         value.name = "sample.average", variable.factor = FALSE)

  ##Merge all of the data
  synth.out <- merge(Y1.out, Y.mean, by = c("variable", "time.index")) %>%
    merge(Y0.out, by = c("region", "variable", "time.index"), all = FALSE) %>%
    .[, gap := value - control] %>%
    setcolorder(c("region", "region.number", "time.index", "variable",
                  "sample.average", "value", "control", "gap")) %>%
    .[order(region.number, time.index)]

  return(synth.out)
}
