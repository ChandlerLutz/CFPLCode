## c:/Dropbox/SyntheticControl/SynthMult/SynthMult_functions/dataprep_setup_fast_DT.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2018-01-12

##A fast version of the dataprep_setup function using data.table

dataprep_setup_fast <- function() {
  ##Just the regular predictors first
  X.predictors.names <- sort(self$predictors)

  X.predictors <- self$all.data %>%
    ##Get just the regions in treated and control units
    ##Get just the pre-treatment data
    ##Select just the variables that we want
    .[region %in% c(self$treated, unique(unlist(self$controls))) &
        time.index %in% self$time.index[self$time.index < self$time.policy],
      mget(c("region.number", self$predictors))] %>%
    ##take mean of each by group
    .[, lapply(.SD, mean, na.rm = TRUE), keyby = region.number] %>%
    ##Transform so region number is the column
    melt(id.vars = "region.number", variable.factor = FALSE) %>%
    dcast(variable ~ region.number, value.var = "value") %>%
    ##order based on the variable
    .[, variable := as.character(variable)] %>%
    ##Order the variables using the lower case of the variable
    .[order(tolower(variable))] %>%
    .[, variable := NULL] %>%
    ##Convert to data frame and set the rownames
    as.data.frame %>%
    set_rownames(X.predictors.names)


  ##-- special predictors -- ##
  if (!is.null(self$special.predictors)) {
    X.special.predictors <- as.data.frame(matrix(NA,
                                                 nrow=length(self$special.predictors),
                                                 ncol=nrow(self$region.numbers)
                                                 ))
    names(X.special.predictors) <- self$region.numbers$region.number

    ##The special predictor names
    special.predictors.names <- lapply(
      self$special.predictors,
      function(x) paste0(x[[1]],".",x[[2]][1])
    )
    special.predictors.names <- do.call("c",special.predictors.names)

    ##Now get the special predictors and use the time
    ##periods supplied by the user
    for (i in 1:length(self$special.predictors)) {

      temp.time.index <- self$special.predictors[[i]][[2]]
      temp.variable <- self$special.predictors[[i]][[1]]

      X.special.predictors[i,] <- self$all.data %>%
        ##Just the regions that we want
        .[(region %in% c(self$treated, unique(unlist(self$controls))))] %>%
        ##just the dates where the special predictors
        ##should be calculated
        .[(time.index %in% temp.time.index)] %>%
        ##Just the region number and the special predictor
        .[, mget(c("region.number", temp.variable))] %>%
        .[, lapply(.SD, mean, na.rm = TRUE), keyby = region.number]  %>%
        .[, get(temp.variable)]
    }
    ##remove the temporary variables
    rm(temp.time.index, temp.variable)
    ##the names for the speicial predictors
    rownames(X.special.predictors) <- special.predictors.names

    ##Bind with
    self$X <- rbind(X.predictors,X.special.predictors) %>%
      as.data.frame(.,stringsAsFactors=FALSE) %>% as.matrix(.) %>%
      set_rownames(c(X.predictors.names,special.predictors.names))

  } else {
    ##No special predictors
    ##Just use the normal predictors
    self$X <- X.predictors %>%
      as.data.frame(.,stringsAsFactors=FALSE) %>% as.matrix(.) %>%
      set_rownames(X.predictors.names)
  }

  return(NULL)

}
