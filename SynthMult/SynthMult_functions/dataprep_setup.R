##dataprep_setup.r

##Function for the R dataprep function
dataprep_setup <- function(treated.id.number,control.units.id.numbers) {
    ## dataprep.out <- dataprep_setup(treated.unit,control.units)
    ##
    ## To get a dataprep object for a SynthClass object and
    ## a supplied treated unit and control units
    ## This will be a private function
    ##
    ## User-specified inputs:
    ##   treated.unit -- string with the treated unit
    ##   control.units -- character vector with the control units
    ##
    ## User-requested output:
    ##   dataprep.out -- a dataprep object to be used with synthetic
    ##                   control optimization

    dataprep.out <- dataprep(
        ##The dataset
        foo = self$all.data,
        ##The regular predictor variables
        predictors = self$predictors,
        ##operator to be used for the predictors
        predictors.op = "mean",
        ##The pretreatment periods (must be numeric vector)
        time.predictors.prior =
            self$time.index[self$time.index < self$time.policy],
        ##Special predictors for variables with a certain
        ##time period
        special.predictors = self$special.predictors,
        ##Numeric dependent variable
        dependent = self$dependent.var,
        ##Column name with the unit numbers (must be numeric)
        unit.variable = "region.number",
        ##Column name with the names of the units
        unit.names.variable = "region",
        ##Column name holding time period
        time.variable = "time.index",
        ##Scalar or character string holding the treated unit
        treatment.identifier = treated.id.number,
        ##Scalars or character strings holding the control units
        ##Use variables for which we have a complete dataset
        ##make sure that the treated unit is not in the
        ##set of controls
        controls.identifier = control.units.id.numbers,
        ##numeric vector containing the periods over which
        ##to optimize
        ##the loss function
        time.optimize.ssr =
            self$time.index[self$time.index < self$time.policy],
        time.plot = self$time.index
        )


    return(dataprep.out)
}
