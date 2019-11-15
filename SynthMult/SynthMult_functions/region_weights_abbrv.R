## c:/Dropbox/SyntheticControl/SynthMult/SynthMult_functions/region_weights_abbrv.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2016-12-13

##A method to get the abbreviated region weights

region_weights_abbrv <- function(region.weights, remove.zeros=TRUE) {
    ## region_weights_abbrv <- region_weights_abbrv(region.weights,remove.zeros=TRUE)
    ##
    ## To return an abbreviated string with the region weights
    ##
    ## User-specified inputs:
    ##   region.weights -- the region weights
    ##   remove.zeros=TRUE -- if set to TRUE, the function will remove
    ##                        units that contribute zero weight to the
    ##                        synthetic control
    ## This will be a private method
    ##
    ## User-requested output:
    ##   region.weights.abbrv -- string of abbreviated weights.


    ##Sort the unit names by their weight
    region.weights <- region.weights[order(region.weights$w.weights,decreasing=TRUE),]
    if (remove.zeros)
        region.weights <- region.weights[region.weights$w.weights >= 0.001,]
    region.weights.abbrv <- paste(region.weights$region,
                                  format(round(region.weights$w.weights,2),nsmall=2),
                                  sep=": ",collapse="; ")
    return(region.weights.abbrv)

}
